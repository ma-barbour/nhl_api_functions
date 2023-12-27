# SETUP ########################################################################

#install.packages("tidyverse")
#install.packages("jsonlite")
#install.packages("lubridate")
#install.packages("parsedate")

library(tidyverse)
library(jsonlite)
library(parsedate)

# FUNCTION: GET PLAY-BY-PLAY DATA ##############################################

# This function pulls (and adds to) the play-by-play data for a single game_id
# This function DOES NOT pull player shift data
# Note: the data provided by the NHL can be incorrect / incomplete
# Use my get_schedule() function to find a game_id
# Wrap this function in a loop to pull data for multiple games at the same time (see EXAMPLE below)

get_play_by_play_data <- function(game_id) {
        
        #game_id <- "2022020555"
        
        pbp_site <- read_json(paste0("https://api-web.nhle.com/v1/gamecenter/", game_id, "/play-by-play"))
        
        # Basic game data
        
        season <- pbp_site[["season"]]
        date <- as.Date(pbp_site[["gameDate"]])
        
        # Team data
        
        away_team <- pbp_site[["awayTeam"]][["abbrev"]]
        away_team_id <- pbp_site[["awayTeam"]][["id"]]
        home_team <- pbp_site[["homeTeam"]][["abbrev"]]
        home_team_id <- pbp_site[["homeTeam"]][["id"]]
        
        # Roster data
        
        players <- pbp_site[["rosterSpots"]] %>%
                tibble() %>%
                unnest_wider(1) %>%
                unnest_wider(firstName, names_sep = "_") %>%
                unnest_wider(lastName, names_sep = "_") %>%
                mutate(player = paste(firstName_default, lastName_default)) %>%
                select(player_id = playerId,
                       player,
                       position = positionCode,
                       team_id = teamId) %>%
                mutate(team = if_else(team_id == away_team_id, away_team, home_team))

        goalie_ids <- players %>%
                filter(position == "G")
        goalie_ids <- goalie_ids$player_id
        
        # Play-by-play data
        
        pbp_data <- pbp_site[["plays"]] %>%
                tibble() %>%
                unnest_wider(1) %>%
                unnest_wider(periodDescriptor) %>%
                unnest_wider(details, names_sep = "_")
        
        # Add game details to PBP data
        
        pbp_data <- pbp_data %>%
                mutate(game_id = game_id,
                       season = season,
                       date = date,
                       away_team = away_team,
                       away_team_id = away_team_id,
                       home_team = home_team,
                       home_team_id = home_team_id)
        
        # Convert period time to seconds and add game time
        
        pbp_data$timeInPeriod <- ms(pbp_data$timeInPeriod)
        pbp_data$timeInPeriod <- period_to_seconds(pbp_data$timeInPeriod)
        pbp_data$game_time_s <- ((pbp_data$period - 1) * 1200) + pbp_data$timeInPeriod
        pbp_data$game_time_s <- ifelse(pbp_data$game_time_s == 4800, NA, pbp_data$game_time_s)
        
        # Add logical for when event team = home team
        
        pbp_data <- pbp_data %>%
                mutate(event_team_home = if_else(
                        details_eventOwnerTeamId == home_team_id, TRUE, FALSE))
        
        # Add fixed coordinates
        # Establish o-zone by period
        
        pbp_data <- pbp_data %>%
                group_by(details_eventOwnerTeamId, period) %>%
                mutate(median_o_zone = median(details_xCoord[details_zoneCode == "O"], na.rm = TRUE)) %>%
                ungroup()
        
        # The home team is always shooting to the "right"
        
        pbp_data <- pbp_data %>%
                mutate(x_fixed = case_when(
                        details_eventOwnerTeamId == home_team_id & median_o_zone > 0 ~ details_xCoord,
                        details_eventOwnerTeamId == home_team_id & median_o_zone < 0 ~ 0 - details_xCoord,
                        details_eventOwnerTeamId == away_team_id & median_o_zone > 0 ~ 0 - details_xCoord,
                        details_eventOwnerTeamId == away_team_id & median_o_zone < 0 ~ details_xCoord)) %>%
                mutate(y_fixed = case_when(
                        details_eventOwnerTeamId == home_team_id & median_o_zone > 0 ~ details_yCoord,
                        details_eventOwnerTeamId == home_team_id & median_o_zone < 0 ~ 0 - details_yCoord,
                        details_eventOwnerTeamId == away_team_id & median_o_zone > 0 ~ 0 - details_yCoord,
                        details_eventOwnerTeamId == away_team_id & median_o_zone < 0 ~ details_yCoord))
        
        # Add shot attempt distance from middle of the net (Euclidean distance formula)
        
        pbp_data <- pbp_data %>%
                mutate(sa_distance = case_when(
                        details_eventOwnerTeamId == home_team_id & typeDescKey %in% c("shot-on-goal", "missed-shot", "goal") ~ round(abs(sqrt((x_fixed - 89)^2 + (y_fixed)^2)), 1),
                        details_eventOwnerTeamId == away_team_id & typeDescKey %in% c("shot-on-goal", "missed-shot", "goal") ~ round(abs(sqrt((x_fixed - (-89))^2 + (y_fixed)^2)), 1)))
        
        # Add shot attempt angle from middle of the net
        
        pbp_data <- pbp_data %>%
                mutate(sa_angle = case_when(
                        details_eventOwnerTeamId == home_team_id & typeDescKey %in% c("shot-on-goal", "missed-shot", "goal") ~ round(abs(atan((0-y_fixed) / (89-x_fixed)) * (180 / pi)), 1),
                        details_eventOwnerTeamId == away_team_id & typeDescKey %in% c("shot-on-goal", "missed-shot", "goal") ~ round(abs(atan((0-y_fixed) / (-89-x_fixed)) * (180 / pi)), 1))) %>%
                mutate(sa_angle = ifelse((details_eventOwnerTeamId == home_team_id & x_fixed > 89) | (details_eventOwnerTeamId == away_team_id & x_fixed < -89), 180 - sa_angle, sa_angle))
        
        # Extend game state data
        # Players on-ice
        
        pbp_data <- pbp_data %>%
                mutate(away_goalies_oi = as.numeric(substr(pbp_data$situationCode, 1, 1)),
                       away_skaters_oi = as.numeric(substr(pbp_data$situationCode, 2, 2)),
                       home_skaters_oi = as.numeric(substr(pbp_data$situationCode, 3, 3)),
                       home_goalies_oi = as.numeric(substr(pbp_data$situationCode, 4, 4))) %>%
                mutate(away_pp = case_when(
                        away_goalies_oi == 1 & away_skaters_oi > home_skaters_oi ~ TRUE,
                        away_goalies_oi == 0 & away_skaters_oi > (home_skaters_oi + 1) ~ TRUE,
                        TRUE ~ FALSE)) %>%
                mutate(home_pp = case_when(
                        home_goalies_oi == 1 & home_skaters_oi > away_skaters_oi ~ TRUE,
                        home_goalies_oi == 0 & home_skaters_oi > (away_skaters_oi + 1) ~ TRUE,
                        TRUE ~ FALSE))
        
        # Event on power play or short handed
        
        pbp_data <- pbp_data %>%
                mutate(event_pp = case_when(
                        details_eventOwnerTeamId == home_team_id & home_pp == TRUE ~ TRUE,
                        details_eventOwnerTeamId == away_team_id & away_pp == TRUE ~ TRUE,
                        TRUE ~ FALSE)) %>%
                mutate(event_sh = case_when(
                        details_eventOwnerTeamId == home_team_id & away_pp == TRUE ~ TRUE,
                        details_eventOwnerTeamId == away_team_id & home_pp == TRUE ~ TRUE,
                        TRUE ~ FALSE))
        
        # Events with empty net
        
        pbp_data <- pbp_data %>%
                mutate(event_on_en = case_when(
                        details_eventOwnerTeamId == home_team_id & away_goalies_oi == 0 ~ TRUE,
                        details_eventOwnerTeamId == away_team_id & home_goalies_oi == 0 ~ TRUE,
                        TRUE ~ FALSE)) %>%
                mutate(event_w_en = case_when(
                        details_eventOwnerTeamId == home_team_id & home_goalies_oi == 0 ~ TRUE,
                        details_eventOwnerTeamId == away_team_id & away_goalies_oi == 0 ~ TRUE,
                        TRUE ~ FALSE))
        
        # Add dangerous shot attempts (unblocked shot attempt in good location)
        
        pbp_data <- pbp_data %>%
                mutate(sa_dangerous = case_when(
                        sa_distance < 30 & sa_angle < 55 ~ TRUE,
                        sa_distance < 25 & sa_angle < 58 ~ TRUE,
                        sa_distance < 20 & sa_angle < 62 ~ TRUE,
                        sa_distance < 15 & sa_angle < 67 ~ TRUE,
                        sa_distance < 10 & sa_angle < 73 ~ TRUE,
                        sa_distance < 5 & sa_angle < 80 ~ TRUE,
                        sa_distance < 3 ~ TRUE,
                        TRUE ~ FALSE))
        
        # Select the target data
        
        pbp_data <- pbp_data %>%
                select(any_of(c("game_id",
                                "season",
                                "date",
                                "away_team",
                                "away_team_id",
                                "home_team",
                                "home_team_id",
                                "sortOrder",
                                "period",
                                "periodType",
                                "timeInPeriod",
                                "game_time_s",
                                "away_goalies_oi",
                                "away_skaters_oi",
                                "home_skaters_oi",
                                "home_goalies_oi",
                                "away_pp",
                                "home_pp",
                                "event_pp",
                                "event_sh",
                                "event_on_en",
                                "event_w_en",
                                "details_eventOwnerTeamId",
                                "event_team_home",
                                "typeDescKey",
                                "details_xCoord",
                                "details_yCoord",
                                "x_fixed",
                                "y_fixed",
                                "details_zoneCode",
                                "details_shootingPlayerId",
                                "details_goalieInNetId",
                                "details_shotType",
                                "sa_distance",
                                "sa_angle",
                                "sa_dangerous",
                                "details_scoringPlayerId",
                                "details_assist1PlayerId",
                                "details_assist2PlayerId",
                                "details_hittingPlayerId",
                                "details_hitteePlayerId",
                                "details_blockingPlayerId",
                                "details_committedByPlayerId",
                                "details_drawnByPlayerId",
                                "details_descKey",
                                "details_typeCode",
                                "details_duration",
                                "details_winningPlayerId",
                                "details_losingPlayerId",
                                "details_playerId",
                                "details_reason")))
        
        # Rename some of the columns
        
        col_names <- c(event_sort = "sortOrder",
                       period_type = "periodType",
                       period_time_s = "timeInPeriod",
                       event_team_id = "details_eventOwnerTeamId",
                       event_type = "typeDescKey",
                       event_x = "details_xCoord",
                       event_y = "details_yCoord",
                       zone = "details_zoneCode",
                       shooter_id = "details_shootingPlayerId",
                       shot_type = "details_shotType",
                       scorer_id = "details_scoringPlayerId",
                       assist_1_id = "details_assist1PlayerId",
                       assist_2_id = "details_assist2PlayerId",
                       hitter_id = "details_hittingPlayerId",
                       hittee_id = "details_hitteePlayerId",
                       blocker_id = "details_blockingPlayerId",
                       penalized_id = "details_committedByPlayerId",
                       penalty_draw_id = "details_drawnByPlayerId",
                       penalty_description = "details_descKey",
                       penalty_type = "details_typeCode",
                       penalty_duration = "details_duration",
                       fo_winner_id = "details_winningPlayerId",
                       fo_loser_id = "details_losingPlayerId",
                       taway_gaway_id = "details_playerId",
                       details = "details_reason",
                       goalie_id = "details_goalieInNetId")
        
        pbp_data <- rename(pbp_data, any_of(col_names))
        
        # Move scorer_id to shooter_id
        
        pbp_data <- pbp_data %>%
                mutate(shooter_id = if_else(event_type == "goal", scorer_id, shooter_id)) %>%
                select(-scorer_id)
        
        return(pbp_data)
        
}

# EXAMPLE ######################################################################

# Pull play-by-play data for game_id 2022020555

sample_pbp_data <- get_play_by_play_data(2022020555)

# Wrap this function in a loop to pull play-by-play data for multiple games
# This example returns the play-by-play data for two Oilers vs Flames games
# Create a vector of game_ids and then run them through the loop 

game_ids <- c(2022020555, 2022020137)

temp_pbp_list <- list()

for (i in 1:length(game_ids)) {
        
        pbp_data <- get_play_by_play_data(game_ids[i])
        
        temp_pbp_list[[i]] <- pbp_data
}

multi_game_pbp <- bind_rows(temp_pbp_list) %>%
        arrange(date, 
                game_id, 
                event_sort)

        
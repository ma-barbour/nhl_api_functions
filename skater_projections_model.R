# INTRODUCTION #################################################################

# This script generates skater projections for the 2023-2024 NHL season
# The projected stats are goals, assists, shots, hits, and blocks
# These projections should be updated if/when skaters change teams

# I'm always "improving" my skater projections
# Areas for further improvement include the following:
# (1) lots of room for improving the SHOTS/HITS/BLOCKS projections;
# (2) add a player development model to predict skater progress; and
# (3) more work on how a changing "environment" affects scoring.

# SETUP ########################################################################

#install.packages("tidyverse")
#install.packages("jsonlite")
#install.packages("lubridate")
#install.packages("parsedate")
#install.packages("ClusterR")
#install.packages("stringr")
#install.packages("AICcmodavg")
#install.packages("caTools")
#install.packages("xgboost")
#install.packages("pROC")
#install.packages("ROCR")

library(tidyverse)
library(jsonlite)
library(lubridate)
library(parsedate)
library(ClusterR)
library(stringr)
library(AICcmodavg)
library(caTools)
library(xgboost)
library(pROC)
library(ROCR)

# NHL API FUNCTIONS ############################################################

##### NHL API FUNCTIONS: GET TEAM ROSTERS

get_team_rosters <- function() {
        
        # Create a vector of team_ids
        
        teams_site <- read_json("https://statsapi.web.nhl.com/api/v1/teams")
        
        teams <- teams_site$teams %>% 
                tibble() %>% 
                unnest_wider(1) %>% 
                select(team_id = id, 
                       full_team_name = name, 
                       team_abbr = abbreviation)
        
        team_ids <- teams$team_id
        
        # Loop through each team's API endpoint and collect expanded roster data
        
        temp_list <- list()
        
        for (i in team_ids) {
                
                roster_url <- paste0("https://statsapi.web.nhl.com/api/v1/teams/", i, "/roster?expand=roster.person")
                
                roster_site <- read_json(roster_url)
                
                roster_data <- roster_site$roster %>%
                        tibble() %>%
                        unnest_wider(1) %>%
                        unnest_wider(1) %>%
                        unnest_wider(currentTeam, names_sep = "_") %>%
                        unnest_wider(primaryPosition, names_sep = "_") %>%
                        unnest_wider(position, names_sep = "_")
                
                roster_data <- select(roster_data, 
                                      player_id = id,
                                      player = fullName,
                                      team_id = currentTeam_id,
                                      team = currentTeam_name,
                                      position = position_abbreviation,
                                      age = currentAge,
                                      dob = birthDate,
                                      nationality,
                                      height,
                                      weight,
                                      shoots_catches = shootsCatches,
                                      number = jerseyNumber
                )
                
                roster_data$dob <- as_date(roster_data$dob)
                
                temp_list[[i]] <- roster_data
        }
        
        rosters <- bind_rows(temp_list) 
        
        return(rosters)
}

##### NHL API FUNCTIONS: GET SCHEDULE

get_schedule <- function(start_date, end_date, reg_szn = "TRUE") {
        
        # Generate the appropriate URL based on reg_szn TRUE/FALSE
        
        sched_url <- ifelse(reg_szn == "FALSE", 
                            paste0("https://statsapi.web.nhl.com/api/v1/schedule?startDate=", start_date, "&endDate=",  end_date), 
                            paste0("https://statsapi.web.nhl.com/api/v1/schedule?startDate=", start_date, "&endDate=",  end_date, "&gameType=R")) 
        
        # Pull and clean schedule data for the specified time period
        
        sched_site <- read_json(sched_url)
        
        sched_dates <- sched_site$dates %>%
                tibble() %>%
                unnest_wider(1) %>%
                unnest_longer(games) %>%
                unnest_wider(games) %>%
                unnest_wider(teams) %>%
                unnest_wider(away) %>%
                select(-link) %>%
                unnest_wider(team) %>%
                rename(away_team_id = id) %>%
                rename(away_team = name) %>%
                select(-link, -leagueRecord, -score) %>%
                unnest_wider(home) %>%
                unnest_wider(team) %>%
                rename(home_team_id = id) %>%
                rename(home_team = name) %>% 
                select(game_id = gamePk,
                       season,
                       game_type = gameType,
                       date,
                       home_team_id,
                       home_team,
                       away_team_id,
                       away_team)
        
        sched_dates$date <- as.Date(sched_dates$date) 
        
        return(sched_dates)
}

##### NHL API FUNCTIONS: GET PLAY-BY-PLAY DATA

get_play_by_play_data <- function(game_id) {
        
        pbp_site <- read_json(paste0("https://statsapi.web.nhl.com/api/v1/game/", game_id, "/feed/live"))
        
        # Get basic game data 
        
        game_data <- pbp_site$gameData %>%
                tibble() %>%
                unnest_wider(1) %>%
                select(game_id = pk, 
                       season, 
                       season_type = type) %>%
                filter(game_id > 0)
        
        game_date <- pbp_site$gameData$datetime[1]
        game_date <- parse_date(game_date) %>%
                as.POSIXct("EST") %>%
                as_date()
        
        game_data$date <- game_date
        
        # Get basic team data
        
        team_data <- pbp_site$gameData %>%
                tibble()
        team_data <- team_data[4,] %>%
                unnest_wider(1) %>%
                unnest_wider(away) %>%
                select(away_team_id = id,
                       away_team = name,
                       home) %>%
                unnest_wider(home) %>%
                select(home_team_id = id,
                       home_team = name,
                       away_team_id,
                       away_team)
        
        home_team <- team_data$home_team
        away_team <- team_data$away_team
        
        # Get basic player data and isolate goalie_ids
        
        player_data <- pbp_site$gameData %>%
                tibble()
        player_data <- player_data[5,] %>%
                unnest_longer(1) %>%
                unnest_wider(1) %>%
                unnest_wider(primaryPosition) %>%
                select(player_id = id,
                       player = fullName,
                       position = type,
                       dob = birthDate)
        
        player_data$dob <- as_date(player_data$dob)
        
        goalies <- filter(player_data, position == "Goalie") %>%
                select(goalie_id = player_id,
                       goalie = player)
        goalie_ids <- goalies$goalie_id
        
        skaters <- filter(player_data, position != "Goalie") %>%
                mutate(position = ifelse(position == "Forward",
                                         "F",
                                         "D"))
        
        forwards <- filter(skaters, position == "F")
        forwards_ids <- forwards$player_id
        
        defensemen <- filter(skaters, position == "D")
        defensemen_ids <- defensemen$player_id
        
        # Pull play-by-play data (events and coordinates)
        # Event player data is omitted here and is dealt with below
        
        all_plays <- pbp_site$liveData$plays$allPlays %>%
                tibble() %>%
                unnest_wider(1) %>%
                unnest_wider(result) %>%
                unnest_wider(strength, names_sep = "_") %>%
                unnest_wider(about) %>%
                unnest_wider(goals) %>%
                unnest_wider(coordinates) %>%
                unnest_wider(team, names_sep = "_")
        
        # Add game details
        
        all_plays$game_id <- game_data$game_id
        all_plays$season <- game_data$season
        all_plays$season_type <- game_data$season_type
        all_plays$date <- game_data$date
        
        # Add team details
        
        all_plays$home_team_id <- team_data$home_team_id
        all_plays$home_team <- team_data$home_team
        all_plays$away_team_id <- team_data$away_team_id
        all_plays$away_team <- team_data$away_team
        
        # Change time data to seconds and add game clock
        # Set game clock to NA for regular season shootouts
        
        all_plays$periodTime <- ms(all_plays$periodTime)
        all_plays$periodTime <- period_to_seconds(all_plays$periodTime)
        all_plays$game_seconds <- ((all_plays$period - 1) * 1200) + all_plays$periodTime
        all_plays$game_seconds <- ifelse(all_plays$game_seconds == 4800 & all_plays$season_type == "R", NA, all_plays$game_seconds)
        
        # Select desired data
        
        all_plays <- select(all_plays, any_of(c("game_id", 
                                                "season", 
                                                "season_type",
                                                "date",
                                                "home_team_id",
                                                "home_team",
                                                "away_team_id",
                                                "away_team",
                                                "eventIdx", 
                                                "period",
                                                "periodTime",
                                                "game_seconds", 
                                                "team_id", 
                                                "team_name", 
                                                "eventTypeId", 
                                                "description", 
                                                "secondaryType",
                                                "penaltyMinutes",
                                                "strength_code", 
                                                "emptyNet",
                                                "home",
                                                "away",
                                                "x", 
                                                "y")))
        
        
        # Rename some of the columns
        
        col_names <- c(event_id = "eventIdx", 
                       event_team_id = "team_id", 
                       event_team = "team_name", 
                       event_type = "eventTypeId", 
                       secondary_type = "secondaryType", 
                       empty_net = "emptyNet", 
                       period_time = "periodTime", 
                       pim = "penaltyMinutes", 
                       home_goals = "home", 
                       away_goals = "away")
        
        all_plays <- rename(all_plays, any_of(col_names))
        
        # Find median x-coordinate for team shot attempts by period
        # This is used to determine if the team is shooting to the "left" or "right"
        # Credit to Dan Morse (hockeyR) for the original code used to add fixed coordinates, shot distance, and shot angle (original code has been modified here)
        
        shot_attempts <- c("MISSED_SHOT","SHOT","GOAL")
        
        all_plays <- all_plays %>%
                group_by(event_team, period) %>%
                mutate(median_sa = median(x[event_type %in% shot_attempts], na.rm = TRUE)) %>%
                ungroup()
        
        # Add fixed coordinates using median x-coordinate
        # The home team is always shooting to the "right"
        
        all_plays <- mutate(all_plays, x_fixed = case_when(
                event_team == home_team & median_sa > 0 ~ x,
                event_team == home_team & median_sa < 0 ~ 0 - x,
                event_team == away_team & median_sa > 0 ~ 0 - x,
                event_team == away_team & median_sa < 0 ~ x)) %>%
                mutate(y_fixed = case_when(
                        event_team == home_team & median_sa > 0 ~ y,
                        event_team == home_team & median_sa < 0 ~ 0 - y,
                        event_team == away_team & median_sa > 0 ~ 0 - y,
                        event_team == away_team & median_sa < 0 ~ y))
        
        # Add shot attempt distance from middle of the net (Euclidean distance formula)
        
        all_plays <- mutate(all_plays, sa_distance = case_when(
                event_team == home_team & event_type %in% shot_attempts ~ round(abs(sqrt((x_fixed - 89)^2 + (y_fixed)^2)), 1),
                event_team == away_team & event_type %in% shot_attempts ~ round(abs(sqrt((x_fixed - (-89))^2 + (y_fixed)^2)), 1)))
        
        # Add shot attempt angle from middle of the net
        
        all_plays <- mutate(all_plays, sa_angle = case_when(
                event_team == home_team & event_type %in% shot_attempts ~ round(abs(atan((0-y_fixed) / (89-x_fixed)) * (180 / pi)), 1),
                event_team == away_team & event_type %in% shot_attempts ~ round(abs(atan((0-y_fixed) / (-89-x_fixed)) * (180 / pi)), 1))) %>%
                mutate(sa_angle = ifelse((event_team == home_team & x_fixed > 89) | (event_team == away_team & x_fixed < -89), 180 - sa_angle, sa_angle))
        
        # Add dangerous shot attempts (goals, shots, and missed shots)
        # A shot attempt is dangerous if distance <= 35 and angle <= 50
        # OR if distance <= 20 and angle <= 60
        # OR if distance <= 10 and was not taken behind the goal line
        # This captures more than 3/4 of goals scored against a goalie
        
        all_plays$sa_dangerous <- ifelse(all_plays$sa_distance <= 35 & all_plays$sa_angle <= 50, TRUE, FALSE)
        all_plays$sa_dangerous <- ifelse(all_plays$sa_distance <= 20 & all_plays$sa_angle <= 60, TRUE, all_plays$sa_dangerous)
        all_plays$sa_dangerous <- ifelse(all_plays$sa_distance <= 10 & all_plays$x < 0 & all_plays$x >= -89 , TRUE, all_plays$sa_dangerous)
        all_plays$sa_dangerous <- ifelse(all_plays$sa_distance <= 10 & all_plays$x > 0 & all_plays$x <= 89 , TRUE, all_plays$sa_dangerous)
        
        # Remove median shot attempt
        
        all_plays <- select(all_plays, -median_sa)
        
        # Unpack the event players for the play-by-play data using a loop
        
        players_raw <- pbp_site$liveData$plays$allPlays %>%
                tibble() %>%
                unnest_wider(1) %>%
                select(players) %>%
                unnest_wider(players, names_sep = "_")
        
        temp_players_list <- list()
        
        for(i in 1:length(players_raw)) {
                
                temp_player_data <- players_raw[,i] %>%
                        unnest_wider(1) %>%
                        unnest_wider(player, names_sep = "_")
                names(temp_player_data)[1] <- paste0("event_player_", i, "_id")
                names(temp_player_data)[2] <- paste0("event_player_", i)
                names(temp_player_data)[4] <- paste0("event_player_", i, "_type")
                temp_player_data <- select(temp_player_data, c(1,2,4))
                
                temp_players_list[[i]] <- temp_player_data
        }
        
        players <- bind_cols(temp_players_list)
        
        # Find the max number of event players (excluding event_player_1)
        
        event_player_id_cols <- select(players, any_of(c(
                "event_player_2_id",
                "event_player_3_id",
                "event_player_4_id")))
        
        length_cols <- length(event_player_id_cols)
        
        # Loop through the event players looking for goalies (excluding goalies who get an assist) and capture the goalie_id
        
        players$goalie_id <- NA
        
        for (i in 1:length_cols) {
                
                players <- mutate(players, goalie_id = 
                                          ifelse(get(paste0("event_player_", i+1, "_id")) %in% goalie_ids & get(paste0("event_player_", i+1, "_type")) != "Assist", 
                                                 get((paste0("event_player_", i+1, "_id"))), 
                                                 goalie_id))
        }
        
        # Add the names of the goalies
        
        players <- left_join(players, goalies, by = "goalie_id")
        
        # Add skater position data for event_player_1
        
        players <- mutate(players, event_player_1_position = case_when(
                event_player_1_id %in% forwards_ids ~ "F",
                event_player_1_id %in% defensemen_ids ~ "D",
                event_player_1_id %in% goalie_ids ~ "G",
                TRUE ~ NA))
        
        # Add date-of-birth data for event_player_1
        
        dob_data <- select(player_data,
                           event_player_1_id = player_id,
                           event_player_1_dob = dob)
        
        players <- players %>%
                left_join(dob_data, by = "event_player_1_id")
        
        # Rearrange columns
        
        players <- select(players, c(1:3, 
                                     length(players) -1, 
                                     length(players), 
                                     4:(length(players) -2)))
        
        # Add the event players to the play-by-play data
        
        pbp_data <- bind_cols(all_plays, players) %>%
                arrange(game_seconds)
        
        # Add a warning for long distance goals scored against a goalie
        # Potential x-coordinate error in the NHL data
        # Errors happen. If you pull all the data for the 2021-2022 and 2022-2023 seasons you will find 18 instances where this check returns a "TRUE"
        
        pbp_data <- mutate(pbp_data, goal_x_error = ifelse(
                event_type == "GOAL" & 
                        sa_distance > 89 & 
                        goalie_id > 0, 
                TRUE, 
                FALSE))
        
        # Add a similar warning for shots
        
        pbp_data <- mutate(pbp_data, shot_x_error = ifelse(
                (event_type == "SHOT" | event_type == "MISSED_SHOT") & 
                        sa_distance > 89 & 
                        (secondary_type == "Tip-In" |
                                 secondary_type == "Wrap-around" |
                                 secondary_type == "Deflected" |
                                 secondary_type == "Poke" |
                                 secondary_type == "Batted" |
                                 secondary_type == "Between Legs" |
                                 secondary_type == "Cradle"),
                TRUE, 
                FALSE))
        
        # Attach the shifts data (requires get_shift_data() function)
        
        on_ice_data <- get_shift_data(game_id)
        
        pbp_data <- pbp_data %>%
                left_join(on_ice_data, by = c("game_id", "game_seconds"))
        
        return(pbp_data)
}

##### NHL API FUNCTIONS: GET GAME SHIFTS DATA

get_shift_data <- function(game_id) {
        
        # Pull and unpack the shifts charts
        
        pbp_site <- read_json(paste0("https://api.nhle.com/stats/rest/en/shiftcharts?cayenneExp=gameId=", game_id))
        
        data <- pbp_site$data %>%
                tibble() %>%
                unnest_wider(1) %>%
                arrange(period, startTime)
        
        # Change times from strings to seconds
        
        data$endTime <- ms(data$endTime)
        data$endTime <- period_to_seconds(data$endTime)
        
        data$startTime <- ms(data$startTime)
        data$startTime <- period_to_seconds(data$startTime)
        
        data$duration <- ms(data$duration, quiet = TRUE)
        data$duration <- period_to_seconds(data$duration)
        
        # Add shift_start in game seconds (add 1 second to start time)
        
        data <- mutate(data, shift_start = case_when(
                period == 1 ~ startTime,
                period == 2 ~ 1200 + startTime,
                period == 3 ~ 2400 + startTime,
                period == 4 ~ 3600 + startTime,
                period == 5 ~ NA))
        
        data$shift_start <- data$shift_start +1
        
        # Add shift_end in game seconds
        
        data <- mutate(data, shift_end = case_when(
                period == 1 ~ endTime,
                period == 2 ~ 1200 + endTime,
                period == 3 ~ 2400 + endTime,
                period == 4 ~ 3600 + endTime,
                period == 5 ~ 3900))
        
        data <- filter(data, shift_end > 0)
        
        # Create full game clock
        
        end_of_game <- data$shift_end[length(data$shift_end)]
        
        full_game_clock <- as_tibble(seq(1:as.numeric(end_of_game))) %>%
                rename(game_seconds = value)
        
        full_game_clock$game_id <- unique(data$gameId)
        
        full_game_clock <- select(full_game_clock, c(2,1))
        
        # Shrink data
        
        data <- select(data, player_id = playerId, 
                       shift_start,
                       shift_end,
                       duration,
                       team_id = teamId) %>%
                mutate(game_seconds = shift_start)
        
        # Remove duplicate and erroneous data 
        # This will not perfectly "fix" the data in all cases
        
        data <- unique(data)
        
        data <- filter(data, player_id > 0)
        
        other_duplicates <- data %>%
                select(game_seconds, player_id) %>%
                duplicated()
        
        remove_rows <- which(other_duplicates == TRUE)
        
        rows_index <- seq_along(1:length(data$player_id))
        rows_index_keep <- setdiff(rows_index, remove_rows)
        
        data <- filter(data, row_number() %in% rows_index_keep)
        
        # Get on-ice data (the time that a shift ends is inserted at the time that the shift starts - this will be followed by a "fill" action below)
        
        on_ice_data <- pivot_wider(data = data, 
                                   names_from = player_id,
                                   values_from = shift_end, 
                                   id_cols = c(game_seconds),
                                   names_prefix = "player_id_")
        
        # Join to full game clock
        
        full_game_data <- full_game_clock %>%
                left_join(on_ice_data, by = "game_seconds")
        
        # Fill columns with shift_end times (direction is down)
        
        full_game_data <- full_game_data %>%
                fill(c(colnames(full_game_data)))
        
        # Loop through the players to set on-ice as 1/0
        
        for (i in (3:length(full_game_data))) {
                
                full_game_data[i]  = ifelse(
                        full_game_data[i] >= full_game_data$game_seconds,
                        1,
                        0)
        }
        
        full_game_data[is.na(full_game_data)] <- 0
        
        # Add total players on-ice (includes goalies)
        
        full_game_data <- full_game_data %>%
                mutate(total_on_ice = rowSums(.[3:length(full_game_data)]))
        
        # Add players on-ice per team
        
        teams <- unique(data$team_id)
        
        team_1_data <- filter(data, team_id == teams[1])
        
        team_1_on_ice_data <- pivot_wider(data = team_1_data, 
                                          names_from = player_id,
                                          values_from = shift_end, 
                                          id_cols = c(game_seconds),
                                          names_prefix = "delete_")
        
        # Join to full game clock
        
        team_1_full_game_data <- full_game_clock %>%
                left_join(team_1_on_ice_data, by = "game_seconds")
        
        # Fill columns with shift end times
        
        team_1_full_game_data <- team_1_full_game_data %>%
                fill(c(colnames(team_1_full_game_data)))
        
        # Loop through the players to set on-ice as 1/0
        
        for (i in (3:length(team_1_full_game_data))) {
                
                team_1_full_game_data[i]  = ifelse(
                        team_1_full_game_data[i] >= team_1_full_game_data$game_seconds,
                        1,
                        0)
        }
        
        team_1_full_game_data[is.na(team_1_full_game_data)] <- 0
        
        # Add total players on-ice (includes goalies)
        
        team_1_full_game_data <- team_1_full_game_data %>%
                mutate(!!paste0("team_id_", teams[1]) := rowSums(.[3:length(team_1_full_game_data)]))
        
        # Shrink columns for join
        
        team_1_full_game_data <- select(team_1_full_game_data, c(2,length(team_1_full_game_data)))
        
        # Join to full game data
        
        full_game_data <- full_game_data %>%
                left_join(team_1_full_game_data, by = "game_seconds")
        
        # Repeat the same steps for team 2
        
        team_2_data <- filter(data, team_id == teams[2])
        
        team_2_on_ice_data <- pivot_wider(data = team_2_data, 
                                          names_from = player_id,
                                          values_from = shift_end, 
                                          id_cols = c(game_seconds),
                                          names_prefix = "delete_")
        
        team_2_full_game_data <- full_game_clock %>%
                left_join(team_2_on_ice_data, by = "game_seconds")
        
        team_2_full_game_data <- team_2_full_game_data %>%
                fill(c(colnames(team_2_full_game_data)))
        
        for (i in (3:length(team_2_full_game_data))) {
                
                team_2_full_game_data[i]  = ifelse(
                        team_2_full_game_data[i] >= team_2_full_game_data$game_seconds,
                        1,
                        0)
        }
        
        team_2_full_game_data[is.na(team_2_full_game_data)] <- 0
        
        team_2_full_game_data <- team_2_full_game_data %>%
                mutate(!!paste0("team_id_", teams[2]) := rowSums(.[3:length(team_2_full_game_data)]))
        
        team_2_full_game_data <- select(team_2_full_game_data, c(2,length(team_2_full_game_data)))
        
        full_game_data <- full_game_data %>%
                left_join(team_2_full_game_data, by = "game_seconds")
        
        # Replace 1/0 with player_id
        
        player_ids <- unique(data$player_id)
        
        for (i in player_ids) {
                
                full_game_data <- mutate_at(full_game_data,
                                            vars(contains(as.character(i))), 
                                            ~ ifelse(. == 1,
                                                     i,
                                                     0))
        }
        
        # Replace player_ids in column names with sequential numbers 
        
        for (i in (1:length(player_ids))) {
                
                full_game_data <- rename_with(
                        full_game_data,
                        .fn = ~ str_replace(.x, 
                                            as.character(player_ids[i]),
                                            paste0("on_ice_", i )),
                        .cols = ends_with(as.character(player_ids[i])))
        }
        
        return(full_game_data)
}

##### NHL API FUNCTIONS: GET GAME GAME LOGS

get_game_logs <- function(player_id, season) {
        
        # Get the player's full name
        
        player_site <- read_json(paste0("https://statsapi.web.nhl.com/api/v1/people/", player_id))
        player_name <- player_site$people %>%
                tibble() %>%
                unnest_wider(1)
        player_name <- player_name$fullName
        
        # Pull and organize the raw game log data
        
        gl_site <- read_json(paste0("https://statsapi.web.nhl.com/api/v1/people/", player_id, "/stats/?stats=gameLog&season=", season))
        
        stats <- gl_site$stats %>%
                tibble() %>%
                unnest_wider(1) 
        stats <- stats$splits %>%
                tibble() %>%
                unnest_longer(1) %>%
                unnest_wider(1) %>%
                unnest_wider(stat) %>%
                unnest_wider(team) %>%
                rename(team_id = id, 
                       team_name = name, 
                       team_link = link) %>%
                unnest_wider(opponent) %>%
                rename(opponent_id = id, 
                       opponent_name = name, 
                       opponent_link = link) %>%
                unnest_wider(game)
        
        # Add player data
        
        stats <- mutate(stats, player_id = player_id,
                        player = player_name)
        
        # Add special teams assists data
        
        stats <- mutate(stats, assists_pp = powerPlayPoints - powerPlayGoals,
                        assists_sh = shortHandedPoints - shortHandedGoals)
        
        # Add even strength scoring data
        
        stats <- mutate(stats, goals_es = goals - (powerPlayGoals + shortHandedGoals), 
                        assists_es = assists - (assists_pp + assists_sh))
        
        # Select the data to be returned by the function
        
        stats <- select(stats, player_id,
                        player,
                        season,
                        game_id = gamePk, 
                        date,
                        toi_as = timeOnIce, 
                        toi_es = evenTimeOnIce, 
                        toi_pp = powerPlayTimeOnIce, 
                        toi_sh = shortHandedTimeOnIce, 
                        goals, 
                        assists,
                        points,
                        shots, 
                        hits, 
                        blocks = blocked, 
                        pim, 
                        goals_es, 
                        goals_pp = powerPlayGoals, 
                        goals_sh = shortHandedGoals, 
                        goals_ot = overTimeGoals, 
                        assists_es, 
                        assists_pp, 
                        assists_sh,
                        points_pp = powerPlayPoints,
                        points_sh = shortHandedPoints,
                        team_id, 
                        team_name, 
                        opponent_id, 
                        opponent_name, 
                        is_home = isHome,
                        is_win = isWin,
                        is_ot = isOT)
        
        # Final clean up for date/time
        
        stats$date <- as.Date(stats$date)
        stats$toi_as <- ms(stats$toi_as)
        stats$toi_as <- period_to_seconds(stats$toi_as)
        stats$toi_es <- ms(stats$toi_es)
        stats$toi_es <- period_to_seconds(stats$toi_es)
        stats$toi_pp <- ms(stats$toi_pp)
        stats$toi_pp <- period_to_seconds(stats$toi_pp)
        stats$toi_sh <- ms(stats$toi_sh)
        stats$toi_sh <- period_to_seconds(stats$toi_sh)
        
        return(stats)
}

# HELPER FUNCTIONS #############################################################

# These functions modify the data pulled from the NHL's API

##### HELPER FUNCTIONS: convert_gl_to_rate_stats(game_logs)

convert_gl_to_rate_stats <- function(game_logs) {
        
        gl <- game_logs %>%
                group_by(player_id) %>%
                arrange(player_id) 
        
        ids <- unique(gl$player_id)
        
        # Compute rate stats (per second)
        
        rate_stats <- gl %>%
                summarise(gp = n(), 
                          toi_gp_as = sum(toi_as) / n(),
                          toi_gp_es = sum(toi_es) / n(),
                          toi_gp_pp = sum(toi_pp) / n(),
                          toi_gp_sh = sum(toi_sh) / n(),
                          goals_as = sum(goals) / sum(toi_as),
                          assists_as = sum(assists) / sum(toi_as),
                          shots_as = sum(shots) / sum(toi_as),
                          hits_as = sum(hits) / sum(toi_as),
                          blocks_as = sum(blocks) / sum(toi_as),
                          pim_as = sum(pim) / sum(toi_as),
                          goals_es = sum(goals_es) / sum(toi_es),
                          assists_es = sum(assists_es) / sum(toi_es),
                          goals_pp = sum(goals_pp) / sum(toi_pp),
                          assists_pp = sum(assists_pp) / sum(toi_pp),
                          goals_sh = sum(goals_sh) / sum(toi_sh),
                          assists_sh = sum(assists_sh) / sum(toi_sh)) 
        
        rate_stats$player_id <- ids
        rate_stats[is.na(rate_stats)] <- 0
        
        # Add total time-on-ice during period
        
        rate_stats <- mutate(rate_stats, toi_total = toi_gp_as * gp)
        
        # Final clean up
        
        rate_stats <- rate_stats %>%
                select(c(1,2,19,3:18))
        
        return(rate_stats)
}

##### HELPER FUNCTIONS: get_regression_data_goals(pbp_data)

get_regression_data_goals <- function(pbp_data) {
        
        working_data <- pbp_data
        
        # Remove empty net and shootouts
        
        working_data <- filter(working_data, goalie_id > 0,
                               period != 5)
        
        # Get regression variables
        
        goals_variable <- working_data %>%
                filter(event_type == "GOAL") %>%
                group_by(event_player_1_id) %>%
                summarise(goals = n())
        
        xg_variable <- working_data %>%
                filter(event_type == "GOAL" | event_type == "SHOT") %>%
                group_by(event_player_1_id) %>%
                summarise(xg = sum(xg))
        
        # Join and clean regression variables
        
        goals_variables <- xg_variable %>%
                left_join(goals_variable, by = "event_player_1_id")
        
        goals_variables <- select(goals_variables, 
                                  event_player_1_id, 
                                  goals, 
                                  xg) 
        
        goals_variables[is.na(goals_variables)] <- 0
        
        goals_variables$goal_score <- goals_variables$goals + goals_variables$xg
        
        names(goals_variables)[1] <- "player_id"
        
        goals_variables <- arrange(goals_variables, desc(goals))
        
        return(goals_variables)
}

##### HELPER FUNCTIONS: get_regression_data_assists(pbp_data)

get_regression_data_assists <- function(pbp_data) {
        
        working_data <- pbp_data
        
        # Remove empty net and shootouts
        
        working_data <- filter(working_data, goalie_id > 0,
                               period != 5)
        
        # Get regression variables
        
        a1_data <- filter(working_data, event_player_2_type == "Assist") %>%
                group_by(event_player_2_id) %>%
                summarise(primary_assists = n(),
                          xg_a1 = sum(xg)) %>%
                ungroup() %>%
                rename(player_id = event_player_2_id)
        
        a2_data <- filter(working_data, event_player_3_type == "Assist") %>%
                group_by(event_player_3_id) %>%
                summarise(secondary_assists = n(),
                          xg_a2 = sum(xg)) %>%
                ungroup() %>%
                rename(player_id = event_player_3_id)
        
        # Join and clean regression variables
        
        assists_variables <- a1_data %>%
                full_join(a2_data, by = "player_id") 
        
        assists_variables[is.na(assists_variables)] <- 0
        
        assists_variables <- mutate(assists_variables, assists = primary_assists + secondary_assists)
        
        assists_variables <- arrange(assists_variables , desc(assists))
        
        return(assists_variables )
}

##### HELPER FUNCTIONS: predict_goals(reg_data)

predict_goals <- function(reg_data) {
        
        working_data <- reg_data
        
        # Get regression coefficients
        
        y_int <- coef(reg_goals_model)[1]
        coef_goal_score <- coef(reg_goals_model)[2]
        
        # Add coefficients to working data
        
        working_data$y_int <- y_int
        working_data$coef_goal_score <- coef_goal_score
        
        # Predict goals
        
        working_data <- mutate(working_data, p_goals_ex_en =
                                       y_int +
                                       (goal_score * coef_goal_score))
        
        # Filter for qualifying skaters
        
        working_data <- filter(working_data, player_id %in% filtered_skaters)
        
        # Adjust negative goals to zero
        
        working_data <- mutate(working_data, p_goals_ex_en = ifelse(p_goals_ex_en < 0, 0, p_goals_ex_en))
        
        return(working_data)
}

##### HELPER FUNCTIONS: predict_assists(reg_data)

predict_assists <- function(reg_data) {
        
        working_data <- reg_data
        
        # Get regression coefficients
        
        y_int <- coef(reg_assists_model)[1]
        coef_xg_a1 <- coef(reg_assists_model)[2]
        coef_xg_a2 <- coef(reg_assists_model)[3]
        
        # Add coefficients to working data
        
        working_data$y_int <- y_int
        working_data$coef_xg_a1 <- coef_xg_a1
        working_data$coef_xg_a2 <- coef_xg_a2
        
        # Predict assists
        
        working_data <- mutate(working_data, p_assists_ex_en =
                                       y_int +
                                       (xg_a1 * coef_xg_a1) +
                                       (xg_a2 * coef_xg_a2))
        
        # Filter for qualifying skaters
        
        working_data <- filter(working_data, player_id %in% filtered_skaters)
        
        # Adjust negative assists to zero
        
        working_data <- mutate(working_data, p_assists_ex_en = ifelse(p_assists_ex_en < 0, 0, p_assists_ex_en))
        
        return(working_data)
}

##### HELPER FUNCTIONS: add_empty_net_goals(reg_data, pbp_data)

add_empty_net_goals <- function(reg_data, pbp_data) {
        
        working_data <- pbp_data
        
        # Isolate goals
        
        working_data <- filter(working_data, event_type == "GOAL")
        
        # Remove shootouts
        
        working_data <- filter(working_data, period != 5)
        
        # Select empty nets
        
        working_data$goalie_id[is.na(working_data$goalie_id)] <- "EN"
        working_data <- filter(working_data, goalie_id == "EN")
        
        # Get goals for each skater
        
        skater_en_goals <- working_data %>%
                group_by(event_player_1_id) %>%
                summarise(en_goals = n()) %>%
                arrange(desc(en_goals))
        
        names(skater_en_goals)[1] <- "player_id"
        
        # Group skaters using a cluster analysis (3 groups)
        
        en_goals_cluster_data <- skater_en_goals[2]
        en_goals_clusters <- kmeans(en_goals_cluster_data, 3, nstart = 100)
        skater_en_goals$cluster <- en_goals_clusters$cluster
        cluster_means <- skater_en_goals %>%
                group_by(cluster) %>%
                summarise(cluster_mean = mean(en_goals))
        skater_en_goals <- left_join(skater_en_goals, cluster_means, by = "cluster")
        
        # Join the cluster data to the goal regression data
        
        reg_data <- left_join(reg_data, skater_en_goals, by = "player_id")
        
        # Assign a zero to skaters with no empty net goals 
        
        reg_data$cluster_mean[is.na(reg_data$cluster_mean)] <- 0
        
        return(reg_data)
}

##### HELPER FUNCTIONS: add_empty_net_assists(reg_data, pbp_data)

add_empty_net_assists <- function(reg_data, pbp_data) {
        
        working_data <- pbp_data
        
        # Isolate goals
        
        working_data <- filter(working_data, event_type == "GOAL")
        
        # Remove shootouts
        
        working_data <- filter(working_data, period != 5)
        
        # Select empty nets
        
        working_data$goalie_id[is.na(working_data$goalie_id)] <- "EN"
        working_data <- filter(working_data, goalie_id == "EN")
        
        # Get assists for each skater
        
        skater_en_assists_1 <- working_data %>%
                group_by(event_player_2_id) %>%
                summarise(en_assists_1 = n()) %>%
                rename(player_id = event_player_2_id)
        
        skater_en_assists_2 <- working_data %>%
                group_by(event_player_3_id) %>%
                summarise(en_assists_2 = n()) %>%
                rename(player_id = event_player_3_id)
        
        skater_en_assists <- skater_en_assists_1 %>%
                full_join(skater_en_assists_2, by = "player_id")
        
        skater_en_assists[is.na(skater_en_assists)] <- 0
        
        skater_en_assists <- mutate(skater_en_assists, en_assists = en_assists_1 + en_assists_2)
        
        skater_en_assists <- select(skater_en_assists, player_id, en_assists)
        
        skater_en_assists <- filter(skater_en_assists, player_id > 0)
        
        # Group skaters using a cluster analysis (3 groups)
        
        en_assists_cluster_data <- skater_en_assists[2]
        en_assists_clusters <- kmeans(en_assists_cluster_data, 3, nstart = 100)
        skater_en_assists$cluster <- en_assists_clusters$cluster
        cluster_means <- skater_en_assists %>%
                group_by(cluster) %>%
                summarise(cluster_mean = mean(en_assists))
        skater_en_assists <- left_join(skater_en_assists, cluster_means, by = "cluster")
        
        # Join the cluster data to the assists regression data
        
        reg_data <- left_join(reg_data, skater_en_assists, by = "player_id")
        
        # Assign a zero to skaters with no empty net goals 
        
        reg_data$cluster_mean[is.na(reg_data$cluster_mean)] <- 0
        
        return(reg_data)
}

##### HELPER FUNCTIONS: convert_pred_to_rate_stats(pred_data, rate_stats, stat, seasons)

convert_pred_to_rate_stats <- function(pred_data, rate_stats, stat, seasons) {
        
        working_data <- rate_stats
        
        stat_column <- colnames(pred_data)[2]
        
        # Get total time-on-ice data for GP period
        
        toi_data <- select(working_data, player_id, toi_total)
        
        # Join the data
        
        pred_data <- left_join(pred_data, toi_data, by = "player_id")
        
        # Compute rate stats
        
        pred_data <- mutate(pred_data, x = get(stat_column) / toi_total)
        
        names(pred_data)[4] <- paste0("p_", stat, "_", seasons, "_rate")
        
        return(pred_data)
}

# LOAD THE DATA ################################################################

# Load data and models that have been saved locally (below)

pbp_data <- read_rds("pbp_data_xg_2017-2023.rds")

raw_game_logs_data <- read_rds("raw_game_logs_data_2017_2023.rds")

raw_schedule <- read_rds("raw_schedule_data_2017_2023.rds")

# Do not load these data if all the above have already been saved

raw_pbp_data <- read_rds("raw_pbp_data_2017_2023.rds")

xgb_model_xg <- xgb.load("xgb_model_xg.model")

# Skip down to "GET ROSTERS"

# GET THE RAW DATA #############################################################

##### GET THE RAW DATA: SCHEDULE

# Pull the regular season schedule for 2017-2018 to 2022-2023

raw_schedule <- get_schedule("2017-10-01", "2023-04-15", reg_szn = "TRUE")

write_rds(raw_schedule, "raw_schedule_data_2017_2023.rds")

##### GET THE RAW DATA: PLAY-BY-PLAY 

# Pull play-by-play data (including on-ice data) for 2017-2018 to 2022-2023

# WARNING: THIS CODE WILL TAKE HOURS TO RUN - SAVE THE DATA AFTER PULLING IT

game_ids <- raw_schedule$game_id

temp_pbp_list <- list()

for (i in 1:length(game_ids)) {
        
        pbp_data <- get_play_by_play_data(game_ids[i])
        
        temp_pbp_list[[i]] <- pbp_data
}

raw_pbp_data <- bind_rows(temp_pbp_list) %>%
        arrange(date, game_id, game_seconds)

# Remove apparent x_errors

raw_pbp_data$goal_x_error[is.na(raw_pbp_data$goal_x_error)] <- FALSE
raw_pbp_data$shot_x_error[is.na(raw_pbp_data$shot_x_error)] <- FALSE

raw_pbp_data <- raw_pbp_data %>%
        filter(goal_x_error == FALSE,
               shot_x_error == FALSE)

# Fill shot type as "Wrist Shot" where data is NA

raw_pbp_data <- mutate(raw_pbp_data, secondary_type = ifelse(
        (event_type == "SHOT" | event_type == "GOAL") &
                is.na(secondary_type),
        "Wrist Shot",
        secondary_type))

# Adjust total_on_ice where data is outside normal bounds

raw_pbp_data <- mutate(raw_pbp_data, total_on_ice = case_when(
        total_on_ice < 8 ~ 8,
        total_on_ice > 12 ~ 12,
        TRUE ~ total_on_ice))

# Tidy the columns

raw_pbp_data <- select(raw_pbp_data, c(1:47, 87, 48:86, 119, 87:118, 120))

write_rds(raw_pbp_data, "raw_pbp_data_2017_2023.rds")

##### GET THE RAW DATA: GAME LOGS

# WARNING: THIS CODE WILL TAKE TIME TO RUN - SAVE THE DATA AFTER PULLING IT

# Find active skaters in each season 

active_gl <- select(raw_pbp_data, 
                    season, 
                    game_id, 
                    event_player_1_id,
                    event_player_1_position) %>%
        unique() %>%
        filter(event_player_1_id > 0,
               event_player_1_position != "G")
        
active_gl <- active_gl %>%
        group_by(season, event_player_1_id) %>%
        summarize(gp = n())

# Remove skaters with less than 20 games played in a season

active_gl <- active_gl %>%
        filter(gp >= 20)

# Loop through the active skaters to pull game logs for each season

temp_gl_list <- list()

for (i in 1:length(active_gl$event_player_1_id)) {
        
        game_logs <- get_game_logs(active_gl[i,2], active_gl[i,1])
        
        temp_gl_list[[i]] <- game_logs
        
}

raw_game_logs_data <- bind_rows(temp_gl_list)
raw_game_logs_data <- unnest(raw_game_logs_data, cols = c(player_id))
names(raw_game_logs_data)[1] <- "player_id"

raw_game_logs_data <- unique(raw_game_logs_data)

write_rds(raw_game_logs_data, "raw_game_logs_data_2017_2023.rds")

# EXPECTED GOALS MODEL #########################################################

# This is an xG model built using XGBoost (logistic regression)

##### EXPECTED GOALS MODEL: GET THE TRAINING DATA

# Exclude data from the short COVID season

xg_training_data <- raw_pbp_data %>%
        filter(date < "2021-01-01" | date > "2021-10-01")

##### EXPECTED GOALS MODEL: PREP TRAINING FEATURES 

# Add prior event outside o-zone

xg_training_data <- xg_training_data %>%
        group_by(game_id, period) %>%
        mutate(pe_x_ozone = case_when(
                event_team == home_team &
                        lag(x_fixed) < 25 ~ as.integer(1),
                event_team == away_team & 
                        lag(x_fixed) > -25 ~ as.integer(1),
                TRUE ~ as.integer(0))) %>%
        ungroup()

# Add prior event giveaway / takeaway

xg_training_data <- xg_training_data %>%
        group_by(game_id, period) %>%
        mutate(turnover = case_when(
                event_team == lag(event_team) &
                        game_seconds - lag(game_seconds) < 6 &
                        lag(event_type ) == "TAKEAWAY" ~ as.integer(1),
                event_team != lag(event_team) &
                        game_seconds - lag(game_seconds) < 6 &
                        lag(event_type ) == "GIVEAWAY" ~ as.integer(1),
                TRUE ~ as.integer(0))) %>%
        ungroup()

# Add prior event faceoff 

xg_training_data <- xg_training_data %>%
        group_by(game_id, period) %>%
        mutate(pe_faceoff = ifelse(lag(event_type) == "FACEOFF",
                                   as.integer(1),
                                   as.integer(0))) %>%
        ungroup()

# Add prior event hit 

xg_training_data <- xg_training_data %>%
        group_by(game_id, period) %>%
        mutate(pe_hit = ifelse(lag(event_type) == "HIT",
                               as.integer(1),
                               as.integer(0))) %>%
        ungroup()

# Add elapsed time from previous event

xg_training_data <- xg_training_data %>%
        group_by(game_id, period) %>%
        mutate(elapsed_time = game_seconds - lag(game_seconds)) %>%
        ungroup()

# Filter out non-shots, empty nets, shootout

xg_training_data <- xg_training_data %>%
        filter(event_type == "SHOT" | 
                       event_type == "GOAL",
               goalie_id > 0,
               period < 5)

# Add juicy rebounds

xg_training_data <- xg_training_data %>%
        mutate(lag_gs = lag(game_seconds, n = 1))
xg_training_data$lag_gs[is.na(xg_training_data$lag_gs)] <- 0

xg_training_data <- xg_training_data %>%
        mutate(lag_y = lag(y_fixed, n = 1))
xg_training_data$lag_y[is.na(xg_training_data$lag_y)] <- 0

xg_training_data <- xg_training_data %>%
        mutate(diff_y = (abs(sqrt((y_fixed - lag_y)^2))))

xg_training_data <- xg_training_data %>%
        mutate(juicy_rebound = ifelse(
                game_seconds - lag_gs <= 3 & 
                        diff_y > 9 &
                        sa_distance <= 30 &
                        sa_angle < 90,
                as.integer(1),
                as.integer(0)))

# Add shot pressure

xg_training_data <- xg_training_data %>%
        mutate(shot_pressure = ifelse(
                event_team == lag(event_team, n = 1) &
                        event_team == lag(event_team, n = 2) &
                        event_team == lag(event_team, n = 3) &
                        period == lag(period, n = 3),
                as.integer(1),
                as.integer(0)))
xg_training_data$shot_pressure[is.na(xg_training_data$shot_pressure)] <- as.integer(0)

# Change goals/shots to 1/0

xg_training_data$event_type <- ifelse(
        xg_training_data$event_type == "GOAL",
        as.integer(1),
        as.integer(0))

# Change shot type to wrist/other

xg_training_data$secondary_type <- ifelse(
        xg_training_data$secondary_type == "Wrist Shot",
        as.integer(1),
        as.integer(0))
xg_training_data$secondary_type[is.na(xg_training_data$secondary_type)] <- as.integer(0)

# Change total on-ice to integers

xg_training_data$total_on_ice <- as.integer(xg_training_data$total_on_ice)

# Shrink the training data to goals and prediction variables

xg_training_data_shrunk <- xg_training_data %>%
        select(event_type,
               sa_distance,
               sa_angle,
               secondary_type,
               juicy_rebound,
               shot_pressure,
               pe_x_ozone,
               pe_hit,
               pe_faceoff,
               turnover,
               elapsed_time,
               total_on_ice,
               period)

# Eliminate cases where there are missing coordinates

xg_training_data_shrunk <- xg_training_data_shrunk %>%
        filter(sa_distance >= 0)

##### EXPECTED GOALS MODEL: SPLIT/PREP THE DATA FOR MODEL TRAINING 

# Split the data to retain a separate test set

train_split_xg <- sample.split(Y = xg_training_data_shrunk$event_type, 
                            SplitRatio = 0.7)

train_data_xg <- xg_training_data_shrunk[train_split_xg,]

test_data_xg <- xg_training_data_shrunk[!train_split_xg,]

# Prep the training data by separating goals/predictors

train_variables_xg <- data.matrix(train_data_xg[, -1])

train_goals_xg <- data.matrix(train_data_xg[, 1])

# Prep the testing data by separating goals/predictors

test_variables_xg <- data.matrix(test_data_xg[, -1])

test_goals_xg <- data.matrix(test_data_xg[, 1])

# Put the data in XGB matrix form

xgb_train_xg <- xgb.DMatrix(data = train_variables_xg, 
                         label = train_goals_xg)

xgb_test_xg <- xgb.DMatrix(data = test_variables_xg, 
                        label = test_goals_xg)

##### EXPECTED GOALS MODEL: TUNE THE EXPECTED GOALS MODEL 

# Use a loop to explore parameter settings
# Set the number of "tuning runs" to be performed by the loop

tuning_runs_xg <- 30

# Create a place to store the results of each tuning run

tuned_params_xg <- data.frame(matrix(nrow = tuning_runs_xg, ncol = 6))
eval_xg <- data.frame(matrix(nrow = tuning_runs_xg, ncol = 4))

# Tune XGBoost parameters using cross validation 

for (i in 1:tuning_runs_xg) {
        
        # Set up how the tuning parameters will be generated in each tuning run
        
        params_xg <- list(objective = "binary:logistic", 
                          eval_metric = "logloss", 
                          eval_metric = "auc",
                          #max_depth = sample(4:5, 1),
                          max_depth = 4,
                          subsample = runif(1, 0.7, 0.99),
                          gamma = runif(1, 0.05, 0.15))
        
        # Perform cross validation
        
        xgb_cv_xg <- xgb.cv(data = xgb_train_xg, 
                            params = params_xg, 
                            nfold = 5, 
                            nrounds = 1000,
                            verbose = TRUE, 
                            early_stopping_rounds = 10)
        
        # Capture the results

        tuned_params_xg[i,] <-  unlist(params_xg)
        eval_xg[i,1] <- min(xgb_cv_xg$evaluation_log$test_logloss_mean)
        eval_xg[i,2] <- which.min(xgb_cv_xg$evaluation_log$test_logloss_mean)
        eval_xg[i,3] <- max(xgb_cv_xg$evaluation_log$test_auc_mean)
        eval_xg[i,4] <- which.max(xgb_cv_xg$evaluation_log$test_auc_mean)
        
        # Print the tuning run that was just completed
        
        print(paste("TUNING ROUND", i, "COMPLETE"))
}

# Process the loop data and explore them in tuning_results_xg
# Note: these results were used to narrow the parameters for additional tuning runs

colnames(tuned_params_xg) <- names(params_xg)
names(tuned_params_xg)[2] <- "metric_1"
names(tuned_params_xg)[3] <- "metric_2"
colnames(eval_xg) <- c("log_loss", "log_loss_rounds", "auc", "auc_rounds")
tuning_results_xg <- cbind(tuned_params_xg, eval_xg)
tuning_results_xg <- na.omit(tuning_results_xg)
tuning_results_xg <- arrange(tuning_results_xg, desc(auc))

# Use the output of the loop to go back and refine the parameters and repeat the tuning process

##### EXPECTED GOALS MODEL: TRAIN THE EXPECTED GOALS MODEL

# This is the expected goals model 
# Parameters are based on results of tuning runs

xgb_model_xg <- xgb.train(params = list(objective = "binary:logistic", 
                                        eval_metric = "logloss", 
                                        eval_metric = "auc",
                                        max.depth = 4,
                                        subsample = 0.906041461108252,
                                        gamma = 0.0739211157895625),
                          data = xgb_train_xg,
                          nrounds = 63)

##### EXPECTED GOALS MODEL: EXPLORE THE EXPECTED GOALS MODEL

# Show the importance of the training features

xgb.importance(colnames(xgb_train_xg), model = xgb_model_xg)

# Test the expected goals model on the withheld test data

xgb_model_pred_xg <- predict(xgb_model_xg, test_variables_xg)

auc(as.vector(test_goals_xg), xgb_model_pred_xg)

# Save the expected goals model

xgb.save(xgb_model_xg, "xgb_model_xg.model")

# ADD EXPECTED GOALS TO RAW PLAY-BY-PLAY DATA ##################################

##### ADD EXPECTED GOALS TO RAW PLAY-BY-PLAY DATA: PREP WORKING DATA

# Create working data for xG analysis and add a temporary event_id (for joining)

pbp_working_data <- raw_pbp_data

pbp_working_data <- mutate(pbp_working_data, temp_event_id = seq(1:length(pbp_working_data$event_id)))

##### ADD EXPECTED GOALS TO RAW PLAY-BY-PLAY DATA: CONFORM DATA TO XG MODEL

# Add prior event outside o-zone

pbp_working_data <- pbp_working_data %>%
        group_by(game_id, period) %>%
        mutate(pe_x_ozone = case_when(
                event_team == home_team &
                        lag(x_fixed) < 25 ~ as.integer(1),
                event_team == away_team & 
                        lag(x_fixed) > -25 ~ as.integer(1),
                TRUE ~ as.integer(0))) %>%
        ungroup()

# Add prior event giveaway / takeaway

pbp_working_data <- pbp_working_data %>%
        group_by(game_id, period) %>%
        mutate(turnover = case_when(
                event_team == lag(event_team) &
                        game_seconds - lag(game_seconds) < 6 &
                        lag(event_type ) == "TAKEAWAY" ~ as.integer(1),
                event_team != lag(event_team) &
                        game_seconds - lag(game_seconds) < 6 &
                        lag(event_type ) == "GIVEAWAY" ~ as.integer(1),
                TRUE ~ as.integer(0))) %>%
        ungroup()

# Add prior event faceoff 

pbp_working_data <- pbp_working_data %>%
        group_by(game_id, period) %>%
        mutate(pe_faceoff = ifelse(lag(event_type) == "FACEOFF",
                                   as.integer(1),
                                   as.integer(0))) %>%
        ungroup()

# Add prior event hit 

pbp_working_data <- pbp_working_data %>%
        group_by(game_id, period) %>%
        mutate(pe_hit = ifelse(lag(event_type) == "HIT",
                               as.integer(1),
                               as.integer(0))) %>%
        ungroup()

# Add elapsed time from previous event

pbp_working_data <- pbp_working_data %>%
        group_by(game_id, period) %>%
        mutate(elapsed_time = game_seconds - lag(game_seconds)) %>%
        ungroup()

# Filter out non-shots, empty nets, shootout

pbp_working_data <- pbp_working_data %>%
        filter(event_type == "SHOT" | 
                       event_type == "GOAL",
               goalie_id > 0,
               period < 5)

# Add juicy rebounds

pbp_working_data <- pbp_working_data %>%
        mutate(lag_gs = lag(game_seconds, n = 1))
pbp_working_data$lag_gs[is.na(pbp_working_data$lag_gs)] <- 0

pbp_working_data <- pbp_working_data %>%
        mutate(lag_y = lag(y_fixed, n = 1))
pbp_working_data$lag_y[is.na(pbp_working_data$lag_y)] <- 0

pbp_working_data <- pbp_working_data %>%
        mutate(diff_y = (abs(sqrt((y_fixed - lag_y)^2))))

pbp_working_data <- pbp_working_data %>%
        mutate(juicy_rebound = ifelse(
                game_seconds - lag_gs <= 3 & 
                        diff_y > 9 &
                        sa_distance <= 30 &
                        sa_angle < 90,
                as.integer(1),
                as.integer(0)))

# Add shot pressure

pbp_working_data <- pbp_working_data %>%
        mutate(shot_pressure = ifelse(
                event_team == lag(event_team, n = 1) &
                        event_team == lag(event_team, n = 2) &
                        event_team == lag(event_team, n = 3) &
                        period == lag(period, n = 3),
                as.integer(1),
                as.integer(0)))
pbp_working_data$shot_pressure[is.na(pbp_working_data$shot_pressure)] <- as.integer(0)

# Change goals to 1/0

pbp_working_data$event_type <- ifelse(
        pbp_working_data$event_type == "GOAL",
        as.integer(1),
        as.integer(0))

# Change shot type to wrist / other

pbp_working_data$secondary_type <- ifelse(
        pbp_working_data$secondary_type == "Wrist Shot",
        as.integer(1),
        as.integer(0))
pbp_working_data$secondary_type[is.na(pbp_working_data$secondary_type)] <- as.integer(0)

# Remove data with missing coordinates

pbp_working_data <- filter(pbp_working_data, sa_distance >= 0)

# Change total on-ice to integers

pbp_working_data$total_on_ice <- as.integer(pbp_working_data$total_on_ice)

# Select the xG model data

pbp_working_data_shrunk <- pbp_working_data %>%
        select(event_type,
               sa_distance,
               sa_angle,
               secondary_type,
               juicy_rebound,
               shot_pressure,
               pe_x_ozone,
               pe_hit,
               pe_faceoff,
               turnover,
               elapsed_time,
               total_on_ice,
               period)

# Convert data to matrix for xgb

variables_matrix_xg <- data.matrix(pbp_working_data_shrunk[, -1])

goals_matrix_xg <- data.matrix(pbp_working_data_shrunk[, 1])

xgb_matrix_xg <- xgb.DMatrix(data = variables_matrix_xg, 
                           label = goals_matrix_xg)

##### ADD EXPECTED GOALS TO RAW PLAY-BY-PLAY DATA: GET XG VALUES

# Make predictions using the xG model

predicted_xg <- predict(xgb_model_xg, xgb_matrix_xg)

##### ADD EXPECTED GOALS TO RAW PLAY-BY-PLAY DATA: ADD XG TO PLAY-BY-PLAY DATA

# Add predictions to play-by-play data

pbp_working_data$xg <- predicted_xg

# Isolate xG data with temp_event_id

pbp_working_data <- select(pbp_working_data, temp_event_id, xg)

# Join xG to main play-by-play data

pbp_data <- raw_pbp_data
pbp_data <- mutate(pbp_data, temp_event_id = seq(1:length(pbp_data$event_id)))

pbp_data <- pbp_data %>%
        left_join(pbp_working_data, by = "temp_event_id") %>%
        select(-temp_event_id)

# Save the play-by-play data with added xG

write_rds(pbp_data, "pbp_data_xg_2017-2023.rds")

# PROJECTED GOALS MODEL ########################################################

# I project goals using a simple linear regression model
# The model has only one independent variable called a "goal_score"
# A goal_score is actual goals + expected goals for a given time period
# It is used to predict goals for that time period (i.e., linear regression)
# This "model" determines how to blend that data to predict future goals 
# The model uses data from 2017-2020
# Data from the 2017-2018 and 2018-2019 seasons are used to build the model
# Data from the 2019-2020 season are used to find the "best blend" 

###### PROJECTED GOALS MODEL: PREP DATA

scoring_training_data <- filter(pbp_data, date < "2020-05-01")

# Remove shootouts, empty nets and goalies

scoring_training_data <- filter(scoring_training_data, 
                                period < 5,
                                goalie_id > 0,
                                event_player_1_position != "G")

# Shrink to xG events and remove data with missing coordinates

scoring_training_data <-  filter(scoring_training_data, 
                                 event_type == "SHOT" | event_type == "GOAL",
                                 xg >0)

# Split training data into model building and model testing

scoring_training_data_building <- filter(scoring_training_data, season == "20172018" | season == "20182019")

scoring_training_data_testing <- filter(scoring_training_data, season == "20192020")

###### PROJECTED GOALS MODEL: GET XG AND GOALS DATA

# Get skater xG and goals data for both building seasons

goals_training_data_building <- scoring_training_data_building %>%
        group_by(event_player_1_id) %>%
        summarize(goals_2 = sum(event_type == "GOAL"), 
                  xg_2 = sum(xg)) %>%
        mutate(goal_score_2 = goals_2 + xg_2) %>%
        ungroup()

# Add xG and goals data for only the most recent season

goals_xg_1 <- scoring_training_data_building %>%
        filter(season == "20182019") %>%
        group_by(event_player_1_id) %>%
        summarize(goals_1 = sum(event_type == "GOAL"), 
                  xg_1 = sum(xg)) %>%
        mutate(goal_score_1 = goals_1 + xg_1) %>%
        ungroup()

goals_training_data_building <- goals_training_data_building %>%
        left_join(goals_xg_1, by = "event_player_1_id")

###### PROJECTED GOALS MODEL: APPLY SOME FILTERS

# Filter out skaters with no data in the most recent season

goals_training_data_building <- filter(goals_training_data_building, goal_score_1 > 0)

# Filter skaters based on minimum 100 games played in building seasons

goals_training_data_gp <- scoring_training_data_building %>%
        select(game_id, event_player_1_id) %>%
        unique() %>%
        group_by(event_player_1_id) %>%
        summarize(gp = n()) %>%
        ungroup()

goals_training_data_gp <- goals_training_data_gp %>%
        filter(gp >= 100)

goals_training_data_building <- filter(goals_training_data_building, event_player_1_id %in% goals_training_data_gp$event_player_1_id)

###### PROJECTED GOALS MODEL: GET TIME-ON-ICE DATA

# Get TOI data (full building, most recent building, testing)

scoring_training_toi_data <- filter(raw_game_logs_data, date < "2020-05-01")

scoring_training_toi_data_building <- filter(scoring_training_toi_data, season == "20172018" | season == "20182019")

scoring_training_toi_data_testing <- filter(scoring_training_toi_data, season == "20192020")

scoring_training_toi_2 <- scoring_training_toi_data_building %>%
        group_by(player_id) %>%
        summarize(toi_2 = sum(toi_as)) %>%
        ungroup() %>%
        rename(event_player_1_id = player_id)

scoring_training_toi_1 <- scoring_training_toi_data_building %>%
        filter(season == "20182019") %>%
        group_by(player_id) %>%
        summarize(toi_1 = sum(toi_as)) %>%
        ungroup() %>%
        rename(event_player_1_id = player_id)

scoring_training_toi_testing <- scoring_training_toi_data_testing %>%
        group_by(player_id) %>%
        summarize(toi_results = sum(toi_as)) %>%
        ungroup() %>%
        rename(event_player_1_id = player_id)

# Add TOI data to building data and convert to rate stats

goals_training_data_building <- goals_training_data_building %>%
        left_join(scoring_training_toi_1, by = "event_player_1_id") %>%
        left_join(scoring_training_toi_2, by = "event_player_1_id")

goals_training_data_building <- goals_training_data_building %>%
        mutate(gs_2_rate = (goal_score_2 / toi_2) * 10000,
               gs_1_rate = (goal_score_1 / toi_1) * 10000,
               goals_2_rate = (goals_2 / toi_2) * 10000)

###### PROJECTED GOALS MODEL: THE LINEAR REGRESSION MODEL

# Build a linear regression model using data from the full building period

predicted_goals_model <- lm(goals_2_rate ~ gs_2_rate, goals_training_data_building)
predicted_goals_model_intercept <- predicted_goals_model$coefficients[1]
predicted_goals_model_gs_2_rate <- predicted_goals_model$coefficients[2]

###### PROJECTED GOALS MODEL: MODEL PREDICTIONS

# Use the coefficients from the predicted_goals_model to predict goals
# Separate predictions made for full building period and most recent season

goals_training_data_building <- goals_training_data_building %>%
        mutate(p_goals_2 = predicted_goals_model_intercept + (gs_2_rate * predicted_goals_model_gs_2_rate),
               p_goals_1 = predicted_goals_model_intercept + (gs_1_rate * predicted_goals_model_gs_2_rate))

###### PROJECTED GOALS MODEL: TEST THE PREDICTIONS

# Get actual goals from the testing season and convert to a rate stat

goals_training_data_testing <- filter(scoring_training_data_testing, event_type == "GOAL") %>%
        group_by(event_player_1_id) %>%
        summarize(goals = n()) %>%
        ungroup()

goals_training_data_testing <- goals_training_data_testing %>%
        left_join(scoring_training_toi_testing, by = "event_player_1_id")

goals_training_data_testing <- mutate(goals_training_data_testing, goals_rate = (goals / toi_results) * 10000)

goals_training_data_testing <- select(goals_training_data_testing, event_player_1_id, goals_rate)

# Add the results to the building data and filter out NAs in testing data

goals_training_data_building <- goals_training_data_building %>%
        left_join(goals_training_data_testing, by = "event_player_1_id")

goals_training_data_building <- filter(goals_training_data_building, goals_rate > 0)

# Create a loop to explore how to blend the building data to predict results
# Evaluation metric is RMSE
# Set number of tuning runs and create a place to store the results

tuning_runs_goals <- 100

goals_blend <- data.frame(matrix(nrow = tuning_runs_goals, ncol = 3))

# Perform the tuning runs

for (i in 1:tuning_runs_goals) {
        
        # Set up how the blend will be generated in each tuning run
        
        p_goals_1_prop = runif(1, 0.1, 0.9)
        p_goals_2_prop = 1 - p_goals_1_prop
        
        goals_loop_data <- goals_training_data_building %>%
                mutate(p_goals = (p_goals_1 * p_goals_1_prop) + (p_goals_2 * p_goals_2_prop))
        
        rmse <- sqrt(mean((goals_loop_data$goals_rate - goals_loop_data$p_goals)^2))
        
        goals_blend[i,1] <- p_goals_1_prop
        goals_blend[i,2] <- p_goals_2_prop
        goals_blend[i,3] <- rmse
        
        # Print the tuning run that was just completed
        
        print(paste("TUNING ROUND", i, "COMPLETE"))
}

# Process the loop data and explore them

colnames(goals_blend) <- c("p_goals_1", "p_goals_2", "rmse")
goals_blend <- goals_blend %>%
        arrange(rmse)

# It looks like a 20/80 blend would be OK

# PROJECTED ASSISTS MODEL ######################################################

# Perform a similar analysis for projected assists
# This analysis is based on the xG values attached to assists

##### PROJECTED ASSISTS MODEL: PREP DATA

# Get data for skater shots and assists in both building seasons

assists_training_data_building <- scoring_training_data_building %>%
        filter(event_player_2_type == "Assist") %>%
        group_by(event_player_2_id) %>%
        summarize(a1_2 = n(),
                  xg_a1_2 = sum(xg)) %>%
        ungroup() %>%
        rename(player_id = event_player_2_id)

xg_a2_2 <- scoring_training_data_building %>%
        filter(event_player_3_type == "Assist") %>%
        group_by(event_player_3_id) %>%
        summarize(a2_2 = n(),
                  xg_a2_2 = sum(xg)) %>%
        ungroup() %>%
        rename(player_id = event_player_3_id)

assists_training_data_building <- assists_training_data_building %>%
        full_join(xg_a2_2, by = "player_id") 
assists_training_data_building[is.na(assists_training_data_building)] <- 0

# Add data for skater shots and assists in only the most recent season

assists_xg_a1_1 <- scoring_training_data_building %>%
        filter(season == "20182019",
               event_player_2_type == "Assist") %>%
        group_by(event_player_2_id) %>%
        summarize(a1_1 = n(),
                  xg_a1_1 = sum(xg)) %>%
        ungroup() %>%
        rename(player_id = event_player_2_id)

assists_xg_a2_1 <- scoring_training_data_building %>%
        filter(season == "20182019",
               event_player_3_type == "Assist") %>%
        group_by(event_player_3_id) %>%
        summarize(a2_1 = n(),
                  xg_a2_1 = sum(xg)) %>%
        ungroup() %>%
        rename(player_id = event_player_3_id)

assists_training_data_building <- assists_training_data_building %>%
        left_join(assists_xg_a1_1, by = "player_id") %>%
        left_join(assists_xg_a2_1, by = "player_id")

##### PROJECTED ASSISTS MODEL: APPLY SOME FILTERS AND TIDY UP

# Filter out skaters with no data in the most recent season

assists_training_data_building <- filter(assists_training_data_building, 
                                         xg_a1_1 > 0,
                                         xg_a2_1 > 0)

# Filter skaters based on minimum 100 games played in building seasons

assists_training_data_gp <- goals_training_data_gp %>%
        rename(player_id = event_player_1_id)

assists_training_data_building <- filter(assists_training_data_building, player_id %in% assists_training_data_gp$player_id)

# Add total assists and tidy up

assists_training_data_building <- mutate(assists_training_data_building, 
                                         assists_2 = a1_2 + a2_2)

assists_training_data_building <- select(assists_training_data_building, 
                                         event_player_1_id = player_id,
                                         assists_2,
                                         xg_a1_2,
                                         xg_a2_2,
                                         xg_a1_1,
                                         xg_a2_1)

##### PROJECTED ASSISTS MODEL: CONVERT TO RATE STATS

# Use TOI data (from projected goals model) to convert to rate stats

assists_training_data_building <- assists_training_data_building %>%
        left_join(scoring_training_toi_1, by = "event_player_1_id") %>%
        left_join(scoring_training_toi_2, by = "event_player_1_id")

assists_training_data_building <- assists_training_data_building %>%
        mutate(xg_a1_2_rate = (xg_a1_2 / toi_2) * 100000,
               xg_a2_2_rate = (xg_a2_2 / toi_2) * 100000,
               xg_a1_1_rate = (xg_a1_1 / toi_1) * 100000,
               xg_a2_1_rate = (xg_a2_1 / toi_1) * 100000,
               assists_2_rate = (assists_2 / toi_2) * 10000)

##### PROJECTED ASSISTS MODEL: LINEAR REGRESSION MODEL

# Build a linear regression model using data from the full building period

predicted_assists_model <- lm(assists_2_rate ~ xg_a1_2_rate + xg_a2_2_rate, assists_training_data_building)
predicted_assists_model_intercept <- predicted_assists_model$coefficients[1]
predicted_assists_model_xg_a1_2_rate <- predicted_assists_model$coefficients[2]
predicted_assists_model_xg_a2_2_rate <- predicted_assists_model$coefficients[3]

##### PROJECTED ASSISTS MODEL: MODEL PREDICTIONS

# Use the coefficients from the predicted_assists_model to predict assists
# Separate predictions made for full building period and most recent season

assists_training_data_building <- assists_training_data_building %>%
        mutate(p_assists_2 = 
                       predicted_assists_model_intercept + 
                       (xg_a1_2_rate * predicted_assists_model_xg_a1_2_rate) +
                       (xg_a2_2_rate * predicted_assists_model_xg_a2_2_rate),
               p_assists_1 = 
                       predicted_assists_model_intercept + 
                       (xg_a1_1_rate * predicted_assists_model_xg_a1_2_rate) +
                       (xg_a2_1_rate * predicted_assists_model_xg_a2_2_rate))

##### PROJECTED ASSISTS MODEL: TEST THE PREDICTIONS

# Get actual assists from the testing season and convert to a rate stat

assists_training_data_testing <- filter(scoring_training_data_testing, event_type == "GOAL")

assists_training_data_testing_a1 <- filter(assists_training_data_testing, event_player_2_type == "Assist") %>%
        group_by(event_player_2_id) %>%
        summarize(a1 = n()) %>%
        ungroup() %>%
        rename(event_player_1_id = event_player_2_id)

assists_training_data_testing_a2 <- filter(assists_training_data_testing, event_player_3_type == "Assist") %>%
        group_by(event_player_3_id) %>%
        summarize(a2 = n()) %>%
        ungroup() %>%
        rename(event_player_1_id = event_player_3_id)

assists_training_data_testing <- assists_training_data_testing_a1 %>%
        left_join(assists_training_data_testing_a2, by = "event_player_1_id") %>%
        mutate(assists = a1 + a2)

assists_training_data_testing <- assists_training_data_testing %>%
        left_join(scoring_training_toi_testing, by = "event_player_1_id")

assists_training_data_testing <- mutate(assists_training_data_testing, assists_rate = (assists / toi_results) * 10000)

assists_training_data_testing <- select(assists_training_data_testing, event_player_1_id, assists_rate)

# Add the results to the building data and filter out NAs in testing data

assists_training_data_building <- assists_training_data_building %>%
        left_join(assists_training_data_testing, by = "event_player_1_id")

assists_training_data_building <- filter(assists_training_data_building, assists_rate > 0)

# Create a loop to explore how to blend the building data to predict results
# Evaluation metric is RMSE
# Set number of tuning runs and create a place to store the results

tuning_runs_assists <- 100

assists_blend <- data.frame(matrix(nrow = tuning_runs_assists, ncol = 3))

# Perform the tuning runs

for (i in 1:tuning_runs_assists) {
        
        # Set up how the blend will be generated in each tuning run
        
        p_assists_1_prop = runif(1, 0.1, 0.9)
        p_assists_2_prop = 1 - p_assists_1_prop
        
        assists_loop_data <- assists_training_data_building %>%
                mutate(p_assists = (p_assists_1 * p_assists_1_prop) + (p_assists_2 * p_assists_2_prop))
        
        rmse <- sqrt(mean((assists_loop_data$assists_rate - assists_loop_data$p_assists)^2))
        
        assists_blend[i,1] <- p_assists_1_prop
        assists_blend[i,2] <- p_assists_2_prop
        assists_blend[i,3] <- rmse
        
        # Print the tuning run that was just completed
        
        print(paste("TUNING ROUND", i, "COMPLETE"))
}

# Process the loop data and explore them

colnames(assists_blend) <- c("p_assists_1", "p_assists_2", "rmse")
assists_blend <- assists_blend %>%
        arrange(rmse)

# A 20/80 blend seems reasonable here and would match the goals blend (above)

# GET ROSTERS ##################################################################

raw_team_rosters <- get_team_rosters()

# Clean and filter position data (remove goalies)

team_rosters <- filter(raw_team_rosters, position != "G") %>%
        mutate(position = ifelse(position == "D", "D", "F")) %>%
        select(c(1:7))

# PROCESS GAME LOGS ############################################################

# Group the game logs by season

gl_data_2 <- raw_game_logs_data %>%
        filter(season == "20212022" | season == "20222023")

gl_data_1 <- raw_game_logs_data %>%
        filter(season == "20222023")

# Convert the game logs to rate stats (per second)

rate_stats_2 <- convert_gl_to_rate_stats(gl_data_2)

rate_stats_1 <- convert_gl_to_rate_stats(gl_data_1)

# QUALIFYING SKATERS ###########################################################

# The game logs were pre-filtered to include only skaters with 20GP in a season
# Add a 100GP filter for the last two seasons

filtered_skaters <- filter(rate_stats_2, gp >= 100)
filtered_skaters <- filtered_skaters$player_id

# Create base for skater projections data

projections_base <- team_rosters %>%
        filter(player_id %in% filtered_skaters)

# PROJECTED GOALS (RAW) ########################################################

##### PROJECTED GOALS (RAW): REGRESSION ANALYSIS

# Get base data for regression analysis

pbp_data_2 <- filter(pbp_data, season == "20212022" | season == "20222023")

reg_goals_2 <- get_regression_data_goals(pbp_data_2)

# Filter to eliminate skaters with few games played

reg_goals_2 <- filter(reg_goals_2, player_id %in% filtered_skaters)

# Build the linear regression model

reg_goals_model <- lm(goals ~ goal_score, reg_goals_2)

print(summary(reg_goals_model)[4])
print(summary(reg_goals_model)[9])

##### PROJECTED GOALS (RAW): PREDICT GOALS (2 SEASONS)

# Add goals predicted by regression model

reg_goals_2 <- predict_goals(reg_goals_2)

# Add a prediction for empty net goals based on cluster analysis

reg_goals_2 <- add_empty_net_goals(reg_goals_2, pbp_data_2)

# Combine predictions and shrink data

p_goals_2 <- reg_goals_2 %>%
        mutate(p_goals_2_count = p_goals_ex_en + cluster_mean) %>%
        select(player_id, 
               p_goals_2_count)

# Convert prediction to a rate stat (per second)

p_goals_2 <- convert_pred_to_rate_stats(p_goals_2, rate_stats_2, "goals", "2")
p_goals_2 <- select(p_goals_2, 
                    c(1,4))

##### PROJECTED GOALS (RAW): PREDICT GOALS (LAST SEASON ONLY)

# Get base data for regression analysis

pbp_data_1 <- filter(pbp_data, season == "20222023")

reg_goals_1 <- get_regression_data_goals(pbp_data_1)

# Add goals predicted by regression model

reg_goals_1 <- predict_goals(reg_goals_1)

# Add a prediction for empty net goals based on cluster analysis

reg_goals_1 <- add_empty_net_goals(reg_goals_1, pbp_data_1)

# Combine predictions and shrink data

p_goals_1 <- reg_goals_1 %>%
        mutate(p_goals_1_count = p_goals_ex_en + cluster_mean) %>%
        select(player_id, 
               p_goals_1_count)

# Convert prediction to a rate stat (per second)

p_goals_1 <- convert_pred_to_rate_stats(p_goals_1, rate_stats_1, "goals", "1")
p_goals_1 <- select(p_goals_1, 
                    c(1,4))

##### PROJECTED GOALS (RAW): JOIN AND BLEND PREDICTIONS

p_goals_raw <- p_goals_2 %>%
        left_join(p_goals_1, by = "player_id") 

# Blend is based on projected goals model (above)

p_goals_raw <- mutate(p_goals_raw, p_goals_raw = (p_goals_2_rate * 0.8) + (p_goals_1_rate * 0.2))

p_goals_raw <- select(p_goals_raw, 
                      player_id, 
                      p_goals_raw)

# PROJECTED ASSISTS (RAW) ######################################################

##### PROJECTED ASSISTS (RAW): REGRESSION ANALYSIS

# Get base data for regression analysis

reg_assists_2 <- get_regression_data_assists(pbp_data_2)

# Filter to eliminate skaters with few games played

reg_assists_2 <- filter(reg_assists_2, player_id %in% filtered_skaters)

# Build the linear regression model

reg_assists_model <- lm(assists ~ xg_a1 + xg_a2, reg_assists_2)

print(summary(reg_assists_model)[4])
print(summary(reg_assists_model)[9])

##### PROJECTED ASSISTS (RAW): PREDICT ASSISTS (2 SEASONS)

# Add assists predicted by regression model

reg_assists_2 <- predict_assists(reg_assists_2)

# Add a prediction for empty net assists based on cluster analysis

reg_assists_2 <- add_empty_net_assists(reg_assists_2, pbp_data_2)

# Combine predictions and shrink data

p_assists_2 <- reg_assists_2 %>%
        mutate(p_assists_2_count = p_assists_ex_en + cluster_mean) %>%
        select(player_id, 
               p_assists_2_count)

# Convert prediction to a rate stat (per second)

p_assists_2 <- convert_pred_to_rate_stats(p_assists_2, rate_stats_2, "assists", "2")
p_assists_2 <- select(p_assists_2, 
                    c(1,4))

##### PROJECTED ASSISTS (RAW): PREDICT ASSISTS (LAST SEASON ONLY)

# Get base data for regression analysis

reg_assists_1 <- get_regression_data_assists(pbp_data_1)

# Add assists predicted by regression model

reg_assists_1 <- predict_assists(reg_assists_1)

# Add a prediction for empty net assists based on cluster analysis

reg_assists_1 <- add_empty_net_assists(reg_assists_1, pbp_data_1)

# Combine predictions and shrink data

p_assists_1 <- reg_assists_1 %>%
        mutate(p_assists_1_count = p_assists_ex_en + cluster_mean) %>%
        select(player_id, 
               p_assists_1_count)

# Convert prediction to a rate stat (per second)

p_assists_1 <- convert_pred_to_rate_stats(p_assists_1, rate_stats_1, "assists", "1")
p_assists_1 <- select(p_assists_1, 
                    c(1,4))

##### PROJECTED ASSISTS (RAW): JOIN AND BLEND PREDICTIONS

p_assists_raw <- p_assists_2 %>%
        left_join(p_assists_1, by = "player_id") 

p_assists_raw[is.na(p_assists_raw)] <- 0

# Blend is based on projected assists model (above)

p_assists_raw  <- mutate(p_assists_raw , p_assists_raw = (p_assists_2_rate * 0.8) + (p_assists_1_rate * 0.2))

p_assists_raw <- select(p_assists_raw, 
                      player_id, 
                      p_assists_raw)

# PROJECTED SHOTS/HITS/BLOCKS (RAW) ############################################

# This is simply a blend of rate stats 
# It's an area that could use some more work

# Get the rate stats

p_shb_2 <- rate_stats_2 %>%
        select(c(1,10,11,12))
names(p_shb_2)[2] <- "shots_rate_2"
names(p_shb_2)[3] <- "hits_rate_2"
names(p_shb_2)[4] <- "blocks_rate_2"

p_shb_1 <- rate_stats_1 %>%
        select(c(1,10,11,12))
names(p_shb_1)[2] <- "shots_rate_1"
names(p_shb_1)[3] <- "hits_rate_1"
names(p_shb_1)[4] <- "blocks_rate_1"

# Join and blend the rate stats (40/60)

p_shb_raw <- p_shb_2 %>%
        left_join(p_shb_1, by = "player_id") %>%
        filter(player_id %in% filtered_skaters)

p_shb_raw <- p_shb_raw %>%
        mutate(p_shots_raw = (shots_rate_2 * 0.6) + (shots_rate_1 * 0.4),
               p_hits_raw = (hits_rate_2 * 0.6) + (hits_rate_1 * 0.4),
               p_blocks_raw = (blocks_rate_2 * 0.6) + (blocks_rate_1 * 0.4))

p_shb_raw <- select(p_shb_raw, c(player_id,
                                 p_shots_raw,
                                 p_hits_raw,
                                 p_blocks_raw))

# PROJECTED TOI (RAW) ##########################################################

# Collect time-on-ice per GP data

toi_as_2 <- rate_stats_2 %>%
        mutate(toi_2 = toi_total / gp) %>%
        select(player_id, toi_2)
toi_as_1 <- rate_stats_1 %>%
        mutate(toi_1 = toi_total / gp) %>%
        select(player_id, toi_1)

# Join and blend the TOI data

p_toi_raw <- toi_as_2 %>%
        left_join(toi_as_1, by = "player_id") %>%
        filter(player_id %in% filtered_skaters)

p_toi_raw <- mutate(p_toi_raw, p_toi_raw = (toi_2 * 0.3) + (toi_1 * 0.7)) 
p_toi_raw <- select(p_toi_raw, 
                    player_id, 
                    p_toi_raw)

# ASSEMBLE RAW SKATER PROJECTIONS ##############################################

# Join the raw data

raw_skater_projections <- projections_base %>%
        left_join(p_toi_raw, by = "player_id") %>%
        left_join(p_goals_raw, by = "player_id") %>%
        left_join(p_assists_raw, by = "player_id") %>%
        left_join(p_shb_raw, by = "player_id")

# ADJUSTMENTS TO SKATER PROJECTIONS ############################################

##### ADJUSTMENTS TO SKATER PROJECTIONS: ADJUST TOI (CLUSTER + AGE)

# Prepare data for clustering based on TOI + points rate (split by position)

toi_forwards_data <- filter(raw_skater_projections, position == "F") %>%
        mutate(p_raw_points = p_goals_raw + p_assists_raw)

toi_forwards_cluster_data <- select(toi_forwards_data, 
                                    p_toi_raw, 
                                    p_raw_points)

toi_defensemen_data <- filter(raw_skater_projections, position == "D") %>%
        mutate(p_raw_points = p_goals_raw + p_assists_raw)

toi_defensemen_cluster_data <- select(toi_defensemen_data, 
                                      p_toi_raw, 
                                      p_raw_points)

# Cluster analysis (split by position)

toi_forwards_clusters <- kmeans(toi_forwards_cluster_data, 24, nstart = 100, iter.max = 15)

toi_defensemen_clusters <- kmeans(toi_defensemen_cluster_data, 12, nstart = 100, iter.max = 15)

# Assign clusters and average TOI to main data

toi_avg_forwards <- as_tibble(toi_forwards_clusters$centers[,1])
names(toi_avg_forwards)[1] <- "cluster_toi"
toi_avg_forwards$cluster <- seq(1:24)

toi_forwards_data$cluster <- toi_forwards_clusters$cluster
toi_forwards_data <- left_join(toi_forwards_data, toi_avg_forwards, by = "cluster")

toi_avg_defensemen <- as_tibble(toi_defensemen_clusters$centers[,1])
names(toi_avg_defensemen)[1] <- "cluster_toi"
toi_avg_defensemen$cluster <- seq(1:12)

toi_defensemen_data$cluster <- toi_defensemen_clusters$cluster
toi_defensemen_data <- left_join(toi_defensemen_data, toi_avg_defensemen, by = "cluster")

# Combine forwards and defensemen and remove cluster

raw_skater_projections <- bind_rows(toi_forwards_data, toi_defensemen_data)

raw_skater_projections <- select(raw_skater_projections, -cluster)

# Adjust TOI for age > 33 at October 1, 2023 (97% of cluster TOI)

raw_skater_projections <- mutate(raw_skater_projections, age_sos = as.Date("2023-10-01") - dob)
raw_skater_projections$age_adj_toi <- ifelse(
        raw_skater_projections$age_sos > 12419, 
        (raw_skater_projections$cluster_toi * 0.97), 
        raw_skater_projections$cluster_toi) 

# Adjust TOI for age < 25 at October 1, 2023 
# Adjust skaters who have high TOI deviation with the hope that these skaters will have fewer "low" TOI games as they improve (50% of 1 standard deviation)

u_25_skaters <- filter(raw_skater_projections, age_sos < 8766)
u_25_skaters <- u_25_skaters$player_id

toi_sd_data <- gl_data_1 %>%
        group_by(player_id) %>%
        summarise(avg_toi = mean(toi_as), 
                  sd_toi = sd(toi_as)) %>%
        mutate(variation = sd_toi / avg_toi) %>%
        filter(player_id %in% u_25_skaters) %>%
        filter(variation > 0.15)

toi_sd_data <- mutate(toi_sd_data, u_25_toi_adj = sd_toi * 0.5) %>%
        select(player_id, 
               u_25_toi_adj)

raw_skater_projections <- left_join(raw_skater_projections, toi_sd_data, by = "player_id")
raw_skater_projections$u_25_toi_adj[is.na(raw_skater_projections$u_25_toi_adj)] <- 0

raw_skater_projections <- mutate(raw_skater_projections, p_toi = age_adj_toi + u_25_toi_adj) %>%
        select(-cluster_toi, 
               -age_adj_toi, 
               -u_25_toi_adj)

##### ADJUSTMENTS TO SKATER PROJECTIONS: ADJUST GOALS (ELITE SHOOTERS + AGE)

# Identify forwards who maintain a high shooting percentage

forward_ids <- filter(raw_skater_projections, position == "F")
forward_ids <- forward_ids$player_id

elite_shooter_data <- raw_game_logs_data %>%
        filter(player_id %in% forward_ids) %>%
        group_by(player_id) %>%
        summarise(gp = n(),
                  shots = sum(shots),
                  goals = sum(goals))

elite_shooter_data <- mutate(elite_shooter_data, shot_pct = goals / shots)

shot_pct_avg <- mean(elite_shooter_data$shot_pct)
shot_pct_sd <- sd(elite_shooter_data$shot_pct)
elite_shooter_pct <- shot_pct_avg + (2 * shot_pct_sd )

elite_shooter_data <- filter(elite_shooter_data, gp >= 120,
                             shot_pct >= elite_shooter_pct) %>%
        select(player_id, 
               shot_pct)

elite_shooter_ids <- elite_shooter_data$player_id

# Adjust goals for elite shooters (60 / 40)

raw_skater_projections <- left_join(raw_skater_projections, elite_shooter_data, by = "player_id")

raw_skater_projections <- mutate(raw_skater_projections, es_adj_goals = ifelse(
        player_id %in% elite_shooter_ids,
        (p_goals_raw * 0.4) + ((p_shots_raw * shot_pct) * 0.6), 
        p_goals_raw))

# Adjust goals for age > 33 at October 1, 2023 (97% of es_adj_goals)

raw_skater_projections$adj_goals <- ifelse(
        raw_skater_projections$age_sos > 12419,
        (raw_skater_projections$es_adj_goals * 0.97), 
        raw_skater_projections$es_adj_goals) 

raw_skater_projections <- select(raw_skater_projections, 
                                 -shot_pct, 
                                 -es_adj_goals )

##### ADJUSTMENTS TO SKATER PROJECTIONS: ADJUST ASSISTS/SHOTS/HITS (AGE)

# Adjust remaining stats (excluding blocks) for age > 33 at October 1, 2023 (97%)

raw_skater_projections$adj_assists <- ifelse(
        raw_skater_projections$age_sos > 12419, 
        (raw_skater_projections$p_assists_raw * 0.97), 
        raw_skater_projections$p_assists_raw)

raw_skater_projections$p_shots <- ifelse(
        raw_skater_projections$age_sos > 12419, 
        (raw_skater_projections$p_shots_raw * 0.97), 
        raw_skater_projections$p_shots_raw)

raw_skater_projections$p_hits <- ifelse(
        raw_skater_projections$age_sos > 12419, 
        (raw_skater_projections$p_hits_raw * 0.97), 
        raw_skater_projections$p_hits_raw)

raw_skater_projections$p_blocks <- raw_skater_projections$p_blocks_raw 

raw_skater_projections <- select(raw_skater_projections, -age_sos)

##### ADJUSTMENTS TO SKATER PROJECTIONS: ADJUST GOALS/ASSISTS (EVIRONMENT)

# This is a challenging adjustment and it's still a work in progress
# The idea is to adjust scoring projections based on changes to a skater's environment
# It will take into account changing team rosters but is not limited to roster changes
# It will also make adjustments where the on-ice environment was significantly different than league average
# This adjustment can over-reward the depth players on good teams

# Use xG data (goals + assists) as a proxy for recent on-ice scoring environment 

skater_xg_data_1 <- reg_assists_1 %>%
        left_join(reg_goals_1, by = "player_id") %>%
        select(player_id,
               xg_goals = xg,
               xg_a1,
               xg_a2) %>%
        mutate(xg_assists = xg_a1 + xg_a2) %>%
        filter(player_id %in% raw_skater_projections$player_id)  

# Give each skater z-scores (goals + assists) based on his scoring environment

mean_goals <- mean(skater_xg_data_1$xg_goals)
sd_goals <- sd(skater_xg_data_1$xg_goals)
mean_assists <- mean(skater_xg_data_1$xg_assists)
sd_assists <- sd(skater_xg_data_1$xg_assists)

skater_xg_data_1 <- skater_xg_data_1 %>%
        mutate(z_goals = (xg_goals - mean_goals) / sd_goals,
               z_assists = (xg_assists - mean_assists) / sd_assists) %>%
        select(player_id,
               z_goals,
               z_assists)

# Give each team a z-score based on the top 9 projected goal scorers

team_z_score <- select(raw_skater_projections, 
                       player_id, 
                       team, 
                       p_toi, 
                       adj_goals)
team_z_score <- mutate(team_z_score, goals = p_toi * adj_goals) %>%
        group_by(team) %>%
        top_n(9, goals) %>%
        ungroup() %>%
        arrange(team, desc(goals))

team_z_score <- team_z_score %>%
        group_by(team) %>%
        summarize(team_goals = sum(goals))

mean_goals_team <- mean(team_z_score$team_goals)
sd_goals_team <- sd(team_z_score$team_goals)

team_z_score <- mutate(team_z_score, z_team = (team_goals - mean_goals_team) / sd_goals_team) %>%
        select(team,
               z_team)

# Join z-score data to skater projections

raw_skater_projections <- raw_skater_projections %>%
        left_join(skater_xg_data_1, by = "player_id") %>%
        left_join(team_z_score, by = "team")

# Compare skater z-scores to team z-scores

raw_skater_projections <- raw_skater_projections %>%
        mutate(diff_z_goals = abs(z_team - z_goals),
               diff_z_assists = abs(z_team - z_assists))

raw_skater_projections <- raw_skater_projections %>%
        mutate(diff_z_goals = ifelse(
                z_goals > z_team,
                diff_z_goals * -1,
                diff_z_goals),
               diff_z_assists = ifelse(
                       z_assists > z_team,
                       diff_z_assists * -1,
                       diff_z_assists))

# Add standard deviations for diff_z goals / assists

raw_skater_projections <- raw_skater_projections %>%
        mutate(sd_diff_z_goals = sd(diff_z_goals),
               sd_diff_z_assists = sd(diff_z_assists))

# Isolate the portion of z-scores that are above/below one standard deviation

raw_skater_projections <- raw_skater_projections %>%
        mutate(o_u_sd_goals = ifelse(
                diff_z_goals < 0,
                diff_z_goals + sd_diff_z_goals,
                diff_z_goals - sd_diff_z_goals),
               o_u_sd_assists = ifelse(
                       diff_z_assists < 0,
                       diff_z_assists + sd_diff_z_assists,
                       diff_z_assists - sd_diff_z_assists))

# Identify skaters with z-score differences above/below one standard deviation

raw_skater_projections <- raw_skater_projections %>%
        mutate(adjust_goals = ifelse(
                diff_z_goals > (sd_diff_z_goals * -1) & diff_z_goals < sd_diff_z_goals,
                FALSE,
                TRUE),
               adjust_assists = ifelse(
                       diff_z_assists > (sd_diff_z_assists * -1) & diff_z_assists < sd_diff_z_assists,
                       FALSE,
                       TRUE))

# Identify skaters with multi-team history

skater_teams_last_season <- select(gl_data_1, 
                                   player_id, 
                                   team_name) %>%
        group_by(player_id) %>%
        unique() %>%
        ungroup()

multiple_teams_last_season <- skater_teams_last_season %>%
        group_by(player_id) %>%
        summarise(teams = n()) %>%
        filter(teams > 1) %>%
        ungroup()
multiple_teams_last_season <- multiple_teams_last_season$player_id

skater_current_team <- team_rosters %>%
        select(player_id,
               team_name = team)

new_team <- setdiff(skater_current_team, skater_teams_last_season)  
new_team <- filter(new_team, player_id %in% filtered_skaters) 
new_team <- new_team$player_id

multiple_teams <- c(multiple_teams_last_season, new_team)

# Add multi-team data to skater projections

raw_skater_projections$multi_team <- ifelse(raw_skater_projections$player_id %in% multiple_teams, TRUE, FALSE)

# Adjust goals based on environment
# Use standard deviation of adj_goals to make adjustment for qualifying skaters
# Note(1): standard deviation is computed separately for F/D
# Note(2): adjustments are larger for skaters with multi-team history

sd_adj_goals <- raw_skater_projections %>%
        group_by(position) %>%
        summarize(sd = sd(adj_goals)) %>%
        ungroup()
sd_adj_goals_f <- as.numeric(sd_adj_goals[2,2])
sd_adj_goals_d <- as.numeric(sd_adj_goals[1,2])

raw_skater_projections <- raw_skater_projections %>%
        mutate(p_goals = case_when(
                adjust_goals == TRUE & 
                        multi_team == TRUE &
                        position == "F" ~ adj_goals + (o_u_sd_goals * sd_adj_goals_f * 0.6),
                adjust_goals == TRUE & 
                        multi_team == TRUE &
                        position == "D" ~ adj_goals + (o_u_sd_goals * sd_adj_goals_d * 0.6),
                adjust_goals == TRUE & 
                        multi_team == FALSE &
                        position == "F" ~ adj_goals + (o_u_sd_goals * sd_adj_goals_f * 0.2),
                adjust_goals == TRUE & 
                        multi_team == FALSE &
                        position == "D" ~ adj_goals + (o_u_sd_goals * sd_adj_goals_d * 0.2),
                TRUE ~ adj_goals))

explore_environment_adj_goals <- raw_skater_projections %>%
        select(player,
               team,
               p_toi, 
               adj_goals, 
               p_goals) %>%
        mutate(unadjusted_goals_82 = adj_goals * p_toi * 82,
               enviro_adjusted_goals_82 = p_goals * p_toi * 82,
               diff = enviro_adjusted_goals_82 - unadjusted_goals_82)

# Repeat the above process to adjust assists

sd_adj_assists <- raw_skater_projections %>%
        group_by(position) %>%
        summarize(sd = sd(adj_assists)) %>%
        ungroup()
sd_adj_assists_f <- as.numeric(sd_adj_assists[2,2])
sd_adj_assists_d <- as.numeric(sd_adj_assists[1,2])

raw_skater_projections <- raw_skater_projections %>%
        mutate(p_assists = case_when(
                adjust_assists == TRUE & 
                        multi_team == TRUE &
                        position == "F" ~ adj_assists + (o_u_sd_assists * sd_adj_assists_f * 0.6),
                adjust_assists == TRUE & 
                        multi_team == TRUE &
                        position == "D" ~ adj_assists + (o_u_sd_assists * sd_adj_assists_d * 0.6),
                adjust_assists == TRUE & 
                        multi_team == FALSE &
                        position == "F" ~ adj_assists + (o_u_sd_assists * sd_adj_assists_f * 0.2),
                adjust_assists == TRUE & 
                        multi_team == FALSE &
                        position == "D" ~ adj_assists + (o_u_sd_assists * sd_adj_assists_d * 0.2),
                TRUE ~ adj_assists))

explore_environment_adj_assists <- raw_skater_projections %>%
        select(player,
               team,
               p_toi, 
               adj_assists, 
               p_assists) %>%
        mutate(unadjusted_assists_82 = adj_assists * p_toi * 82,
               enviro_adjusted_assists_82 = p_assists * p_toi * 82,
               diff = enviro_adjusted_assists_82 - unadjusted_assists_82)

# SKATER PROJECTIONS ###########################################################

# Select target data

skater_projections <- select(raw_skater_projections, c(1,2,4:7,15,33,34,18:20))

##### Skater projections: add stats per GP and per 82 GP

skater_projections <- mutate(skater_projections, 
                             goals_gp = p_toi * p_goals,
                             assists_gp = p_toi * p_assists,
                             shots_gp = p_toi * p_shots,
                             hits_gp = p_toi * p_hits,
                             blocks_gp = p_toi * p_blocks,
                             goals_82 = round(goals_gp * 82),
                             assists_82 = round(assists_gp * 82),
                             shots_82 = round(shots_gp * 82),
                             hits_82 = round(hits_gp * 82),
                             blocks_82 = round(blocks_gp * 82),
                             points_82 = goals_82 + assists_82) %>%
        arrange(desc(points_82))

##### Skater projections: add 2022-2023 stats (82 game pace)

skater_2022_2023_pace_data <- raw_game_logs_data %>%
        filter(date > "2022-10-01") %>%
        group_by(player_id) %>%
        summarise(gp = n(),
                  goals = sum(goals),
                  assists = sum(assists),
                  points = sum(points))

skater_2022_2023_pace_data <- mutate(skater_2022_2023_pace_data,
                                     goals_22_23 = round((goals / gp) * 82),
                                     assists_22_23 = round((assists / gp) * 82),
                                     points_22_23 = goals_22_23 + assists_22_23)

skater_2022_2023_pace_data <- select(skater_2022_2023_pace_data, player_id,
                                     goals_22_23,
                                     assists_22_23,
                                     points_22_23)

skater_projections <- left_join(skater_projections, skater_2022_2023_pace_data, by = "player_id") 
skater_projections$points_diff <- skater_projections$points_82 - skater_projections$points_22_23

##### Skater projections: filter for top 400 skaters based on points

skater_projections <- slice_head(skater_projections, n = 400)

# EXPORT #######################################################################

# Save skater projections locally

# write_rds(skater_projections, "skater_projections_2023_2024.RDS")


# Push to a Google Sheet

#install.packages("googledrive")
#install.packages("googlesheets4")

library(googledrive)
library(googlesheets4)

# Create a new spreadsheet in Google and then paste the url below
# You can change the name of the sheet that will be pushed to Google

your_google_sheet_url <- "" # <<< Paste your url between the quotation marks 

name_of_sheet <- "18_skaters_skater_projections"

sheet_write(skater_projections, 
            ss = your_google_sheet_url, 
            sheet = name_of_sheet)

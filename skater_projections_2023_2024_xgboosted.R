# INTRODUCTION #################################################################

# This script generates skater projections for the 2023-2024 NHL season
# The projected stats are goals, assists, shots, hits, and blocks
# Note: projections will change a little as team rosters change in the off-season
# Raw data are pulled directly from the NHL's API
# This script includes a simple expected goals model

# TABLE OF CONTENTS ############################################################

# Introduction
# Table Of Contents
# Setup
# NHL API Functions
# Helper Functions
# Get The Raw Data
# Add xGoals To Play-By-Play Data
# Get Dates For 160/80/40 Team Games
# Process Game Logs
# Filter For Qualifying Skaters
# Get on-ice xGoals data
# Projected Goals (Raw)
# Projected Assists (Raw)
# Projected Shots/Hits/Blocks (Raw)
# Projected TOI (Raw)
# Assemble Raw Skater Projections
# Adjustments To Skater Projections
# Skater Projections
# Export

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

##### NHL API Functions: get_team_rosters()

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

##### NHL API Functions: get_schedule(start_date, end_date, reg_szn = "TRUE")

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

##### NHL API Functions: get_shift_data(game_id)

get_shift_data <- function(game_id) {
        
        # Pull and unpack the shifts charts
        
        pbp_site <- read_json(paste0("https://api.nhle.com/stats/rest/en/shiftcharts?cayenneExp=gameId=", game_id))
        
        data <- pbp_site$data %>%
                tibble() %>%
                unnest_wider(1) %>%
                arrange(period, startTime)
        
        # Changes times from strings to seconds
        
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
        
        data <- select(data, "player_id" = playerId, 
                       shift_start,
                       shift_end,
                       duration,
                       "team_id" = teamId) %>%
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
        
        # Get on-ice data (shift end indicated at shift start time)
        
        on_ice_data <- pivot_wider(data = data, 
                                   names_from = player_id,
                                   values_from = shift_end, 
                                   id_cols = c(game_seconds),
                                   names_prefix = "player_id_")
        
        # Join to full game clock
        
        full_game_data <- full_game_clock %>%
                left_join(on_ice_data, by = "game_seconds")
        
        # Fill columns with shift end times
        
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

##### NHL API Functions: get_play_by_play_data(game_id)

get_play_by_play_data <- function(game_id) {
        
        pbp_site <- read_json(paste0("https://statsapi.web.nhl.com/api/v1/game/", game_id, "/feed/live"))
        
        # Get basic game data 
        
        game_data <- pbp_site$gameData %>%
                tibble() %>%
                unnest_wider(1) %>%
                select(game_id = pk, season, season_type = type) %>%
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
                       position = type)
        
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
        
        # Rearrange columns
        
        players <- select(players, c(1:3, length(players), 4:(length(players) -1)))
        
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
        
        
        # Bolt on the shifts data
        
        on_ice_data <- get_shift_data(game_id)
        
        pbp_data <- pbp_data %>%
                left_join(on_ice_data, by = c("game_id", "game_seconds"))
        
        return(pbp_data)
}

##### NHL API Functions: get_game_logs(player_id, season)

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

##### Helper functions: convert_gl_to_rate_stats(game_logs)

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

##### Helper functions: get_regression_data_goals(pbp_data, start_date)

get_regression_data_goals <- function(pbp_data, start_date) {
        
        working_data <- pbp_data
        
        # Filter data based on start_date
        
        working_data <- filter(working_data, date >= start_date)
        
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
        
        sa_dangerous_variable <- working_data %>%
                filter(sa_dangerous == TRUE) %>%
                group_by(event_player_1_id) %>%
                summarise(sa_dangerous = n())
        
        shots_variable <- working_data %>%
                filter(event_type == "GOAL" | event_type == "SHOT") %>%
                group_by(event_player_1_id) %>%
                summarise(shots = n())
        
        # Join and clean regression variables
        
        goals_variables <- shots_variable %>%
                full_join(sa_dangerous_variable, by = "event_player_1_id") %>%
                left_join(goals_variable, by = "event_player_1_id") %>%
                left_join(xg_variable, by = "event_player_1_id") 
                
        goals_variables <- select(goals_variables, 
                                  event_player_1_id, 
                                  goals, 
                                  xg, 
                                  sa_dangerous, 
                                  shots) 
        
        goals_variables$goals[is.na(goals_variables$goals)] <- 0
        goals_variables$xg[is.na(goals_variables$xg)] <- 0
        goals_variables$sa_dangerous[is.na(goals_variables$sa_dangerous)] <- 0
        goals_variables$shots[is.na(goals_variables$shots)] <- 0
        
        # Add composite variables
        # Note: proportion of sa_dangerous that turned into goals was 0.152
        
        goals_variables$goal_score_1 <- goals_variables$goals + goals_variables$xg
        goals_variables$goal_score_2 <- goals_variables$goals + (goals_variables$sa_dangerous * 0.152)
        goals_variables$shot_score <- goals_variables$sa_dangerous + goals_variables$xg
        
        names(goals_variables)[1] <- "player_id"
        
        goals_variables <- arrange(goals_variables, desc(goals))
        
        return(goals_variables)
}

##### Helper functions: get_regression_data_assists(pbp_data, oi_xg_data, start_date)

get_regression_data_assists <- function(pbp_data, oi_xg_data, start_date) {
        
        # Start with oi_xg_data

        oi_xg_working_data <- oi_xg_data
        
        # Filter data based on start_date
        
        oi_xg_working_data <- filter(oi_xg_working_data, date >= start_date)
        
        # Get on-ice xG data
        
        oi_xg_working_data <- oi_xg_working_data %>%
                group_by(player_id) %>%
                summarize(oi_xg = sum(oi_xg))
        
        # Now get the player's xG data from the play-by-play data
        
        working_data <- pbp_data
        
        # Filter data based on start_date
        
        working_data <- filter(working_data, date >= start_date)
        
        # Remove empty net and shootouts
        
        working_data <- filter(working_data, goalie_id > 0,
                               period != 5)
        
        # Get player's xG data
        
        player_xG_data <- working_data %>%
                filter(event_type == "SHOT" | event_type == "GOAL") %>%
                group_by(event_player_1_id) %>%
                summarize(xg = sum(xg)) %>%
                rename("player_id" = event_player_1_id)
        
        # Join player xG data to on-ice xG data
        
        oi_xg_working_data <- oi_xg_working_data %>%
                left_join(player_xG_data, by = "player_id")
        
        # Get on-ice xG data that excludes the player's contribution
        
        oi_xg_working_data <- oi_xg_working_data %>%
                mutate(oi_xg_ex_player = oi_xg - xg) %>%
                select(player_id, oi_xg_ex_player)
        
        # Get remaining regression variables
        
        a1_data <- filter(working_data, event_player_2_type == "Assist") %>%
                group_by(event_player_2_id) %>%
                summarise(primary_assists = n()) 
        names(a1_data)[1] <- "player_id"
        
        a2_data <- filter(working_data, event_player_3_type == "Assist") %>%
                group_by(event_player_3_id) %>%
                summarise(secondary_assists = n()) 
        names(a2_data)[1] <- "player_id"
        
        a1_xg_data <- filter(working_data, event_player_2_type == "Assist") %>%
                group_by(event_player_2_id) %>%
                summarise(xg_a1 = sum(xg)) 
        names(a1_xg_data)[1] <- "player_id"
        
        a2_xg_data <- filter(working_data, event_player_3_type == "Assist") %>%
                group_by(event_player_3_id) %>%
                summarise(xg_a2 = sum(xg)) 
        names(a2_xg_data)[1] <- "player_id"
        
        player_position <- team_rosters %>%
                select(player_id,
                       position) %>%
                mutate(position = ifelse(
                        position == "F",
                        1,
                        0))
        
        shots_data <- working_data %>%
                filter(event_type == "SHOT" | event_type == "GOAL") %>%
                group_by(event_player_1_id) %>%
                summarize(shots = n()) %>%
                rename("player_id" = event_player_1_id)
        
        # Join and clean regression variables
        
        assists_variables <- a1_data %>%
                full_join(a2_data, by = "player_id") %>%
                full_join(a1_xg_data, by = "player_id") %>%
                full_join(a2_xg_data, by = "player_id") %>%
                left_join(player_position, by = "player_id") %>%
                left_join(oi_xg_working_data, by = "player_id") %>%
                left_join(shots_data, by = "player_id")
        
        assists_variables <- mutate(assists_variables, 
                total_assists = primary_assists + secondary_assists,
                proportion_primary = primary_assists / total_assists)
        
        assists_variables <- select(assists_variables, player_id, 
                                    total_assists, 
                                    xg_a1, 
                                    xg_a2,
                                    proportion_primary,
                                    position,
                                    oi_xg_ex_player,
                                    shots) 
        
        assists_variables$total_assists[is.na(assists_variables$total_assists)] <- 0
        assists_variables$xg_a1[is.na(assists_variables$xg_a1)] <- 0
        assists_variables$xg_a2[is.na(assists_variables$xg_a2)] <- 0
        assists_variables$proportion_primary[is.na(assists_variables$proportion_primary)] <- 0
        
        assists_variables <- filter(assists_variables, !is.na(position))
        
        assists_variables <- arrange(assists_variables, desc(total_assists))
        
        # Filter out players who have no on-ice xG data (effectively, limiting the pool to players in the filtered skaters list)
        
        assists_variables <- filter(assists_variables, oi_xg_ex_player > 0)
        
        # Consider expanding all the data used in this model in the future
        
        return(assists_variables)
}

##### Helper functions: predict_goals(reg_data)

predict_goals <- function(reg_data) {
        
        working_data <- reg_data
        
        # Get regression coefficients
        
        y_int <- coef(reg_goals_model)[1]
        coef_goal_score_1 <- coef(reg_goals_model)[2]
        
        # Add coefficients to working data
        
        working_data$y_int <- y_int
        working_data$coef_goal_score_1 <- coef_goal_score_1
        
        # Predict goals
        
        working_data <- mutate(working_data, lm_goals =
                                       y_int +
                                       (goal_score_1 * coef_goal_score_1))
        
        # Blend predicted goals and actual goals 90/10
        
        working_data <- mutate(working_data, p_goals_ex_en = 
                                       (lm_goals * 0.9) + (goals * 0.1))
        
        # Filter for qualifying skaters
        
        working_data <- filter(working_data, player_id %in% filtered_skaters)
        
        # Adjust negative goals to zero
        
        working_data <- mutate(working_data, p_goals_ex_en = ifelse(p_goals_ex_en < 0, 0, p_goals_ex_en))
        
        return(working_data)
}

##### Helper functions: predict_assists(reg_data)

predict_assists <- function(reg_data) {
        
        working_data <- reg_data
        
        prediction_data <- select(working_data, c(3:length(working_data)))

        predicted_assists <- predict(xgb_model_assists,
                                     data.matrix(prediction_data))
        
        working_data$xgb_assists <- predicted_assists
        
        # Blend predicted assists and actual assists 90/10
        
        working_data <- mutate(working_data, p_assists_ex_en = 
                                       (xgb_assists * 0.9) + (total_assists * 0.1))
        
        # Filter for qualifying skaters
        
        working_data <- filter(working_data, player_id %in% filtered_skaters)
        
        # Adjust negative assists to zero
        
        working_data <- mutate(working_data, p_assists_ex_en = ifelse(p_assists_ex_en < 0, 0, p_assists_ex_en))
        
        return(working_data)
}

##### Helper functions: add_empty_net_goals(reg_data, pbp_data, gp_date)

add_empty_net_goals <- function(reg_data, pbp_data, gp_date) {
        
        working_data <- pbp_data
        
        # Filter working_data based on gp_date
        
        working_data <- filter(working_data, date >= gp_date)
        
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

##### Helper functions: add_empty_net_assists(reg_data, pbp_data, gp_date)

add_empty_net_assists <- function(reg_data, pbp_data, gp_date) {
        
        working_data <- pbp_data
        
        # Filter working_data based on gp_date
        
        working_data <- filter(working_data, date >= gp_date)
        
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
                summarise(en_assists_1 = n()) 
        names(skater_en_assists_1)[1] <- "player_id"
        
        skater_en_assists_2 <- working_data %>%
                group_by(event_player_3_id) %>%
                summarise(en_assists_2 = n())
        names(skater_en_assists_2)[1] <- "player_id"
        
        skater_en_assists <- skater_en_assists_1 %>%
                full_join(skater_en_assists_2, by = "player_id")
        
        skater_en_assists$en_assists_1[is.na(skater_en_assists$en_assists_1)] <- 0
        skater_en_assists$en_assists_2[is.na(skater_en_assists$en_assists_2)] <- 0
        
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

##### Helper functions: convert_pred_to_rate_stats(pred_data, rate_stats, stat, gp)

convert_pred_to_rate_stats <- function(pred_data, rate_stats, stat, gp) {
        
        working_data <- rate_stats
        
        stat_column <- colnames(pred_data)[2]
        
        # Get total time-on-ice data for GP period
        
        toi_data <- select(working_data, player_id, toi_total)
        
        # Join the data
        
        pred_data <- left_join(pred_data, toi_data, by = "player_id")
        
        # Compute rate stats
        
        pred_data <- mutate(pred_data, x = get(stat_column) / toi_total)
        
        names(pred_data)[4] <- paste0("p_", stat, "_", gp, "_rate")
        
        return(pred_data)
}

##### Helper functions: add_xg_data(training_data, pbp_data)

add_xg_data <- function(training_data, pbp_data) {
        
        # Build an xG model using the training data
        
        working_data <- training_data
        
        # Change event_player_1_position from F/D to 1/0
        
        working_data$event_player_1_position <- ifelse(
                working_data$event_player_1_position == "F",
                1,
                0)
        
        # Add prior event outside o-zone
        
        working_data <- working_data %>%
                group_by(game_id, period) %>%
                mutate(pe_x_ozone = case_when(
                        event_team == home_team &
                                lag(x_fixed) < 25 ~ 1,
                        event_team == away_team & 
                                lag(x_fixed) > -25 ~ 1,
                        TRUE ~ 0)) %>%
                ungroup()
        
        # Add prior event giveaway / takeaway
        
        working_data <- working_data %>%
                group_by(game_id, period) %>%
                mutate(turnover = case_when(
                        event_team == lag(event_team) &
                                game_seconds - lag(game_seconds) < 6 &
                                lag(event_type ) == "TAKEAWAY" ~ 1,
                        event_team != lag(event_team) &
                                game_seconds - lag(game_seconds) < 6 &
                                lag(event_type ) == "GIVEAWAY" ~ 1,
                        TRUE ~ 0)) %>%
                ungroup()
        
        # Add elapsed time from prior event
        
        working_data <- working_data %>%
                group_by(game_id, period) %>%
                mutate(elapsed_time = game_seconds - lag(game_seconds)) %>%
                ungroup()
        
        # Filter out non-shots, empty nets, shootout
        
        working_data <- working_data %>%
                filter(event_type == "SHOT" | 
                               event_type == "GOAL",
                       goalie_id > 0,
                       period < 5)
        
        # Add juicy rebounds
        
        working_data <- working_data %>%
                mutate(lag_gs = lag(game_seconds, n = 1))
        working_data$lag_gs[is.na(working_data$lag_gs)] <- 0
        
        working_data <- working_data %>%
                mutate(lag_y = lag(y_fixed, n = 1))
        working_data$lag_y[is.na(working_data$lag_y)] <- 0
        
        working_data <- working_data %>%
                mutate(diff_y = (abs(sqrt((y_fixed - lag_y)^2))))
        
        working_data <- working_data %>%
                mutate(juicy_rebound = ifelse(
                        game_seconds - lag_gs < 3 & 
                                diff_y > 9 &
                                sa_distance <= 30 &
                                sa_angle < 90,
                        1,
                        0))
        
        # Add rushes occurring after an opposing shot
        
        working_data <- working_data %>%
                mutate(lag_x = lag(x_fixed, n = 1))
        working_data$lag_x[is.na(working_data$lag_x)] <- 0
        
        working_data <- working_data %>%
                mutate(diff_x = (abs(sqrt((x_fixed - lag_x)^2))))
        
        working_data <- working_data %>%
                mutate(after_shot_rush = ifelse(
                        game_seconds - lag_gs < 7 &
                                period == lag(period, n =1) &
                                diff_x > 60 &
                                sa_distance < 40 &
                                sa_angle < 90,
                        1,
                        0)) 
        
        # Add column for close shot attempts from below the goal line
        
        working_data <- working_data %>%
                mutate(goal_line = ifelse(
                        sa_angle > 90 &
                                sa_distance < 12, 
                        1,
                        0))
        
        # Add column for shot pressure
        
        working_data <- working_data %>%
                mutate(shot_pressure = ifelse(
                        event_team == lag(event_team, n = 1) &
                                event_team == lag(event_team, n = 2) &
                                event_team == lag(event_team, n = 3) &
                                period == lag(period, n = 3),
                        1,
                        0))
        working_data$shot_pressure[is.na(working_data$shot_pressure)] <- 0
        
        # Change goals/shots to 1/0
        
        working_data$event_type <- ifelse(
                working_data$event_type == "GOAL",
                1,
                0)
        
        # Change shot type to wrist / other
        
        working_data$secondary_type <- ifelse(
                working_data$secondary_type == "Wrist Shot",
                1,
                0)
        working_data$secondary_type[is.na(working_data$secondary_type)] <- 0
        
        # Select the training data
        
        working_data_shrunk <- working_data %>%
                select(event_type,
                       secondary_type,
                       sa_distance,
                       sa_angle,
                       juicy_rebound,
                       after_shot_rush,
                       goal_line,
                       shot_pressure,
                       pe_x_ozone,
                       turnover,
                       elapsed_time,
                       event_player_1_position)
        
        # Eliminate cases where there are missing coordinates
        
        working_data_shrunk <- working_data_shrunk %>%
                filter(sa_distance >= 0)
        
        train_split <- sample.split(Y = working_data_shrunk$event_type, 
                                    SplitRatio = 0.7)
        
        train_data <- working_data_shrunk[train_split,]
        
        test_data <- working_data_shrunk[!train_split,]
        
        # Prep training data
        
        train_variables <- data.matrix(train_data[, -1])
        
        train_goals <- data.matrix(train_data[, 1])
        
        # Prep testing data
        
        test_variables <- data.matrix(test_data[, -1])
        
        test_goals <- data.matrix(test_data[, 1])
        
        # Final training and testing data sets
        
        xgb_train <- xgb.DMatrix(data = train_variables, 
                                 label = train_goals)
        
        xgb_test <- xgb.DMatrix(data = test_variables, 
                                label = test_goals)
        
        # Get nrounds parameter
        
        watchlist <- list(train = xgb_train, 
                          test = xgb_test)
        
        set.seed(28)
        
        cv <- xgb.cv(params = list(objective = "binary:logistic", 
                                   eval_metric = "auc",
                                   max.depth = 6),
                     data = xgb_train,
                     watchlist = watchlist,
                     nrounds = 100,
                     nfold = 5, 
                     early_stopping_rounds = 20)
        
        # The XGBoost Model
        
        final_xg_model <- xgb.train(data = xgb_train,
                                    watchlist = watchlist,
                                    nrounds = cv$best_iteration,
                                    max.depth = 6, 
                                    objective = "binary:logistic",
                                    eval_metric = "auc",
                                    verbose = 0)
        
        # Conform play-by-play data to xg_model data 
        # Add a temporary event_id for joining data
        
        return_data <- mutate(pbp_data, temp_id = paste0(game_id, event_id))
        
        pbp_prediction_data <- mutate(pbp_data, temp_id = paste0(game_id, event_id))
        
        pbp_prediction_data$event_player_1_position <- ifelse(
                pbp_prediction_data$event_player_1_position == "F",
                1,
                0)
        
        # Add prior event outside o-zone
        
        pbp_prediction_data <- pbp_prediction_data %>%
                group_by(game_id, period) %>%
                mutate(pe_x_ozone = case_when(
                        event_team == home_team &
                                lag(x_fixed) < 25 ~ 1,
                        event_team == away_team & 
                                lag(x_fixed) > -25 ~ 1,
                        TRUE ~ 0)) %>%
                ungroup()
        
        # Add prior event giveaway / takeaway
        
        pbp_prediction_data<- pbp_prediction_data %>%
                group_by(game_id, period) %>%
                mutate(turnover = case_when(
                        event_team == lag(event_team) &
                                game_seconds - lag(game_seconds) < 6 &
                                lag(event_type ) == "TAKEAWAY" ~ 1,
                        event_team != lag(event_team) &
                                game_seconds - lag(game_seconds) < 6 &
                                lag(event_type ) == "GIVEAWAY" ~ 1,
                        TRUE ~ 0)) %>%
                ungroup()
        
        # Add elapsed time from prior event
        
        pbp_prediction_data <- pbp_prediction_data %>%
                group_by(game_id, period) %>%
                mutate(elapsed_time = game_seconds - lag(game_seconds)) %>%
                ungroup()
        
        # Filter out non-shots, empty nets, shootout
        
        pbp_prediction_data <- pbp_prediction_data %>%
                filter(event_type == "SHOT" | 
                               event_type == "GOAL",
                       goalie_id > 0,
                       period < 5)
        
        # Add juicy rebounds
        
        pbp_prediction_data <- pbp_prediction_data %>%
                mutate(lag_gs = lag(game_seconds, n = 1))
        pbp_prediction_data$lag_gs[is.na(pbp_prediction_data$lag_gs)] <- 0
        
        pbp_prediction_data <- pbp_prediction_data %>%
                mutate(lag_y = lag(y_fixed, n = 1))
        pbp_prediction_data$lag_y[is.na(pbp_prediction_data$lag_y)] <- 0
        
        pbp_prediction_data <- pbp_prediction_data %>%
                mutate(diff_y = (abs(sqrt((y_fixed - lag_y)^2))))
        
        pbp_prediction_data <- pbp_prediction_data %>%
                mutate(juicy_rebound = ifelse(
                        game_seconds - lag_gs < 3 & 
                                diff_y > 9 &
                                sa_distance <= 30 &
                                sa_angle < 90,
                        1,
                        0))
        
        # Add rushes occurring after an opposing shot
        
        pbp_prediction_data <- pbp_prediction_data %>%
                mutate(lag_x = lag(x_fixed, n = 1))
        pbp_prediction_data$lag_x[is.na(pbp_prediction_data$lag_x)] <- 0
        
        pbp_prediction_data <- pbp_prediction_data %>%
                mutate(diff_x = (abs(sqrt((x_fixed - lag_x)^2))))
        
        pbp_prediction_data <- pbp_prediction_data %>%
                mutate(after_shot_rush = ifelse(
                        game_seconds - lag_gs < 7 &
                                period == lag(period, n =1) &
                                diff_x > 60 &
                                sa_distance < 40 &
                                sa_angle < 90,
                        1,
                        0)) 
        
        # Add column for close shot attempts from below the goal line
        
        pbp_prediction_data <- pbp_prediction_data %>%
                mutate(goal_line = ifelse(
                        sa_angle > 90 &
                                sa_distance < 12, 
                        1,
                        0))
        
        # Add column for shot pressure
        
        pbp_prediction_data <- pbp_prediction_data %>%
                mutate(shot_pressure = ifelse(
                        event_team == lag(event_team, n = 1) &
                                event_team == lag(event_team, n = 2) &
                                event_team == lag(event_team, n = 3) &
                                period == lag(period, n = 3),
                        1,
                        0))
        pbp_prediction_data$shot_pressure[is.na(pbp_prediction_data$shot_pressure)] <- 0
        
        # Change goals/shots to 1/0
        
        pbp_prediction_data$event_type <- ifelse(
                pbp_prediction_data$event_type == "GOAL",
                1,
                0)
        
        # Change shot type to wrist / other
        
        pbp_prediction_data$secondary_type <- ifelse(
                pbp_prediction_data$secondary_type == "Wrist Shot",
                1,
                0)
        pbp_prediction_data$secondary_type[is.na(pbp_prediction_data$secondary_type)] <- 0
        
        # Select the necessary prediction data
        
        pbp_prediction_data_shrunk <- pbp_prediction_data %>%
                select(event_type,
                       secondary_type,
                       sa_distance,
                       sa_angle,
                       juicy_rebound,
                       after_shot_rush,
                       goal_line,
                       shot_pressure,
                       pe_x_ozone,
                       turnover,
                       elapsed_time,
                       event_player_1_position)
        
        # Put shrunk data in matrix form
        
        variables_matrix <- data.matrix(pbp_prediction_data_shrunk[, -1])
        
        goals_matrix <- data.matrix(pbp_prediction_data_shrunk[, 1])
        
        pbp_prediction_data_matrix <- xgb.DMatrix(data = variables_matrix, 
                                                  label = goals_matrix)
        
        # Make the predictions
        
        predicted_xg <- predict(final_xg_model, pbp_prediction_data_matrix)
        
        # Add the predictions to the play-by_play prediction data
        
        pbp_prediction_data$xg <- predicted_xg
        
        # Isolate temp_id and xg for join back to main data
        
        pbp_prediction_data <- select(pbp_prediction_data,
                                      temp_id,
                                      xg)
        
        # Add xg to return data
        
        return_data <- return_data %>%
                left_join(pbp_prediction_data, by = "temp_id")
        
        # Remove temp-id
        
        return_data <- select(return_data, -temp_id)
        
        return(return_data)
}

# GET THE RAW DATA #############################################################

##### Get the raw data: rosters

raw_team_rosters <- get_team_rosters()

# Clean and filter position data (remove goalies)

team_rosters <- filter(raw_team_rosters, position != "G") %>%
        mutate(position = ifelse(position == "D", "D", "F")) %>%
        select(c(1:7))

##### Get the raw data: schedule

# Pull the regular season schedule for 2021-2022 and 2022-2023
# This should pull 2624 observations

raw_schedule <- get_schedule("2021-10-01", "2023-04-15", reg_szn = "TRUE")

# Clean and add team games played to schedule data (counting from most recent) 

schedule_home <- raw_schedule %>%
        select(-away_team, 
               -away_team_id, 
               -home_team_id) %>%
        rename(full_team_name = home_team)

schedule_away <- raw_schedule %>%
        select(-home_team, 
               -home_team_id, 
               -away_team_id) %>%
        rename(full_team_name = away_team)

schedule <- bind_rows(schedule_home, schedule_away) %>%
        arrange(desc(date))

schedule <- schedule %>%
        group_by(full_team_name) %>%
        mutate(team_gp = row_number()) %>%
        ungroup()

##### Get the raw data: play-by-play

# Load the data after saving locally (below) 
# raw_pbp_data <- read_rds("raw_pbp_oi_data_2021_2023.RDS")
# raw_pbp_data_xg_data <- read_rds("raw_pbp_oi_data_2018_2020.RDS")
# Now skip these loops and go to "Get the raw data: remove (potential) errors in raw play-by-play data"

# WARNING: THIS CODE TAKES SOME TIME TO RUN

# Take the game_ids from the schedule data and loop them through the pbp function

game_ids <- raw_schedule$game_id

temp_pbp_list <- list()

for (i in 1:length(game_ids)) {
        
        pbp_data <- get_play_by_play_data(game_ids[i])
        
        temp_pbp_list[[i]] <- pbp_data
}

raw_pbp_data <- bind_rows(temp_pbp_list) %>%
        arrange(date, game_id, game_seconds)

# Save data locally after running the code
# write_rds(raw_pbp_data, "raw_pbp_data_2021_2023.RDS")

# Get two extra years of data for xG model (skip COVID year)

schedule_xg_data <- get_schedule("2018-10-01", "2020-03-13", reg_szn = "TRUE")

game_ids_xg_data <- schedule_xg_data$game_id

temp_pbp_list <- list()

for (i in 1:length(game_ids_xg_data)) {
        
        pbp_data <- get_play_by_play_data(game_ids_xg_data[i])
        
        temp_pbp_list[[i]] <- pbp_data
}

raw_pbp_data_xg_data <- bind_rows(temp_pbp_list) %>%
        arrange(date, game_id, game_seconds)

# Save data locally after running the code
# write_rds(raw_pbp_data_xg_data, "raw_pbp_data_2018_2020.RDS")

##### Get the raw data: remove (potential) errors in raw play-by-play data

# Remove apparent x_errors

raw_pbp_data$goal_x_error[is.na(raw_pbp_data$goal_x_error)] <- FALSE
raw_pbp_data$shot_x_error[is.na(raw_pbp_data$shot_x_error)] <- FALSE
raw_pbp_data_xg_data$goal_x_error[is.na(raw_pbp_data_xg_data$goal_x_error)] <- FALSE
raw_pbp_data_xg_data$shot_x_error[is.na(raw_pbp_data_xg_data$shot_x_error)] <- FALSE

raw_pbp_data <- raw_pbp_data %>%
        filter(goal_x_error == FALSE,
               shot_x_error == FALSE)

raw_pbp_data_xg_data <- raw_pbp_data_xg_data %>%
        filter(goal_x_error == FALSE,
               shot_x_error == FALSE)

# Fill shot type as "Wrist Shot" where data is NA

raw_pbp_data <- mutate(raw_pbp_data, secondary_type = ifelse(
        (event_type == "SHOT" | event_type == "GOAL") &
                is.na(secondary_type),
        "Wrist Shot",
        secondary_type))

raw_pbp_data_xg_data <- mutate(raw_pbp_data_xg_data, secondary_type = ifelse(
        (event_type == "SHOT" | event_type == "GOAL") &
                is.na(secondary_type),
        "Wrist Shot",
        secondary_type))

##### Get the raw data: game logs

# Load the data after saving locally (below) 
# raw_game_logs_data <- read_rds("raw_game_logs_data_2021_2023.RDS")
# Now skip to "ADD xGOALS TO PLAY-BY-PLAY DATA" 

# WARNING: THIS CODE TAKES SOME TIME TO RUN

# Identify active skaters then loop the player_ids through the gl function

active_2022_2023 <- filter(raw_pbp_data, season == 20222023)
active_2022_2023 <- unique(active_2022_2023$event_player_1_id)
active_2021_2022 <- filter(raw_pbp_data, season == 20212022)
active_2021_2022 <- unique(active_2021_2022$event_player_1_id)

roster_skaters <- team_rosters$player_id

player_ids <- intersect(roster_skaters, active_2022_2023)
player_ids <- intersect(player_ids, active_2021_2022)

temp_gl_list <- list()

for (i in 1:length(player_ids)) {
        
        game_logs <- get_game_logs(player_ids[i], 20222023)
        
        temp_gl_list[[i]] <- game_logs
}

gl_2022_2023 <- bind_rows(temp_gl_list) 

temp_gl_list <- list()

for (i in 1:length(player_ids)) {
        
        game_logs <- get_game_logs(player_ids[i], 20212022)
        
        temp_gl_list[[i]] <- game_logs
}

gl_2021_2022 <- bind_rows(temp_gl_list) 

raw_game_logs_data <- bind_rows(gl_2022_2023, gl_2021_2022) %>%
        arrange(desc(date))

raw_game_logs_data <- unique(raw_game_logs_data)

# Save data locally after running the code
# write_rds(raw_game_logs_data, "raw_game_logs_data_2021_2023.RDS")

# ADD xGOALS TO PLAY-BY-PLAY DATA ##############################################

# Expected goals are computed with an XGBoost model (logistic regression)
# The model is found in the helper function above

training_data <- bind_rows(raw_pbp_data, raw_pbp_data_xg_data) %>%
        arrange(desc(date))

pbp_data <- add_xg_data(training_data, raw_pbp_data)

# Tidy the data

pbp_data <- select(pbp_data, c(1:29, 120, 30:84, 101, 119, 86:100, 102:118, 85))

# GET DATES FOR 160/80/40 TEAM GAMES ###########################################

date_40_gp <- schedule %>%
        filter(team_gp == 40)
date_40_gp <- date_40_gp[16,4]
date_40_gp <- date_40_gp$date

date_80_gp <- schedule %>%
        filter(team_gp == 80)
date_80_gp <- date_80_gp[16,4]
date_80_gp <- date_80_gp$date

date_160_gp <- schedule %>%
        filter(team_gp == 160)
date_160_gp <- date_160_gp[16,4]
date_160_gp <- date_160_gp$date

# PROCESS GAME LOGS ############################################################

###### Process game logs: group game logs based on number of games

gl_data_160 <- raw_game_logs_data %>%
        filter(date >= date_160_gp)

gl_data_80 <- raw_game_logs_data %>%
        filter(date >= date_80_gp)

gl_data_40 <- raw_game_logs_data %>%
        filter(date >= date_40_gp)

###### Process game logs: convert game logs to rate stats (per second rates)

rate_stats_160 <- convert_gl_to_rate_stats(gl_data_160)

rate_stats_80 <- convert_gl_to_rate_stats(gl_data_80)

rate_stats_40 <- convert_gl_to_rate_stats(gl_data_40)

# FILTER FOR QUALIFYING SKATERS ################################################

##### Filter for qualifying skaters: set minimum GP

filter_160gp = 100
filter_40gp = 20

##### Filter for qualifying skaters: apply GP filter

skaters_160_gp <- rate_stats_160 %>%
        filter(gp >= filter_160gp) %>%
        select(player_id)

skaters_40_gp <- rate_stats_40 %>%
        filter(gp >= filter_40gp) %>%
        select(player_id)

##### Filter for qualifying skaters: master list (vector) of qualifying skaters

filtered_skaters <- inner_join(skaters_40_gp, skaters_160_gp, by = "player_id")
filtered_skaters <- filtered_skaters$player_id 

##### Filter for qualifying skaters: basic skater projections data

projections_base <- team_rosters %>%
        filter(player_id %in% filtered_skaters)

# GET ON-ICE XGOALS DATA ###############################################

# Load the data after saving locally (below) 
# oi_xg_data <- read_rds("oi_xg_data_2021_2023.RDS")
# Now skip to "PROJECTED GOALS (RAW)" 

# WARNING: THIS CODE TAKES SOME TIME TO RUN

###### Get on-ice xGoals data: prepare data

game_logs_data <- raw_game_logs_data %>%
        filter(player_id %in% filtered_skaters)

pbp_data_xg_only <- pbp_data %>%
        filter(event_type == "GOAL" | event_type == "SHOT")

###### Get on-ice xGoals data: loop through play-by-play data

# Loop through the game logs to get player/team data
# Use the game logs data to find on-ice xG data in the play-by-play data

temp_oi_xg_list <- list()

for (i in (1:length(game_logs_data$game_id))) {
        
        # Isolate a game / player in the game logs
        
        gl_xg_working_data <- game_logs_data
        gl_xg_working_data <- gl_xg_working_data[i,]
        
        gl_xg_game_id <- gl_xg_working_data[1,4]
        gl_xg_player_id <- gl_xg_working_data[1,1]
        gl_xg_team_id <- gl_xg_working_data[1,26]
        gl_xg_date <- gl_xg_working_data[1,5]
        
        # Pull that game from the play-by_play data
        
        pbp_oi_xg_working_data <- filter(pbp_data_xg_only, 
                                         game_id %in% gl_xg_game_id)
        
        # Find the player's on-ice xG (for)
        
        pbp_oi_xg_working_data <- pbp_oi_xg_working_data %>%
                filter(event_team_id %in% gl_xg_team_id)
        pbp_oi_xg_working_data <- pbp_oi_xg_working_data %>%
                filter(if_any(everything(), ~ . %in% gl_xg_player_id))
        
        oi_xg <- sum(pbp_oi_xg_working_data$xg)
        
        temp_oi_xg <- tibble(gl_xg_date,
                             gl_xg_game_id,
                             gl_xg_player_id,
                             oi_xg)
        
        temp_oi_xg_list[[i]] <- temp_oi_xg
}

oi_xg_data <- bind_rows(temp_oi_xg_list)
oi_xg_data$oi_xg[is.na(oi_xg_data$oi_xg)] <- 0

# Save data locally after running the code
# write_rds(oi_xg_data, "oi_xg_data_2021_2023.RDS")

# PROJECTED GOALS (RAW) ########################################################

##### Projected goals (raw): regression analysis

# Get data for regression analysis

reg_goals_160 <- get_regression_data_goals(pbp_data, date_160_gp)

# Inspect correlation of regression variables

reg_goals_variables_cor <- cor(reg_goals_160[, c("goals", "xg", "sa_dangerous", "shots", "goal_score_1", "goal_score_2", "shot_score")])

# Generate potential linear regression models

reg_goals_model_1 <- lm(goals ~ goal_score_1, reg_goals_160)
reg_goals_model_2 <- lm(goals ~ goal_score_2, reg_goals_160)
reg_goals_model_3 <- lm(goals ~ shot_score, reg_goals_160)
reg_goals_model_4 <- lm(goals ~ xg, reg_goals_160)
reg_goals_model_5 <- lm(goals ~ xg + sa_dangerous + shots, reg_goals_160)
reg_goals_model_6 <- lm(goals ~ xg + sa_dangerous, reg_goals_160)
reg_goals_model_7 <- lm(goals ~ xg + shots, reg_goals_160)
reg_goals_model_8 <- lm(goals ~ shot_score + shots, reg_goals_160)

# Test the models with Akaike information criterion (AIC)

reg_goals_models <- list(reg_goals_model_1, reg_goals_model_2, reg_goals_model_3, reg_goals_model_4, reg_goals_model_5, reg_goals_model_6, reg_goals_model_7, reg_goals_model_8)
reg_goals_models_names <- c("mod_1", "mod_2", "mod_3", "mod_4", "mod_5", "mod_6", "mod_7", "mod_8")

reg_goals_models_test <- aictab(cand.set = reg_goals_models, modnames = reg_goals_models_names)

# Select regression model

# It's not surprising that goal_score_1 was the winner given that it includes goals in the composite variable
# Predicted goals using this model will account for a skater's xG and this can make a big difference (see Leon Draisaitl (8477934))

reg_goals_model <- reg_goals_model_1

# Check regression model

print(summary(reg_goals_model)[4])
print(summary(reg_goals_model)[9])

##### Projected goals (raw): predicted goals (160 GP)

# Add goals predicted by regression model

reg_goals_160 <- predict_goals(reg_goals_160)

# Add a prediction for empty net goals based on cluster analysis

reg_goals_160 <- add_empty_net_goals(reg_goals_160, pbp_data, date_160_gp)

# Combine predictions and shrink data

p_goals_160 <- reg_goals_160 %>%
        mutate(p_goals_160_count = p_goals_ex_en + cluster_mean) %>%
        select(player_id, p_goals_160_count)

# Convert prediction to a rate stat (per second)

p_goals_160 <- convert_pred_to_rate_stats(p_goals_160, rate_stats_160, "goals", "160")
p_goals_160 <- select(p_goals_160, c(1,4))

##### Projected goals (raw): predicted goals (80 GP)

# Get regression data

reg_goals_80 <- get_regression_data_goals(pbp_data, date_80_gp)

# Add goals predicted by regression model

reg_goals_80 <- predict_goals(reg_goals_80)

# Add a prediction for empty net goals based on cluster analysis

reg_goals_80 <- add_empty_net_goals(reg_goals_80, pbp_data, date_80_gp)

# Combine predictions and shrink data

p_goals_80 <- reg_goals_80 %>%
        mutate(p_goals_80_count = p_goals_ex_en + cluster_mean) %>%
        select(player_id, p_goals_80_count)

# Convert prediction to a rate stat (per second)

p_goals_80 <- convert_pred_to_rate_stats(p_goals_80, rate_stats_80, "goals", "80")
p_goals_80 <- select(p_goals_80, c(1,4))

##### Projected goals (raw): predicted goals (40 GP)

# Get regression data

reg_goals_40 <- get_regression_data_goals(pbp_data, date_40_gp)

# Add goals predicted by regression model

reg_goals_40 <- predict_goals(reg_goals_40)

# Add a prediction for empty net goals based on cluster analysis

reg_goals_40 <- add_empty_net_goals(reg_goals_40, pbp_data, date_40_gp)

# Combine predictions and shrink data

p_goals_40 <- reg_goals_40 %>%
        mutate(p_goals_40_count = p_goals_ex_en + cluster_mean) %>%
        select(player_id, p_goals_40_count)

# Convert prediction to a rate stat (per second)

p_goals_40 <- convert_pred_to_rate_stats(p_goals_40, rate_stats_40, "goals", "40")
p_goals_40 <- select(p_goals_40, c(1,4))

##### Projected goals (raw): join predicted goals data and apply GP weighting

p_goals_raw <- p_goals_160 %>%
        left_join(p_goals_80, by = "player_id") %>%
        left_join(p_goals_40, by = "player_id")

p_goals_raw <- mutate(p_goals_raw, p_goals_raw = (p_goals_160_rate * 0.35) + (p_goals_80_rate * 0.55) + (p_goals_40_rate * 0.1))

p_goals_raw <- select(p_goals_raw, player_id, p_goals_raw)

# PROJECTED ASSISTS (RAW) ######################################################

##### Projected assists (raw): XGBoost regression analysis

# Get data for building regression model

reg_assists_160 <- get_regression_data_assists(pbp_data, oi_xg_data, date_160_gp)

# Remove player_id

reg_assists_160_shrunk <- reg_assists_160[, -1] 

# Split data to train the model

train_split_assists <- sample.split(Y = reg_assists_160_shrunk$total_assists, 
                                    SplitRatio = 0.7)

train_data_assists <- reg_assists_160_shrunk[train_split_assists,]

test_data_assists <- reg_assists_160_shrunk[!train_split_assists,]

# Put training and testing data in a matrix

train_variables_assists <- data.matrix(train_data_assists[, -1])

train_total_assists <- data.matrix(train_data_assists[, 1])

test_variables_assists <- data.matrix(test_data_assists[, -1])

test_total_assists <- data.matrix(test_data_assists[, 1])

# Final training and testing data sets

xgb_train_assists <- xgb.DMatrix(data = train_variables_assists, 
                                 label = train_total_assists)

xgb_test_assists <- xgb.DMatrix(data = test_variables_assists, 
                                label = test_total_assists)

# Get nrounds parameter

watchlist_assists <- list(train = xgb_train_assists, 
                          test = xgb_test_assists)

set.seed(28)

cv_assists <- xgb.cv(params = list(max.depth = 5,
                                   eta = 0.04,
                                   gamma = 0.07),
                     data = xgb_train_assists,
                     watchlist = watchlist_assists,
                     nrounds = 1000,
                     nfold = 5, 
                     early_stopping_rounds = 40)

# The XGBoost Model for assists

xgb_model_assists <- xgb.train(params = list(max.depth = 5,
                                             eta = 0.04,
                                             gamma = 0.07),
                               data = xgb_train_assists,
                               watchlist = watchlist_assists,
                               nrounds = cv_assists$best_iteration)

importance_matrix_assists <- xgb.importance(colnames(xgb_train_assists),
                                            model = xgb_model_assists)
importance_matrix_assists

##### Projected assists (raw): predicted assists (160 GP)

# Add assists predicted by regression model

reg_assists_160 <- predict_assists(reg_assists_160)

# Add a prediction for empty net assists based on cluster analysis

reg_assists_160 <- add_empty_net_assists(reg_assists_160, pbp_data, date_160_gp)

# Combine predictions and shrink data

p_assists_160 <- reg_assists_160 %>%
        mutate(p_assists_160_count = p_assists_ex_en + cluster_mean) %>%
        select(player_id, p_assists_160_count)

# Convert prediction to a rate stat (per second)

p_assists_160 <- convert_pred_to_rate_stats(p_assists_160, rate_stats_160, "assists", "160")
p_assists_160 <- select(p_assists_160, c(1,4))

##### Projected assists (raw): predicted assists (80 GP)

# Get regression data

reg_assists_80 <- get_regression_data_assists(pbp_data, oi_xg_data, date_80_gp)

# Add assists predicted by regression model

reg_assists_80 <- predict_assists(reg_assists_80)

# Add a prediction for empty net assists based on cluster analysis

reg_assists_80 <- add_empty_net_assists(reg_assists_80, pbp_data, date_80_gp)

# Combine predictions and shrink data

p_assists_80 <- reg_assists_80 %>%
        mutate(p_assists_80_count = p_assists_ex_en + cluster_mean) %>%
        select(player_id, p_assists_80_count)

# Convert prediction to a rate stat (per second)

p_assists_80 <- convert_pred_to_rate_stats(p_assists_80, rate_stats_80, "assists", "80")
p_assists_80 <- select(p_assists_80, c(1,4))

##### Projected assists (raw): predicted assists (40 GP)

# Get regression data

reg_assists_40 <- get_regression_data_assists(pbp_data, oi_xg_data, date_40_gp)

# Add assists predicted by regression model

reg_assists_40 <- predict_assists(reg_assists_40)

# Add a prediction for empty net assists based on cluster analysis

reg_assists_40 <- add_empty_net_assists(reg_assists_40, pbp_data, date_40_gp)

# Combine predictions and shrink data

p_assists_40 <- reg_assists_40 %>%
        mutate(p_assists_40_count = p_assists_ex_en + cluster_mean) %>%
        select(player_id, p_assists_40_count)

# Convert prediction to a rate stat (per second)

p_assists_40 <- convert_pred_to_rate_stats(p_assists_40, rate_stats_40, "assists", "40")
p_assists_40 <- select(p_assists_40, c(1,4))

##### Projected assists (raw): join predicted assists data and apply GP weighting

p_assists_raw <- p_assists_160 %>%
        left_join(p_assists_80, by = "player_id") %>%
        left_join(p_assists_40, by = "player_id")

p_assists_raw$p_assists_80_rate[is.na(p_assists_raw$p_assists_80_rate)] <- 0
p_assists_raw$p_assists_40_rate[is.na(p_assists_raw$p_assists_40_rate)] <- 0

p_assists_raw <- mutate(p_assists_raw, p_assists_raw = (p_assists_160_rate * 0.3) + (p_assists_80_rate * 0.6) + (p_assists_40_rate * 0.1))

p_assists_raw <- select(p_assists_raw, player_id, p_assists_raw)

# PROJECTED SHOTS/HITS/BLOCKS (RAW) ############################################

# This is simply a blend of rate stats from 160/80/40 GP
# Could be worth exploring an adjustment based on current team
# For example, joining a better team could cause an increase in shots and a decrease in hits/blocks

##### Projected shots/hits/blocks (raw): collect rate stats based on GP

p_shb_160 <- rate_stats_160 %>%
        select(c(1,10,11,12))
names(p_shb_160)[2] <- "shots_rate_160"
names(p_shb_160)[3] <- "hits_rate_160"
names(p_shb_160)[4] <- "blocks_rate_160"

p_shb_80 <- rate_stats_80 %>%
        select(c(1,10,11,12))
names(p_shb_80)[2] <- "shots_rate_80"
names(p_shb_80)[3] <- "hits_rate_80"
names(p_shb_80)[4] <- "blocks_rate_80"

p_shb_40 <- rate_stats_40 %>%
        select(c(1,10,11,12))
names(p_shb_40)[2] <- "shots_rate_40"
names(p_shb_40)[3] <- "hits_rate_40"
names(p_shb_40)[4] <- "blocks_rate_40"

##### Projected shots/hits/blocks (raw): join and blend rate stats

p_shb_raw <- p_shb_160 %>%
        left_join(p_shb_80, by = "player_id") %>%
        left_join(p_shb_40, by = "player_id")

p_shb_raw <- p_shb_raw %>%
        mutate(p_shots_raw = (shots_rate_160 * 0.35) + (shots_rate_80 * 0.55) + (shots_rate_40 * 0.1)) %>%
        mutate(p_hits_raw = (hits_rate_160 * 0.35) + (hits_rate_80 * 0.55) + (hits_rate_40 * 0.1)) %>%
        mutate(p_blocks_raw = (blocks_rate_160 * 0.35) + (blocks_rate_80 * 0.55) + (blocks_rate_40 * 0.1))

p_shb_raw <- select(p_shb_raw, c(1,11,12,13))

# PROJECTED TOI (RAW) ##########################################################

##### Projected TOI (raw): collect TOI / GP based on 160/80/40 GP

toi_as_160_gp <- rate_stats_160 %>%
        mutate(toi_160 = toi_total / gp) %>%
        select(player_id, toi_160)
toi_as_80_gp <- rate_stats_80 %>%
        mutate(toi_80 = toi_total / gp) %>%
        select(player_id, toi_80)
toi_as_40_gp <- rate_stats_40 %>%
        mutate(toi_40 = toi_total / gp) %>%
        select(player_id, toi_40)

##### Projected TOI (raw): join and blend TOI

p_toi_raw <- toi_as_160_gp %>%
        left_join(toi_as_80_gp, by = "player_id") %>%
        left_join(toi_as_40_gp, by = "player_id") %>%
        filter(player_id %in% filtered_skaters)

p_toi_raw <- mutate(p_toi_raw, p_toi_raw = (toi_160 * 0.15) + (toi_80 * 0.65) + (toi_40 * 0.2))
p_toi_raw <- select(p_toi_raw, player_id, p_toi_raw)

# ASSEMBLE RAW SKATER PROJECTIONS ##############################################

##### Assemble raw skater projections: join the raw data

raw_skater_projections <- projections_base %>%
        left_join(p_toi_raw, by = "player_id") %>%
        left_join(p_goals_raw, by = "player_id") %>%
        left_join(p_assists_raw, by = "player_id") %>%
        left_join(p_shb_raw, by = "player_id")

# ADJUSTMENTS TO SKATER PROJECTIONS ############################################

##### Adjustments to skater projections: adjust TOI (cluster + age)

# Prepare data for clustering based on TOI + points rate (split by position)

toi_forwards_data <- filter(raw_skater_projections, position == "F") %>%
        mutate(p_raw_points = p_goals_raw + p_assists_raw)

toi_forwards_cluster_data <- select(toi_forwards_data, p_toi_raw, 
                                    p_raw_points)

toi_defensemen_data <- filter(raw_skater_projections, position == "D") %>%
        mutate(p_raw_points = p_goals_raw + p_assists_raw)

toi_defensemen_cluster_data <- select(toi_defensemen_data, p_toi_raw, 
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
raw_skater_projections$age_adj_toi <- ifelse(raw_skater_projections$age_sos > 12419, (raw_skater_projections$cluster_toi * 0.97), raw_skater_projections$cluster_toi) 

# Adjust TOI for age < 25 at October 1, 2023 
# Adjust skaters who have high TOI deviation with the hope that these skaters will have fewer "low" TOI games as they improve (50% of 1 standard deviation)

u_25_skaters <- filter(raw_skater_projections, age_sos < 8766)
u_25_skaters <- u_25_skaters$player_id

toi_sd_data <- gl_data_80 %>%
        group_by(player_id) %>%
        summarise(avg_toi = mean(toi_as), 
                  sd_toi = sd(toi_as)) %>%
        mutate(variation = sd_toi / avg_toi) %>%
        filter(player_id %in% u_25_skaters) %>%
        filter(variation > 0.15)

toi_sd_data <- mutate(toi_sd_data, u_25_toi_adj = sd_toi * 0.5) %>%
        select(player_id, u_25_toi_adj)

raw_skater_projections <- left_join(raw_skater_projections, toi_sd_data, by = "player_id")
raw_skater_projections$u_25_toi_adj[is.na(raw_skater_projections$u_25_toi_adj)] <- 0

raw_skater_projections <- mutate(raw_skater_projections, p_toi = age_adj_toi + u_25_toi_adj) %>%
        select(-cluster_toi, -age_adj_toi, -u_25_toi_adj)

##### Adjustments to skater projections: adjust goals (elite shooters + age)

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
        select(player_id, shot_pct)

elite_shooter_ids <- elite_shooter_data$player_id

# Adjust goals for elite shooters (60 / 40)
# Note: this can actually reduce a skater's projected goals (see Brayden Point)

raw_skater_projections <- left_join(raw_skater_projections, elite_shooter_data, by = "player_id")

raw_skater_projections <- mutate(raw_skater_projections, es_adj_goals = ifelse(player_id %in% elite_shooter_ids, (p_goals_raw * 0.4) + ((p_shots_raw * shot_pct) * 0.6), p_goals_raw))

# Adjust goals for age > 33 at October 1, 2023 (97% of es_adj_goals)

raw_skater_projections$adj_goals <- ifelse(raw_skater_projections$age_sos > 12419, (raw_skater_projections$es_adj_goals * 0.97), raw_skater_projections$es_adj_goals) 

raw_skater_projections <- select(raw_skater_projections, -shot_pct, -es_adj_goals )

##### Adjustments to skater projections: adjust assists/shots/hits (age)

# Adjust remaining stats (excluding blocks) for age > 33 at October 1, 2023 (97%)

raw_skater_projections$adj_assists <- ifelse(raw_skater_projections$age_sos > 12419, (raw_skater_projections$p_assists_raw * 0.97), raw_skater_projections$p_assists_raw)

raw_skater_projections$p_shots <- ifelse(raw_skater_projections$age_sos > 12419, (raw_skater_projections$p_shots_raw * 0.97), raw_skater_projections$p_shots_raw)

raw_skater_projections$p_hits <- ifelse(raw_skater_projections$age_sos > 12419, (raw_skater_projections$p_hits_raw * 0.97), raw_skater_projections$p_hits_raw)

raw_skater_projections$p_blocks <- raw_skater_projections$p_blocks_raw 

raw_skater_projections <- select(raw_skater_projections, -age_sos)

##### Adjustments to skater projections: adjust goals/assists (current environment)

# As a proxy for recent on-ice scoring environment use: oi_xg_ex_player from reg_assists_80 + xg on goals scored 80 GP (with a 1.5 multiplier for goals)
# Give each skater a z-score based on his past scoring environment

skater_xg_80gp_data <- filter(pbp_data, date > date_80_gp,
                              period != 5,
                              event_type == "GOAL",
                              goalie_id > 0) %>%
        group_by(event_player_1_id) %>%
        summarise(xg_goals = sum(xg))
skater_xg_80gp_data$xg_goals <- skater_xg_80gp_data$xg_goals * 1.5
names(skater_xg_80gp_data)[1] <- "player_id"

skater_oi_data <- select(reg_assists_80, player_id, oi_xg_ex_player) 

skater_oi_data <- left_join(skater_oi_data, skater_xg_80gp_data, by = "player_id")
skater_oi_data$xg_goals[is.na(skater_oi_data$xg_goals)] <- 0

skater_oi_data <- mutate(skater_oi_data, xg_oi = oi_xg_ex_player + xg_goals)

skater_oi_data$avg_oi <- mean(skater_oi_data$xg_oi)
skater_oi_data$sd_oi <- mean(skater_oi_data$xg_oi)
skater_oi_data <- mutate(skater_oi_data, skater_z = (xg_oi - avg_oi) / sd_oi)

# Give each team a z-score based on top 8 projected goal scorers

team_oi_data <- select(raw_skater_projections, player_id, 
                       team, p_toi, 
                       adj_goals)
team_oi_data <- mutate(team_oi_data, goals = p_toi * adj_goals) %>%
        group_by(team) %>%
        top_n(8, goals)

team_oi_data <- summarise(team_oi_data, team_goals = sum(goals))

team_oi_data$avg_goals <- mean(team_oi_data$team_goals)
team_oi_data$sd_goals <- sd(team_oi_data$team_goals)
team_oi_data <- mutate(team_oi_data, team_z = (team_goals - avg_goals) / sd_goals)

# Adjust z-scores relative to lowest scores

min_skater_z <- min(skater_oi_data$skater_z)
min_team_z <- min(team_oi_data$team_z)

skater_oi_data$skater_z_rel <- skater_oi_data$skater_z - min_skater_z  
team_oi_data$team_z_rel <- team_oi_data$team_z - min_team_z 

# Join z_rel to skater projections

skater_z <- select(skater_oi_data, player_id, 
                   skater_z_rel)
team_z <- select(team_oi_data, team, 
                 team_z_rel)

raw_skater_projections <- left_join(raw_skater_projections, skater_z, by = "player_id")
raw_skater_projections <- left_join(raw_skater_projections, team_z, by = "team")
raw_skater_projections$skater_z_rel[is.na(raw_skater_projections$skater_z_rel)] <-0

raw_skater_projections <- mutate(raw_skater_projections, diff_z = team_z_rel - skater_z_rel)

# Identify skaters with multi-team history

skater_teams_last_season <- select(gl_data_80, 
                         player_id, 
                         team_name) %>%
        group_by(player_id) %>%
        unique()

multiple_teams_last_season <- skater_teams_last_season %>%
        summarise(teams = n()) %>%
        filter(teams > 1)
multiple_teams_last_season <- multiple_teams_last_season$player_id

skater_current_team <- team_rosters %>%
        select(player_id,
               team)
names(skater_current_team)[2] <- "team_name"

new_team <- setdiff(skater_current_team, skater_teams_last_season)  
new_team <- filter(new_team, player_id %in% filtered_skaters) 
new_team <- new_team$player_id

multiple_teams <- c(multiple_teams_last_season, new_team)

# Add environment data to skater projections

raw_skater_projections$multi_team <- ifelse(raw_skater_projections$player_id %in% multiple_teams, TRUE, FALSE)

# Adjust projected goals based on environment for skaters +/- 1 sd
# Adjustment is stronger for multi-team skaters

diff_z_avg <- sd(raw_skater_projections$diff_z)
diff_z_sd <- sd(raw_skater_projections$diff_z)

adj_goals_sd <- sd(raw_skater_projections$adj_goals)

raw_skater_projections <- mutate(raw_skater_projections, p_goals = case_when(
        multi_team == FALSE & diff_z < 0 ~ adj_goals + ((diff_z * adj_goals_sd) * 0.05),
        multi_team == FALSE & diff_z > 2 ~ adj_goals + ((diff_z * adj_goals_sd) * 0.05),
        multi_team == TRUE & diff_z < 0 ~ adj_goals + ((diff_z * adj_goals_sd) * 0.15),
        multi_team == TRUE & diff_z > 2 ~ adj_goals + ((diff_z * adj_goals_sd) * 0.15),
        TRUE ~ adj_goals))

# Adjust projected assists based on environment for skaters +/- 1 sd
# Adjustment is stronger for multi-team skaters

adj_assists_sd <- sd(raw_skater_projections$adj_assists)

raw_skater_projections <- mutate(raw_skater_projections, p_assists = case_when(
        multi_team == FALSE & diff_z < 0 ~ adj_assists + ((diff_z * adj_assists_sd) * 0.05),
        multi_team == FALSE & diff_z > 2 ~ adj_assists + ((diff_z * adj_assists_sd) * 0.05),
        multi_team == TRUE & diff_z < 0 ~ adj_assists + ((diff_z * adj_assists_sd) * 0.15),
        multi_team == TRUE & diff_z > 2 ~ adj_assists + ((diff_z * adj_assists_sd) * 0.15),
        TRUE ~ adj_assists))

# SKATER PROJECTIONS ###########################################################

##### Skater projections: select wanted data

skater_projections <- select(raw_skater_projections, c(1,2,4:7,15,25,26,18:20))

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

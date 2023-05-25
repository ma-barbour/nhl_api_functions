# SETUP ########################################################################

#install.packages("tidyverse")
#install.packages("jsonlite")
#install.packages("lubridate")
#install.packages("parsedate")

library(tidyverse)
library(jsonlite)
library(lubridate)
library(parsedate)

# FUNCTION: GET PLAY-BY-PLAY DATA ##############################################

# This function pulls (and adds to) the play-by-play data for a single game_id
# This function DOES NOT pull player shift data
# Note: the data provided by the NHL can be incorrect / incomplete
# Use my get_schedule() function to find a game_id
# Wrap this function in a loop to pull data for multiple games at the same time (see EXAMPLE below)

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
        
        # Add the event players to the play-by-play data
        
        pbp_data <- bind_cols(all_plays, players) %>%
                arrange(game_seconds)
        
        # Add a warning for long distance goals scored against a goalie
        # Potential x-coordinate error in the NHL data
        # Errors happen. If you pull all the data for the 2021-2022 and 2022-2023 seasons you will find 18 instances where this check returns a "TRUE"
        # Note: not every instance will be an error. Sometimes goalies let in bad goals
        
        pbp_data <- mutate(pbp_data, check_x_error = ifelse(event_type == "GOAL" & sa_distance > 89 & goalie_id > 0, TRUE, FALSE))
        
        return(pbp_data)
}

# EXAMPLE ######################################################################

# Pull play-by-play data for Oilers vs Flames

oilers_flames_pbp <- get_play_by_play_data(2022020555)

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
        arrange(date, game_id, game_seconds)

# Curious about what those dangerous shot attempts look like?
# Plot the dangerous shot attempts using sportyR

#install.packages("sportyR")

library(sportyR)

plot_data <- filter(oilers_flames_pbp, sa_dangerous == TRUE)

plot_dangerous_sa <- geom_hockey("NHL", display_range = "full") + geom_point(data = plot_data, aes(x_fixed, y_fixed))

plot_dangerous_sa


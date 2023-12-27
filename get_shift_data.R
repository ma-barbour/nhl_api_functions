# SETUP ########################################################################

#install.packages("tidyverse")
#install.packages("jsonlite")

library(tidyverse)
library(jsonlite)

# FUNCTION: GET SHIFT DATA #####################################################

# This function pulls the shift data for a single game_id
# It is intended to be joined to the full play-by-play data pulled using my get_play_by_play_data() function (left_join using game_id and game_seconds to get on-ice data for each event in the play-by-play data)
# The data provided by the NHL can sometimes be incorrect / incomplete - the function takes this into account but often cannot "correct" the data 
# Use my get_schedule() function to find a game_id

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
        
# EXAMPLE ######################################################################

# Pull shift data for Oilers vs Flames game

oilers_flames_shifts <- get_shift_data(2022020555)


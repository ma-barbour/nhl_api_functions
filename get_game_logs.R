# SETUP ########################################################################

#install.packages("tidyverse")
#install.packages("jsonlite")
#install.packages("lubridate")

library(tidyverse)
library(jsonlite)
library(lubridate)

# FUNCTION: GET GAME LOGS ######################################################

# This function pulls a player's game logs for a specified season
# SKATERS ONLY
# REGULAR SEASON ONLY
# Time-on-ice data is returned as seconds
# Use my get_team_rosters() function to find player_ids
# The season format is YEARYEAR (for example 20222023)
# Wrap this function in a loop to pull data for multiple players at the same time (see EXAMPLE below)

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

# EXAMPLE ######################################################################

# Pull Connor McDavid's game logs for the 2022-2023 season

mcdavid_gl_2022_2023 <- get_game_logs(8478402, 20222023)

# Wrap this function in a loop to pull game logs for multiple skaters
# This example returns game logs for Connor McDavid and Leon Draisaitl
# Create a vector of player_ids and then run them through the loop 

player_ids <- c(8478402, 8477934)

temp_gl_list <- list()

for (i in 1:length(player_ids)) {
        
        game_logs <- get_game_logs(player_ids[i], 20222023)
        
        temp_gl_list[[i]] <- game_logs
}

multi_gl_2022_2023 <- bind_rows(temp_gl_list) %>%
        arrange(date)


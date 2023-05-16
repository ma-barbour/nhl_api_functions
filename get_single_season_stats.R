# SETUP ########################################################################

#install.packages("tidyverse")
#install.packages("jsonlite")
#install.packages("lubridate")

library(tidyverse)
library(jsonlite)
library(lubridate)

# FUNCTION: GET SINGLE SEASON STATS ############################################

# This function pulls a player's stats for a specified season
# SKATERS ONLY
# REGULAR SEASON ONLY
# Time-on-ice data is returned as seconds
# Use my get_team_rosters() function to find player_ids
# The season format is YEARYEAR (for example 20222023)
# Wrap this function in a loop to pull data for multiple players at the same time (see EXAMPLE below)

get_single_season_stats <- function(player_id, season) {
        
        # Get the player's full name
        
        player_site <- read_json(paste0("https://statsapi.web.nhl.com/api/v1/people/", player_id))
        player_name <- player_site$people %>%
                tibble() %>%
                unnest_wider(1)
        player_name <- player_name$fullName
        
        # Pull and organize the raw data
        
        sss_site <- read_json(paste0("https://statsapi.web.nhl.com/api/v1/people/", player_id, "/stats/?stats=statsSingleSeason&season=", season))
        
        stats <- sss_site$stats %>%
                tibble() %>%
                unnest_wider(1) 
        stats <- stats$splits %>%
                tibble() %>%
                unnest(1) %>%
                unnest_wider(1) %>%
                tibble() %>%
                unnest_wider(2) 
        
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
                        gp = games, 
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
                        shooting_pct = shotPct,
                        face_off_pct = faceOffPct,
                        plus_minus = plusMinus)
        
        # Final clean up for time
        
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

# Pull Connor McDavid's stats for the 2022-2023 season

mcdavid_sss_2022_2023 <- get_single_season_stats(8478402, 20222023)

# Wrap this function in a loop to pull stats for multiple skaters
# This example returns stats for Connor McDavid and Leon Draisaitl
# Create a vector of player_ids and then run them through the loop 

player_ids <- c(8478402, 8477934)

temp_sss_list <- list()

for (i in 1:length(player_ids)) {
        
        season_stats <- get_single_season_stats(player_ids[i], 20222023)
        
        temp_sss_list[[i]] <- season_stats
}

multi_sss_2022_2023 <- bind_rows(temp_sss_list) 


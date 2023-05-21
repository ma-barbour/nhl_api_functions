# SETUP ########################################################################

#install.packages("tidyverse")
#install.packages("jsonlite")
#install.packages("lubridate")

library(tidyverse)
library(jsonlite)
library(lubridate)

# FUNCTION: GET CAREER STATS ###################################################

# This function pulls a player's NHL career stats
# SKATERS ONLY
# REGULAR SEASON
# Time-on-ice data is returned as seconds
# Use my get_team_rosters() function to find player_ids
# Wrap this function in a loop to pull data for multiple players at the same time (see EXAMPLE below)

get_career_stats <- function(player_id) {
        
        # Get the player's full name
        
        player_site <- read_json(paste0("https://statsapi.web.nhl.com/api/v1/people/", player_id))
        player_name <- player_site$people %>%
                tibble() %>%
                unnest_wider(1)
        player_name <- player_name$fullName
        
        # Pull and organize the raw data
        
        yby_site <- read_json(paste0("https://statsapi.web.nhl.com/api/v1/people/", player_id, "/stats/?stats=yearByYear"))
        
        stats <- yby_site$stats %>%
                tibble() %>%
                unnest_wider(1) 
        stats <- stats$splits %>%
                tibble() %>%
                unnest(1) %>%
                unnest_wider(1) %>%
                unnest_wider(stat) %>%
                unnest_wider(team, names_sep = "_") %>%
                unnest_wider(league, names_sep = "_")
                
        # Add special teams assists data
        
        stats <- mutate(stats, assists_pp = powerPlayPoints - powerPlayGoals,
                        assists_sh = shortHandedPoints - shortHandedGoals)
        
        # Add even strength scoring data
        
        stats <- mutate(stats, goals_es = goals - (powerPlayGoals + shortHandedGoals), 
                        assists_es = assists - (assists_pp + assists_sh))
        
        # Clean up for time
        
        stats$timeOnIce <- ms(stats$timeOnIce, quiet = TRUE)
        stats$timeOnIce <- period_to_seconds(stats$timeOnIce)
        stats$evenTimeOnIce <- ms(stats$evenTimeOnIce, quiet = TRUE)
        stats$evenTimeOnIce <- period_to_seconds(stats$evenTimeOnIce)
        stats$powerPlayTimeOnIce <- ms(stats$powerPlayTimeOnIce, quiet = TRUE)
        stats$powerPlayTimeOnIce <- period_to_seconds(stats$powerPlayTimeOnIce)
        stats$shortHandedTimeOnIce <- ms(stats$shortHandedTimeOnIce, quiet = TRUE)
        stats$shortHandedTimeOnIce <- period_to_seconds(stats$shortHandedTimeOnIce)
        
        # Filter for only NHL data
        
        stats <- filter(stats, league_id == 133)
        
        # Sum the data for each stat
        
        stats <- summarise(stats, gp = sum(games),
                           toi_as = sum(timeOnIce),
                           toi_es = sum(evenTimeOnIce),
                           toi_pp = sum(powerPlayTimeOnIce),
                           toi_sh = sum(shortHandedTimeOnIce),
                           goals = sum(goals),
                           assists = sum(assists),
                           points = sum(points),
                           shots = sum(shots),
                           hits = sum(hits),
                           blocks = sum(blocked),
                           pim = sum(pim),
                           goals_es = sum(goals_es),
                           goals_pp = sum(powerPlayGoals),
                           goals_sh = sum(shortHandedGoals),
                           goals_ot = sum(overTimeGoals),
                           assists_es = sum(assists_es),
                           assists_pp = sum(assists_pp),
                           assists_sh = sum(assists_sh),
                           points_pp = sum(powerPlayPoints),
                           points_sh = sum(shortHandedPoints))
        
        # Add career shooting percentage
        
        stats <- mutate(stats, shooting_pct = round((goals / shots) * 100, 1))
        
        # Add player data
        
        stats <- mutate(stats, player_id = player_id,
                        player = player_name) 
        
        # Rearrange the data to be returned by the function
        
        stats <- select(stats, c(23:24,1:22))

        return(stats)
}

# EXAMPLE ######################################################################

# Pull Connor McDavid's career stats

mcdavid_career_stats <- get_career_stats(8478402)

# Wrap this function in a loop to pull stats for multiple skaters
# This example returns stats for Connor McDavid and Leon Draisaitl
# Create a vector of player_ids and then run them through the loop 

player_ids <- c(8478402, 8477934)

temp_career_list <- list()

for (i in 1:length(player_ids)) {
        
        career_stats <- get_career_stats(player_ids[i])
        
        temp_career_list[[i]] <- career_stats
}

multi_career_stats <- bind_rows(temp_career_list) 


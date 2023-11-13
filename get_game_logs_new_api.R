# SETUP ########################################################################

#install.packages("tidyverse")
#install.packages("jsonlite")
#install.packages("lubridate")

library(tidyverse)
library(jsonlite)
library(lubridate)

# FUNCTION: GET GAME LOGS ######################################################

# This function pulls a skater's regular season game logs for a specified season
# Time-on-ice data is returned as seconds
# Use my get_team_rosters() function to find player_ids
# The season format is YEARYEAR (for example 20222023)
# Wrap this function in a loop to pull data for multiple players at the same time (see EXAMPLE below)

get_skater_game_logs <- function(player_id, season) {
        
        # Get the player's name
        
        player_site <- read_json(paste0("https://api-web.nhle.com/v1/player/", player_id, "/landing"))
        
        name <- paste(player_site[["firstName"]], player_site[["lastName"]])
        
        # Pull and unnest the raw game log data
        
        base_url <- "https://api-web.nhle.com/v1/player/"
        
        site_url <- paste0(base_url, player_id, "/game-log/", season, "/2" )
        
        gl_site <- read_json(site_url)
        
        gl_data <- gl_site[["gameLog"]] %>%
                tibble() %>%
                unnest_wider(1) %>%
                unnest_wider(commonName, names_sep = "_") %>%
                unnest_wider(opponentCommonName, names_sep = "_")

        # Add player_id and name
        
        gl_data <- gl_data %>%
                mutate(player_id = player_id,
                       player = name)
        
        # Add even strength goals data
        
        gl_data <- gl_data %>%
                mutate(es_goals = goals - (powerPlayGoals + shorthandedGoals))
        
        # Add all game state assists data
        
        gl_data <- gl_data %>%
                mutate(pp_assists = powerPlayPoints - powerPlayGoals) %>%
                mutate(sh_assists = shorthandedPoints - shorthandedGoals) %>%
                mutate(es_assists = assists - (pp_assists + sh_assists))
        
        # Select target data
        
        gl_data <- gl_data %>%
                select(game_id = gameId,
                       date = gameDate,
                       player_id,
                       player,
                       team = teamAbbrev,
                       opponent = opponentAbbrev,
                       home_away = homeRoadFlag,
                       points,
                       goals,
                       assists,
                       shots,
                       pim,
                       es_goals,
                       es_assists,
                       pp_goals = powerPlayGoals,
                       pp_assists,
                       sh_goals = shorthandedGoals,
                       sh_assists,
                       toi_as = toi)
        
        # Change home/away to "H" / "A"
        
        gl_data <- gl_data %>%
                mutate(home_away = if_else(home_away == "H", "H", "A"))
        
        # Clean up date and TOI
        
        gl_data$date <- as.Date(gl_data$date)
        gl_data$toi_as <- ms(gl_data$toi_as)
        gl_data$toi_as <- period_to_seconds(gl_data$toi_as)
        
        # Create rate stats (per 60 minutes)

        gl_data <- gl_data %>%
                mutate(goals_60m = (goals / toi_as) * 60 * 60) %>%
                mutate(assists_60m = (assists / toi_as) * 60 * 60) %>%
                mutate(shots_60m = (shots / toi_as) * 60 * 60)
                
        return(gl_data)
        
}

# EXAMPLE ######################################################################

# Pull Connor McDavid's game logs for the 2022-2023 season

mcdavid_gl_2022_2023 <- get_skater_game_logs(8478402, 20222023)

# Wrap this function in a loop to pull game logs for multiple skaters
# This example returns game logs for Connor McDavid and Leon Draisaitl
# Create a vector of player_ids and then run them through the loop 

player_ids <- c(8478402, 8477934)

temp_gl_list <- list()

for (i in 1:length(player_ids)) {
        
        game_logs <- get_skater_game_logs(player_ids[i], 20222023)
        
        temp_gl_list[[i]] <- game_logs
}

multi_gl_2022_2023 <- bind_rows(temp_gl_list) %>%
        arrange(player_id,
                date)

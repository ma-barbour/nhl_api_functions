# SETUP ########################################################################

#install.packages("tidyverse")
#install.packages("jsonlite")
#install.packages("lubridate")

library(tidyverse)
library(jsonlite)
library(lubridate)

# FUNCTION: GET ALL TEAM ROSTERS ###############################################

# This function loops through each team's API endpoint and pulls expanded roster data

get_all_team_rosters <- function() {
        
        # Create a vector of team_ids
        
        teams_site <- read_json("https://statsapi.web.nhl.com/api/v1/teams")
        
        teams <- teams_site$teams %>% 
                tibble() %>% 
                unnest_wider(1) %>% 
                select(team_id = id, full_team_name = name, team_abbr = abbreviation)
        
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

# EXAMPLE ######################################################################

current_nhl_roster_data <- get_all_team_rosters()


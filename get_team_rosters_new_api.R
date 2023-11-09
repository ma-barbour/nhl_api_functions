# SETUP ########################################################################

#install.packages("tidyverse")
#install.packages("jsonlite")
#install.packages("lubridate")

library(tidyverse)
library(jsonlite)
library(lubridate)

# FUNCTION: GET CURRENT TEAM ROSTERS ###########################################

# This function pulls the current roster data for every team

get_current_rosters <- function () {
        
        # Get team tri-codes (using MTL as the base club)
        
        season <- "20232024"
        
        tri_code_url <- paste0("https://api-web.nhle.com/v1/club-schedule-season/mtl/", season)
        
        tri_code_data <- read_json(tri_code_url)
        
        tri_codes <- tri_code_data[["games"]] %>%
                tibble() %>%
                unnest_wider(1) %>%
                filter(gameType == 2) %>%
                select(awayTeam) %>%
                unnest_wider(1)
        
        tri_codes <- unique(tri_codes$abbrev)
        
        # Loop through each team's roster
        
        base_url <- "https://api-web.nhle.com/v1/roster/"
        
        roster_loop_data <- list()
        
        for (i in (1:length(tri_codes))) {
                
                temp_roster_data <- read_json(paste0(base_url, tri_codes[i], "/current"))
                
                temp_roster <- temp_roster_data %>%
                        tibble() %>%
                        unnest_longer(1) %>%
                        unnest_wider(1) %>%
                        unnest_wider(firstName, names_sep = "_") %>%
                        unnest_wider(lastName, names_sep = "_") %>%
                        mutate(name = paste(firstName_default, lastName_default)) %>%
                        mutate(team = tri_codes[i]) %>%
                        select(player_id = id,
                               name,
                               position = positionCode,
                               team,
                               dob = birthDate,
                               height_cm = heightInCentimeters,
                               weight_kg = weightInKilograms,
                               headshot)
                
                temp_roster$dob <- as.Date(temp_roster$dob)
                
                roster_loop_data[[i]] <- temp_roster
                
        }
        
        # Combine the loop data
        
        roster_data <- roster_loop_data %>%
                bind_rows()
        
        return(roster_data)
        
}

# FUNCTION: GET OLD TEAM ROSTERS ###############################################

# This function pulls roster data for every team for the specified season
# Enter the season in format "20**20**" (include the quotation marks)
# Players who were on more than one team in a season will appear more than once

get_old_rosters <- function (season) {
        
        # Get team tri-codes (using MTL as the base club)
        
        tri_code_url <- paste0("https://api-web.nhle.com/v1/club-schedule-season/mtl/", season)
        
        tri_code_data <- read_json(tri_code_url)
        
        tri_codes <- tri_code_data[["games"]] %>%
                tibble() %>%
                unnest_wider(1) %>%
                filter(gameType == 2) %>%
                select(awayTeam) %>%
                unnest_wider(1)
        
        tri_codes <- unique(tri_codes$abbrev)
        
        # Loop through each team's roster
        
        base_url <- "https://api-web.nhle.com/v1/roster/"
        
        roster_loop_data <- list()
        
        for (i in (1:length(tri_codes))) {
                
                temp_roster_data <- read_json(paste0(base_url, tri_codes[i], "/", season))
                 
                temp_roster <- temp_roster_data %>%
                        tibble() %>%
                        unnest_longer(1) %>%
                        unnest_wider(1) %>%
                        unnest_wider(firstName, names_sep = "_") %>%
                        unnest_wider(lastName, names_sep = "_") %>%
                        mutate(name = paste(firstName_default, lastName_default)) %>%
                        mutate(team = tri_codes[i]) %>%
                        select(player_id = id,
                               name,
                               position = positionCode,
                               team,
                               dob = birthDate,
                               height_cm = heightInCentimeters,
                               weight_kg = weightInKilograms,
                               headshot)
                
                temp_roster$dob <- as.date(temp_roster$dob)
                
                roster_loop_data[[i]] <- temp_roster
          
        }
        
        # Combine the loop data
        
        roster_data <- roster_loop_data %>%
                bind_rows()
        
        return(roster_data)
        
}

# EXAMPLE ######################################################################

# Pull current roster data

roster_data_current <- get_current_rosters()

# Pull 2022-2023 roster data

roster_data_2022_2023 <- get_old_rosters("20222023")


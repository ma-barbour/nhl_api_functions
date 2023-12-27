# SETUP ########################################################################

#install.packages("tidyverse")
#install.packages("jsonlite")

library(tidyverse)
library(jsonlite)

# FUNCTION: GET SEASON SCHEDULE ################################################

# This function pulls the full NHL regular schedule for a specified season
# The season format is "20**20**" (include the quotation marks)

get_season_schedule <- function(season) {
        
        # Get team tri-codes for the season (using MTL as the base club)
        
        tri_code_url <- paste0("https://api-web.nhle.com/v1/club-schedule-season/mtl/", season)
        
        tri_code_data <- read_json(tri_code_url)
        
        tri_codes <- tri_code_data[["games"]] %>%
                tibble() %>%
                unnest_wider(1) %>%
                filter(gameType == 2) %>%
                select(awayTeam) %>%
                unnest_wider(1)
        
        tri_codes <- unique(tri_codes$abbrev)
        
        # Loop through each team's season schedule
        
        base_url <- "https://api-web.nhle.com/v1/club-schedule-season/"
        
        schedule_loop_data <- list()
        
        for (i in (1:length(tri_codes))) {
                
                temp_schedule_data <- read_json(paste0(base_url, tri_codes[i], "/", season))
                
                temp_schedule <- temp_schedule_data[["games"]] %>%
                        tibble() %>%
                        unnest_wider(1) %>%
                        filter(gameType == 2) %>%
                        unnest_wider(awayTeam, names_sep = "_") %>%
                        unnest_wider(homeTeam, names_sep = "_") %>%
                        select(game_id = id,
                               season,
                               date = gameDate,
                               away_team = awayTeam_abbrev,
                               home_team = homeTeam_abbrev)
                
                temp_schedule$date <- as.Date(temp_schedule$date)
                
                schedule_loop_data[[i]] <- temp_schedule
                
        }
        
        # Combine the loop data
        
        schedule_data <- schedule_loop_data %>%
                bind_rows()
        
        # Remove duplicates
        
        schedule_data <- unique(schedule_data)
        
        # Arrange by date
        
        schedule_data <- schedule_data %>%
                arrange(date)
        
        return(schedule_data)
        
}

# FUNCTION: GET DAY SCHEDULE ###################################################

# This function pulls the schedule for a specified day
# The day format is "YYYY-MM-DD" (include the quotation marks)

get_day_schedule <- function(day) {
        
        schedule_site <- read_json(paste0("https://api-web.nhle.com/v1/schedule/", day))
        
        schedule <- schedule_site[["gameWeek"]] %>%
                tibble() %>%
                unnest_wider(1) %>%
                filter(date == day) %>%
                unnest_longer(games) %>%
                unnest_wider(games) %>%
                unnest_wider(awayTeam, names_sep = "_") %>%
                unnest_wider(homeTeam, names_sep = "_") %>%
                unnest_wider(awayTeam_placeName, names_sep = "_") %>%
                unnest_wider(homeTeam_placeName, names_sep = "_") %>%
                select(game_id = id,
                       date,
                       season,
                       away_team_id = awayTeam_id,
                       home_team_id = homeTeam_id,
                       away_team = awayTeam_placeName_default,
                       home_team = homeTeam_placeName_default,
                       away_goals = awayTeam_score,
                       home_goals = homeTeam_score,
                       away_logo = awayTeam_logo,
                       home_logo = homeTeam_logo)

        return(schedule)
        
}

# EXAMPLES #####################################################################

# Pull data for the 2023-2024 regular season

schedule_20232024 <- get_season_schedule("20232024")

# Pull data for multiple seasons using a loop

seasons <- c("20222023", "20232024")

loop_data <- list()

for (j in (1:length(seasons))) {
        
        temp_loop_data <- get_season_schedule(seasons[j])
        
        loop_data[[j]] <- temp_loop_data
        
}

schedule_multi_seasons <- loop_data %>%
        bind_rows() %>%
        arrange(date)

# Pull data for a specified day

schedule_day <- get_day_schedule("2023-11-07")


# SETUP ########################################################################

#install.packages("tidyverse")
#install.packages("jsonlite")
#install.packages("lubridate")

library(tidyverse)
library(jsonlite)
library(lubridate)

# FUNCTION: GET SCHEDULE #######################################################

# This function pulls the NHL schedule for a specified time period
# The date format is "YYYY-MM-DD" (include the quotation marks)
# The default setting is to pull data for ONLY the regular season
# To pull data for ALL GAMES enter "FALSE" for reg_szn (include the quotation marks)

get_schedule <- function(start_date, end_date, reg_szn = "TRUE") {
        
        # Generate the appropriate URL based on reg_szn TRUE/FALSE
        
        sched_url <- ifelse(reg_szn == "FALSE", paste0("https://statsapi.web.nhl.com/api/v1/schedule?startDate=", start_date, "&endDate=",  end_date), paste0("https://statsapi.web.nhl.com/api/v1/schedule?startDate=", start_date, "&endDate=",  end_date, "&gameType=R")) 
        
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

# EXAMPLES######################################################################

# Pull data for the 2021-2022 regular season

reg_szn_schedule_data_2021_2022 <- get_schedule("2021-09-24", "2022-06-28")

# Pull data for all games in the 2021-2022 season

full_szn_schedule_data_2021_2022 <- get_schedule("2021-09-24", "2022-06-28", "FALSE")

# Pull data for a single day (October 28, 2021)

single_day_schedule_data <- get_schedule("2021-10-28", "2021-10-28")
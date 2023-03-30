
##########

##### rm(list = ls())


# for manual testing I will go through like 5-10 links and validate results agaisnt the actual website

library(tidyverse)

library(rvest)
library(xml2)

library(worldfootballR)
source('Development and Test Files /Second Function Attempt.R')

print(paste("Starting to collect 2017/18 Premier League data at: ", Sys.time()))

epl_17_18_urls <- 
    fb_match_urls(country = "", 
                  gender = "M", 
                  season_end_year = 2018, 
                  tier = "", 
                  non_dom_league_url = "https://fbref.com/en/comps/9/history/Premier-League-Seasons")

epl_17_18 <- detail_fb_data_for_all_matches(matches_urls = epl_17_18_urls, time_pause = 3, season = "2017/2018")

print(paste("Finished collection of 2017/18 Premier League data at: ", Sys.time()))

################################################################################################
################################################################################################

print(paste("Starting to collect 2018/19 Premier League data at: ", Sys.time()))

epl_18_19_urls <- 
    fb_match_urls(country = "", 
                  gender = "M", 
                  season_end_year = 2019, 
                  tier = "", 
                  non_dom_league_url = "https://fbref.com/en/comps/9/history/Premier-League-Seasons")

epl_18_19 <- detail_fb_data_for_all_matches(matches_urls = epl_18_19_urls, time_pause = 3, season = "2018/2019")

print(paste("Finished collection of 2018/19 Premier League data at: ", Sys.time()))

# there was some issue with the function after the 54% percent completion

################################################################################################
################################################################################################

print(paste("Starting to collect 2019/20 Premier League data at: ", Sys.time()))

epl_19_20_urls <- 
    fb_match_urls(country = "", 
                  gender = "M", 
                  season_end_year = 2020, 
                  tier = "", 
                  non_dom_league_url = "https://fbref.com/en/comps/9/history/Premier-League-Seasons")

epl_19_20 <- detail_fb_data_for_all_matches(matches_urls = epl_19_20_urls, time_pause = 3, season = "2019/2020")

print(paste("Finished collection of 2019/20 Premier League data at: ", Sys.time()))
print(paste0("Total observations 19/20: ", nrow(epl_19_20)))

################################################################################################
################################################################################################

print(paste("Starting to collect 2020/21 Premier League data at: ", Sys.time()))

epl_20_21_urls <- 
    fb_match_urls(country = "", 
                  gender = "M", 
                  season_end_year = 2021, 
                  tier = "", 
                  non_dom_league_url = "https://fbref.com/en/comps/9/history/Premier-League-Seasons")

epl_20_21 <- detail_fb_data_for_all_matches(matches_urls = epl_20_21_urls, time_pause = 3, season = "2020/2021")

print(paste("Finished collection of 2020/21 Premier League data at: ", Sys.time()))
print(paste0("Total observations 20/21: ", nrow(epl_20_21)))

################################################################################################
################################################################################################

print(paste("Starting to collect 2021/22 Premier League data at: ", Sys.time()))

epl_21_22_urls <- 
    fb_match_urls(country = "", 
                  gender = "M", 
                  season_end_year = 2022, 
                  tier = "", 
                  non_dom_league_url = "https://fbref.com/en/comps/9/history/Premier-League-Seasons")

epl_21_22 <- detail_fb_data_for_all_matches(matches_urls = epl_21_22_urls, time_pause = 3, season = "2021/2022")

print(paste("Finished collection of 2021/22 Premier League data at: ", Sys.time()))
print(paste0("Total observations 21/22: ", nrow(epl_21_22)))

################################################################################################
################################################################################################

print(paste("Starting to collect 2022/23 Premier League data at: ", Sys.time()))

epl_22_23_urls <- 
    fb_match_urls(country = "", 
                  gender = "M", 
                  season_end_year = 2023, 
                  tier = "", 
                  non_dom_league_url = "https://fbref.com/en/comps/9/history/Premier-League-Seasons")

epl_22_23 <- detail_fb_data_for_all_matches(matches_urls = epl_22_23_urls, time_pause = 3, season = "2021/2022")

print(paste("Finished collection of 2022/23 Premier League data at: ", Sys.time()))
print(paste0("Total observations 22/23: ", nrow(epl_22_23)))





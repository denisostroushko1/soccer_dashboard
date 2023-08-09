
source("Master Packages.R")
source("Master Functions.R")


liga_link <- "https://fbref.com/en/comps/14/history/Copa-Libertadores-Seasons"

################################################################################################
################################################################################################

print(paste("Starting to collect 2022 Copa Libertadores data at: ", Sys.time()))

urls_22 <- 
  fb_match_urls_modified(
              country = "", 
              gender = "M", 
              season_end_year = 2022, 
              tier = "", 
              non_dom_league_url = liga_link, 
              time_pause = 5, 
              seasons_df= seasons) 


urls_22 <- remove_promotion_relegation_playoffs(urls_22)

lib_22 <- detail_fb_data_for_all_matches(matches_urls = urls_22, time_pause = 5)

lib_22$season = "2022"
  
print(paste("Finished collection of 2022 Copa Libertadores data at: ", Sys.time()))
print(paste("Total observations: ", nrow(lib_22)))

################################################################################################
################################################################################################

print(paste("Starting to collect 2021 Copa Libertadores data at: ", Sys.time()))

urls_21 <- 
  fb_match_urls_modified(
              country = "", 
              gender = "M", 
              season_end_year = 2021, 
              tier = "", 
              non_dom_league_url = liga_link, 
              time_pause = 5, 
              seasons_df= seasons) 


urls_21 <- remove_promotion_relegation_playoffs(urls_21)

lib_21 <- detail_fb_data_for_all_matches(matches_urls = urls_21, time_pause = 5)

lib_21$season = "2021"
  
print(paste("Finished collection of 2021 Copa Libertadores data at: ", Sys.time()))
print(paste("Total observations: ", nrow(lib_21)))

################################################################################################
################################################################################################

print(paste("Starting to collect 2020 Copa Libertadores data at: ", Sys.time()))

urls_20 <- 
  fb_match_urls_modified(
              country = "", 
              gender = "M", 
              season_end_year = 2020, 
              tier = "", 
              non_dom_league_url = liga_link, 
              time_pause = 5, 
              seasons_df= seasons) 


urls_20 <- remove_promotion_relegation_playoffs(urls_20)

lib_20 <- detail_fb_data_for_all_matches(matches_urls = urls_20, time_pause = 5)

lib_20$season = "2020"
  
print(paste("Finished collection of 2020 Copa Libertadores data at: ", Sys.time()))
print(paste("Total observations: ", nrow(lib_20)))

################################################################################################
################################################################################################

print(paste("Starting to collect 2019 Copa Libertadores data at: ", Sys.time()))

urls_19 <- 
  fb_match_urls_modified(
              country = "", 
              gender = "M", 
              season_end_year = 2019, 
              tier = "", 
              non_dom_league_url = liga_link, 
              time_pause = 5, 
              seasons_df= seasons) 


urls_19 <- remove_promotion_relegation_playoffs(urls_19)

lib_19 <- detail_fb_data_for_all_matches(matches_urls = urls_19, time_pause = 5)

lib_19$season = "2019"

print(paste("Finished collection of 2019 Copa Libertadores data at: ", Sys.time()))
print(paste("Total observations: ", nrow(lib_19)))

################################################################################################
################################################################################################

copa_1_f <- 
  rbind(lib_22,
        lib_21,
        lib_20,
        lib_19)


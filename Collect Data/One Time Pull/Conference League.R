

source("Master Packages.R")
source("Master Functions.R")


liga_link <- "https://fbref.com/en/comps/882/history/Europa-Confconfnce-League-Seasons"

################################################################################################
################################################################################################

print(paste("Starting to collect 2021/22 European Confconfnce League data at: ", Sys.time()))

urls_21_22 <- 
    fb_match_urls(country = "", 
                  gender = "M", 
                  season_end_year = 2022, 
                  tier = "", 
                  non_dom_league_url = liga_link)

urls_21_22 <- remove_promotion_relegation_playoffs(urls_21_22)

conf_21_22 <- detail_fb_data_for_all_matches(matches_urls = urls_21_22, time_pause = 3)

print(paste("Finished collection of 2021/22 European Confconfnce League data at: ", Sys.time()))
print(paste("Total observations: ", nrow(conf_21_22)))

################################################################################################
################################################################################################

print(paste("Starting to collect 2022/23 European Confconfnce League data at: ", Sys.time()))

urls_22_23 <- 
    fb_match_urls(country = "", 
                  gender = "M", 
                  season_end_year = 2023, 
                  tier = "", 
                  non_dom_league_url = liga_link)

urls_22_23 <- remove_promotion_relegation_playoffs(urls_22_23)

conf_22_23 <- detail_fb_data_for_all_matches(matches_urls = urls_22_23, time_pause = 3)

print(paste("Finished collection of 2022/23 European Confconfnce League data at: ", Sys.time()))
print(paste("Total observations: ", nrow(conf_22_23)))

################################################################################################
################################################################################################

conf_1_f <- 
  rbind(conf_22_23,
        conf_21_22)


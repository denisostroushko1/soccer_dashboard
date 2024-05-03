 

source("Master Packages.R")
source("Master Functions.R")

liga_link <- "https://fbref.com/en/comps/31/history/Liga-MX-Seasons"

################################################################################################
################################################################################################

print(paste("Starting to collect 2018/19 Mexican Liga MX data at: ", Sys.time()))

urls_18_19 <- 
    fb_match_urls(country = "", 
                  gender = "M", 
                  season_end_year = 2019, 
                  tier = "", 
                  non_dom_league_url = liga_link)

urls_18_19 <- remove_promotion_relegation_playoffs(urls_18_19)

liga_mx_18_19 <- detail_fb_data_for_all_matches(matches_urls = urls_18_19, time_pause = 3)

print(paste("Finished collection of 2018/19 Mexican Liga MX data at: ", Sys.time()))
print(paste("Total observations: ", nrow(liga_mx_18_19)))

################################################################################################
################################################################################################

print(paste("Starting to collect 2019/20 Mexican Liga MX data at: ", Sys.time()))

urls_19_20 <- 
    fb_match_urls(country = "", 
                  gender = "M", 
                  season_end_year = 2020, 
                  tier = "", 
                  non_dom_league_url = liga_link)

urls_19_20 <- remove_promotion_relegation_playoffs(urls_19_20)

liga_mx_19_20 <- detail_fb_data_for_all_matches(matches_urls = urls_19_20, time_pause = 3)

print(paste("Finished collection of 2019/20 Mexican Liga MX data at: ", Sys.time()))
print(paste("Total observations: ", nrow(liga_mx_19_20)))

################################################################################################
################################################################################################

print(paste("Starting to collect 2020/21 Mexican Liga MX data at: ", Sys.time()))

urls_20_21 <- 
    fb_match_urls(country = "", 
                  gender = "M", 
                  season_end_year = 2021, 
                  tier = "", 
                  non_dom_league_url = liga_link)

urls_20_21 <- remove_promotion_relegation_playoffs(urls_20_21)

liga_mx_20_21 <- detail_fb_data_for_all_matches(matches_urls = urls_20_21, time_pause = 3)

print(paste("Finished collection of 2020/21 Mexican Liga MX data at: ", Sys.time()))
print(paste("Total observations: ", nrow(liga_mx_20_21)))

################################################################################################
################################################################################################

print(paste("Starting to collect 2021/22 Mexican Liga MX data at: ", Sys.time()))

urls_21_22 <- 
    fb_match_urls(country = "", 
                  gender = "M", 
                  season_end_year = 2022, 
                  tier = "", 
                  non_dom_league_url = liga_link)

urls_21_22 <- remove_promotion_relegation_playoffs(urls_21_22)

liga_mx_21_22 <- detail_fb_data_for_all_matches(matches_urls = urls_21_22, time_pause = 3)

print(paste("Finished collection of 2021/22 Mexican Liga MX data at: ", Sys.time()))
print(paste("Total observations: ", nrow(liga_mx_21_22)))

################################################################################################
################################################################################################

print(paste("Starting to collect 2022/23 Mexican Liga MX data at: ", Sys.time()))

urls_22_23 <- 
    fb_match_urls(country = "", 
                  gender = "M", 
                  season_end_year = 2023, 
                  tier = "", 
                  non_dom_league_url = liga_link)

urls_22_23 <- remove_promotion_relegation_playoffs(urls_22_23)

liga_mx_22_23 <- detail_fb_data_for_all_matches(matches_urls = urls_22_23, time_pause = 3)

print(paste("Finished collection of 2022/23 Mexican Liga MX data at: ", Sys.time()))
print(paste("Total observations: ", nrow(liga_mx_22_23)))

################################################################################################
################################################################################################

liga_mx_1_f <- 
  rbind(liga_mx_22_23,
        liga_mx_21_22,
        liga_mx_20_21,
        liga_mx_19_20, 
        liga_mx_18_19)


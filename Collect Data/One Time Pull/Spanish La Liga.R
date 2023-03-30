

source("Master Packages.R")
source("Master Functions.R")


print(paste("Starting to collect 2017/18 Spanish La Liga data at: ", Sys.time()))

liga_link <- "https://fbref.com/en/comps/12/history/La-Liga-Seasons"

################################################################################################
################################################################################################
la_17_18_urls <- 
    fb_match_urls(country = "", 
                  gender = "M", 
                  season_end_year = 2018, 
                  tier = "", 
                  non_dom_league_url = liga_link)

la_17_18 <- detail_fb_data_for_all_matches(matches_urls = la_17_18_urls, time_pause = 3)

print(paste("Finished collection of 2017/18 Spanish La Liga data at: ", Sys.time()))
print(paste("Total observations: ", nrow(la_17_18)))

################################################################################################
################################################################################################

print(paste("Starting to collect 2018/19 Spanish La Liga data at: ", Sys.time()))

la_18_19_urls <- 
    fb_match_urls(country = "", 
                  gender = "M", 
                  season_end_year = 2019, 
                  tier = "", 
                  non_dom_league_url = liga_link)

la_18_19 <- detail_fb_data_for_all_matches(matches_urls = la_18_19_urls, time_pause = 3)

print(paste("Finished collection of 2018/19 Spanish La Liga data at: ", Sys.time()))
print(paste("Total observations: ", nrow(la_18_19)))

################################################################################################
################################################################################################

print(paste("Starting to collect 2019/20 Spanish La Liga data at: ", Sys.time()))

la_19_20_urls <- 
    fb_match_urls(country = "", 
                  gender = "M", 
                  season_end_year = 2020, 
                  tier = "", 
                  non_dom_league_url = liga_link)

la_19_20 <- detail_fb_data_for_all_matches(matches_urls = la_19_20_urls, time_pause = 3)

print(paste("Finished collection of 2019/20 Spanish La Liga data at: ", Sys.time()))
print(paste("Total observations: ", nrow(la_19_20)))

################################################################################################
################################################################################################

print(paste("Starting to collect 2020/21 Spanish La Liga data at: ", Sys.time()))

la_20_21_urls <- 
    fb_match_urls(country = "", 
                  gender = "M", 
                  season_end_year = 2021, 
                  tier = "", 
                  non_dom_league_url = liga_link)

la_20_21 <- detail_fb_data_for_all_matches(matches_urls = la_20_21_urls, time_pause = 3)

print(paste("Finished collection of 2020/21 Spanish La Liga data at: ", Sys.time()))
print(paste("Total observations: ", nrow(la_20_21)))

################################################################################################
################################################################################################

print(paste("Starting to collect 2021/22 Spanish La Liga data at: ", Sys.time()))

la_21_22_urls <- 
    fb_match_urls(country = "", 
                  gender = "M", 
                  season_end_year = 2022, 
                  tier = "", 
                  non_dom_league_url = liga_link)

la_21_22 <- detail_fb_data_for_all_matches(matches_urls = la_21_22_urls, time_pause = 3)

print(paste("Finished collection of 2021/22 Spanish La Liga data at: ", Sys.time()))
print(paste("Total observations: ", nrow(la_21_22)))

################################################################################################
################################################################################################

print(paste("Starting to collect 2022/23 Spanish La Liga data at: ", Sys.time()))

la_22_23_urls <- 
    fb_match_urls(country = "", 
                  gender = "M", 
                  season_end_year = 2023, 
                  tier = "", 
                  non_dom_league_url = liga_link)

la_22_23 <- detail_fb_data_for_all_matches(matches_urls = la_22_23_urls, time_pause = 3)

la_22_23

print(paste("Finished collection of 2022/23 Spanish La Liga data at: ", Sys.time()))
print(paste("Total observations: ", nrow(la_21_23)))

################################################################################################
################################################################################################

la_liga_f <- 
  rbind(la_22_23,
        la_21_22,
        la_20_21,
        la_19_20, 
        la_18_19,
        la_17_18)


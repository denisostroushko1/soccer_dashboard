

source("Master Packages.R")
source("Master Functions.R")


liga_link <- "https://fbref.com/en/comps/10/history/Championship-Seasons"

################################################################################################
################################################################################################

print(paste("Starting to collect 2018/19 English Championship data at: ", Sys.time()))

urls_18_19 <- 
    fb_match_urls(country = "", 
                  gender = "M", 
                  season_end_year = 2019, 
                  tier = "", 
                  non_dom_league_url = liga_link)

urls_18_19 <- remove_promotion_relegation_playoffs(urls_18_19)

eng_2_championship_18_19 <- detail_fb_data_for_all_matches(matches_urls = urls_18_19, time_pause = 3)

print(paste("Finished collection of 2018/19 English Championship data at: ", Sys.time()))
print(paste("Total observations: ", nrow(eng_2_championship_18_19)))

################################################################################################
################################################################################################

print(paste("Starting to collect 2019/20 English Championship data at: ", Sys.time()))

urls_19_20 <- 
    fb_match_urls(country = "", 
                  gender = "M", 
                  season_end_year = 2020, 
                  tier = "", 
                  non_dom_league_url = liga_link)

urls_19_20 <- remove_promotion_relegation_playoffs(urls_19_20)

eng_2_championship_19_20 <- detail_fb_data_for_all_matches(matches_urls = urls_19_20, time_pause = 3)

print(paste("Finished collection of 2019/20 English Championship data at: ", Sys.time()))
print(paste("Total observations: ", nrow(eng_2_championship_19_20)))

################################################################################################
################################################################################################

print(paste("Starting to collect 2020/21 English Championship data at: ", Sys.time()))

urls_20_21 <- 
    fb_match_urls(country = "", 
                  gender = "M", 
                  season_end_year = 2021, 
                  tier = "", 
                  non_dom_league_url = liga_link)

urls_20_21 <- remove_promotion_relegation_playoffs(urls_20_21)

eng_2_championship_20_21 <- detail_fb_data_for_all_matches(matches_urls = urls_20_21, time_pause = 3)

print(paste("Finished collection of 2020/21 English Championship data at: ", Sys.time()))
print(paste("Total observations: ", nrow(eng_2_championship_20_21)))

################################################################################################
################################################################################################

print(paste("Starting to collect 2021/22 English Championship data at: ", Sys.time()))

urls_21_22 <- 
    fb_match_urls(country = "", 
                  gender = "M", 
                  season_end_year = 2022, 
                  tier = "", 
                  non_dom_league_url = liga_link)

urls_21_22 <- remove_promotion_relegation_playoffs(urls_21_22)

eng_2_championship_21_22 <- detail_fb_data_for_all_matches(matches_urls = urls_21_22, time_pause = 3)

print(paste("Finished collection of 2021/22 English Championship data at: ", Sys.time()))
print(paste("Total observations: ", nrow(eng_2_championship_21_22)))

################################################################################################
################################################################################################

print(paste("Starting to collect 2022/23 English Championship data at: ", Sys.time()))

urls_22_23 <- 
    fb_match_urls(country = "", 
                  gender = "M", 
                  season_end_year = 2023, 
                  tier = "", 
                  non_dom_league_url = liga_link)

urls_22_23 <- remove_promotion_relegation_playoffs(urls_22_23)

eng_2_championship_22_23 <- detail_fb_data_for_all_matches(matches_urls = urls_22_23, time_pause = 3)

print(paste("Finished collection of 2022/23 English Championship data at: ", Sys.time()))
print(paste("Total observations: ", nrow(eng_2_championship_22_23)))

################################################################################################
################################################################################################

eng_2_championship_1_f <- 
  rbind(eng_2_championship_22_23,
        eng_2_championship_21_22,
        eng_2_championship_20_21,
        eng_2_championship_19_20, 
        eng_2_championship_18_19)


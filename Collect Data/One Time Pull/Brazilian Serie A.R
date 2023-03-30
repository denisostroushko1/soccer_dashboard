

source("Master Packages.R")
source("Master Functions.R")

liga_link <- "https://fbref.com/en/comps/24/history/Serie-A-Seasons"

################################################################################################
################################################################################################

print(paste("Starting to collect 2018/19 Brazilian Seria A data at: ", Sys.time()))

urls_18_19 <- 
    fb_match_urls(country = "", 
                  gender = "M", 
                  season_end_year = 2019, 
                  tier = "", 
                  non_dom_league_url = liga_link)

urls_18_19 <- remove_promotion_relegation_playoffs(urls_18_19)

seria_bra_18_19 <- detail_fb_data_for_all_matches(matches_urls = urls_18_19, time_pause = 3)

print(paste("Finished collection of 2018/19 Brazilian Seria A data at: ", Sys.time()))
print(paste("Total observations: ", nrow(seria_bra_18_19)))


################################################################################################
################################################################################################

print(paste("Starting to collect 2019/20 Brazilian Seria A data at: ", Sys.time()))

urls_19_20 <- 
    fb_match_urls(country = "", 
                  gender = "M", 
                  season_end_year = 2020, 
                  tier = "", 
                  non_dom_league_url = liga_link)

urls_19_20 <- remove_promotion_relegation_playoffs(urls_19_20)

seria_bra_19_20 <- detail_fb_data_for_all_matches(matches_urls = urls_19_20, time_pause = 3)

print(paste("Finished collection of 2019/20 Brazilian Seria A data at: ", Sys.time()))
print(paste("Total observations: ", nrow(seria_bra_19_20)))

################################################################################################
################################################################################################

print(paste("Starting to collect 2020/21 Brazilian Seria A data at: ", Sys.time()))

urls_20_21 <- 
    fb_match_urls(country = "", 
                  gender = "M", 
                  season_end_year = 2021, 
                  tier = "", 
                  non_dom_league_url = liga_link)

urls_20_21 <- remove_promotion_relegation_playoffs(urls_20_21)

seria_bra_20_21 <- detail_fb_data_for_all_matches(matches_urls = urls_20_21, time_pause = 3)

print(paste("Finished collection of 2020/21 Brazilian Seria A data at: ", Sys.time()))
print(paste("Total observations: ", nrow(seria_bra_20_21)))

################################################################################################
################################################################################################

print(paste("Starting to collect 2021/22 Brazilian Seria A data at: ", Sys.time()))

urls_21_22 <- 
    fb_match_urls(country = "", 
                  gender = "M", 
                  season_end_year = 2022, 
                  tier = "", 
                  non_dom_league_url = liga_link)

urls_21_22 <- remove_promotion_relegation_playoffs(urls_21_22)

seria_bra_21_22 <- detail_fb_data_for_all_matches(matches_urls = urls_21_22, time_pause = 3)

print(paste("Finished collection of 2021/22 Brazilian Seria A data at: ", Sys.time()))
print(paste("Total observations: ", nrow(seria_bra_21_22)))

################################################################################################
################################################################################################

if(T == F){
  print(paste("Starting to collect 2022/23 Brazilian Seria A data at: ", Sys.time()))
  
  urls_22_23 <- 
      fb_match_urls(country = "", 
                    gender = "M", 
                    season_end_year = 2023, 
                    tier = "", 
                    non_dom_league_url = liga_link)
  
  urls_22_23 <- remove_promotion_relegation_playoffs(urls_22_23)
  
  seria_bra_22_23 <- detail_fb_data_for_all_matches(matches_urls = urls_22_23, time_pause = 3)
  
  print(paste("Finished collection of 2022/23 Brazilian Seria A data at: ", Sys.time()))
  print(paste("Total observations: ", nrow(seria_bra_22_23)))
}
################################################################################################
################################################################################################

seria_bra_1_f <- 
  rbind(# seria_bra_22_23,
        seria_bra_21_22,
        seria_bra_20_21,
        seria_bra_19_20, 
        seria_bra_18_19)




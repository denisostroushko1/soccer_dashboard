
source("Master Packages.R")
source("Master Functions.R")


if(file.exists('keys.R') == T){
  source("keys.R")
  
  Sys.setenv("AWS_ACCESS_KEY_ID" = access_key,
             "AWS_SECRET_ACCESS_KEY" = secret_key, 
             "AWS_DEFAULT_REGION" =  aws_region)
}

if(file.exists('keys.R') == F){
        Sys.setenv("AWS_ACCESS_KEY_ID" = Sys.getenv("access_key"),
                   "AWS_SECRET_ACCESS_KEY" = Sys.getenv("secret_key"), 
                   "AWS_DEFAULT_REGION" =  Sys.getenv("aws_region"))
    
}
file_name <- "fixtures_zip.zip" # Replace with the file name of the zipped CSV file

tempfile <- tempfile()  
save_object(object = "s3://shiny-soccer-data/fixtures_zip.zip", file = tempfile)
zipped <- unzip(tempfile)

seasons <- read_csv("seasons_and_fixtures.csv")
seasons <- seasons[,-1]

unlink('seasons_and_fixtures.csv')
unlink('fixtures_zip.zip')


liga_link <- "https://fbref.com/en/comps/882/history/Europa-Conference-League-Seasons"

################################################################################################
################################################################################################

print(paste("Starting to collect 2021/22 European Confconfnce League data at: ", Sys.time()))

urls_21_22 <- 
  fb_match_urls_modified(
              country = "", 
              gender = "M", 
              season_end_year = 2022, 
              tier = "", 
              non_dom_league_url = liga_link, 
              time_pause = 5, 
              seasons_df= seasons) 


urls_21_22 <- remove_promotion_relegation_playoffs(urls_21_22)

conf_21_22 <- detail_fb_data_for_all_matches(matches_urls = urls_21_22, time_pause = 5)

conf_21_22$season = "2021/2022"
  
print(paste("Finished collection of 2021/22 European Confconfnce League data at: ", Sys.time()))
print(paste("Total observations: ", nrow(conf_21_22)))

################################################################################################
################################################################################################

print(paste("Starting to collect 2022/23 European Confconfnce League data at: ", Sys.time()))

urls_22_23 <- 
  fb_match_urls_modified(
              country = "", 
              gender = "M", 
              season_end_year = 2023, 
              tier = "", 
              non_dom_league_url = liga_link, 
              time_pause = 5, 
              seasons_df= seasons) 


urls_22_23 <- remove_promotion_relegation_playoffs(urls_22_23)

conf_22_23 <- detail_fb_data_for_all_matches(matches_urls = urls_22_23, time_pause = 5)
conf_22_23$season = "2022/2023"

print(paste("Finished collection of 2022/23 European Confconfnce League data at: ", Sys.time()))
print(paste("Total observations: ", nrow(conf_22_23)))

################################################################################################
################################################################################################

conf_1_f <- 
  rbind(conf_22_23,
        conf_21_22)

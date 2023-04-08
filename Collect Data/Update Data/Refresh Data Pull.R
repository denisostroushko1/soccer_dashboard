

rm(list = ls())

source("keys.R")
source("Master Packages.R")
source("Master Functions.R")

#####
#   1) download zip fromd rive, unpack and modify the data frame
        drive_deauth()
        file_id <- drive_link # Replace with the file ID of the zipped CSV file
        file_name <- "dashboard_data_csv_zip.zip" # Replace with the file name of the zipped CSV file
        drive_download(as_id(file_id), path = file_name, overwrite = T)
    
        unzip(file_name)
    
        older_data <- read_csv("dashboard_data.csv")
    
        colnames_to_remove <- colnames(older_data)[grep("[1-9]", colnames(older_data))]
        
        older_data <- older_data[, -c(which(colnames(older_data) %in% colnames_to_remove))]
        
        unlink('dashboard_data.csv')
        unlink('dashboard_data_csv_zip.zip')
        
    # tic toc timingL 14.411 sec elapsed
    
#   2) test for comparison: somehow google drive is much faster than AWS infrastructure.... go figure 
  #   
  # tic()
  #   Sys.setenv("AWS_ACCESS_KEY_ID" = access_key,
  #              "AWS_SECRET_ACCESS_KEY" = secret_key,
  #              "AWS_DEFAULT_REGION" = "us-east-2")
  #   
  #   tempfile <- tempfile()  
  #   save_object(object = "s3://shiny-soccer-data/dashboard_data_csv_zip.zip", file = tempfile)
  #   data_all <- read_csv(unzip(tempfile) )
  #   
  #   colnames_to_remove <- colnames(data_all)[grep("[1-9]", colnames(data_all))]
  #   data_all <- data_all[, -c(which(colnames(data_all) %in% colnames_to_remove))]
  # toc()
  # 70.408 sec elapsed
#########################################################

# test the function 
links_in_upate <- pull_new_matches_urls(data_to_compare = older_data)

new_data <- detail_fb_data_for_all_matches(matches_urls = links_in_upate, time_pause = 5)
  # hoping that increasing sleep time from 3 to 5 will add stability 
  # unfortunately, there are still errors sometimes when we set time_pause equal to 3
### pipe all links from the new list into a collect data function 

list_of_leagues_we_update <- 
     c("EFL Championship",
    "Eredivisie" ,
    "La Liga",
    "Liga MX",
    "Ligue 1" ,
    "Major League Soccer",    
    "Premier League"   ,
    "Primeira Liga" ,
    "Serie A" ,
    "UEFA Champions League", 
   # "UEFA Europa League"    ,       
    "Campeonato Brasileiro Série A",  
    "Fußball-Bundesliga"
    )

####### need to replace this such taht I do not rely on someone's repo
seasons <- 
  read.csv("https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/all_leages_and_cups/all_competitions.csv", 
  stringsAsFactors = F)
#######

seasons_for_df <- seasons[seasons$competition_name %in% list_of_leagues_we_update, ] %>% 
  group_by(competition_name) %>% 
  summarize(season_end_year = max(season_end_year)) %>% 
  
  inner_join(
    seasons %>% select(competition_name, season_end_year, seasons), 
    by = c("competition_name", "season_end_year")
  ) %>% 
  
  rename(season = seasons, 
         league_name = competition_name) %>% 
  
  unique() %>% 
  
  mutate(league_name = paste0(league_name, " "))


##### final result
new_data <-
  new_data %>% 
  left_join(
    seasons_for_df, 
    by = "league_name"
  ) 

new_data <- new_data %>% select(-season_end_year)
colnames_to_remove <- colnames(new_data)[grep("[1-9]", colnames(new_data))]
new_data <- new_data[, -c(which(colnames(new_data) %in% colnames_to_remove))]
# upload refreshed data now 

refreshed_data <- rbind(older_data, new_data)

########### 
# SEND DATA TO GOOGLE DRIVE 
write.csv(refreshed_data, "dashboard_data.csv")
### Step 2: zip file
zip(zipfile = "dashboard_data_csv_zip", files = "dashboard_data.csv")

drive_deauth()
soccer_df <- 
  drive_put(
    media = "dashboard_data_csv_zip.zip", 
    path = drive_my_path,
    name = "dashboard_data_csv_zip.zip"
    )
soccer_df %>% drive_share_anyone()

## clean up repository once files are uploaded
unlink('dashboard_data_csv_zip.zip')
unlink('dashboard_data.csv')




source("Master Packages.R")
source("Master Functions.R")

tempfile <- tempfile()  
save_object(object = "s3://shiny-soccer-data/dashboard_data.csv", file = tempfile)
older_data <- read.csv(tempfile)

#########################################################

# test the function 
links_in_upate <- pull_new_matches_urls(data_to_compare = older_data)

new_data <- detail_fb_data_for_all_matches(matches_urls = links_in_upate, time_pause = 3)

### pipe all links from the new list into a collect data fucntion 

list_of_leagues_we_update <- 
    c("EFL Championship",
    "Eredivisie" ,
    "La Liga",
    "Liga MX",
    "Ligue 1" ,
    "Major League Soccer",    
    "Premier League"   ,
    "Primeira Liga"  ,
    "Serie A" ,
    "UEFA Champions League",    
    "Campeonato Brasileiro Série A",  
    "Fußball-Bundesliga"
    )


seasons <- 
  read.csv("https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/all_leages_and_cups/all_competitions.csv", 
  stringsAsFactors = F)

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

# upload refreshed data now 
refreshed_data <- rbind(older_data, new_data)
write.csv(refreshed_data, "./dashboard_data.csv")

Sys.setenv("AWS_ACCESS_KEY_ID" = "AKIAJ6746JOZ3BSWWIEA",
           "AWS_SECRET_ACCESS_KEY" = "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",
           "AWS_DEFAULT_REGION" = "us-east-2")

# Set the name of the S3 bucket you want to upload the file to
bucket_name <- "shiny-soccer-data"

# Set the name you want to give the file in the S3 bucket
s3_file_name <- "dashboard_data.csv"

# Upload the file to S3
put_object(file = "./Collect Data/One Time Pull/dashboard_data.csv", 
           object = s3_file_name,
           bucket = bucket_name)



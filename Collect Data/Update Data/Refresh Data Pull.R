
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


source("Master Packages.R")
source("Master Functions.R")


#####
#   1) downlaod zip from AWS, both files here now! 


        tempfile <- tempfile()  
        save_object(object = "s3://shiny-soccer-data/dashboard_data_csv_zip.zip", file = tempfile)
        zipped <- unzip(tempfile)
        older_data <- read_csv("dashboard_data.csv")
      
        colnames_to_remove <- colnames(older_data)[grep("[1-9]", colnames(older_data))]
        
        older_data <- older_data[, -c(which(colnames(older_data) %in% colnames_to_remove))]
        
        unlink('dashboard_data.csv')
        unlink('dashboard_data_csv_zip.zip')
        
#   2) download file with already used match links 
        file_name <- "already_used_links_zip.zip" # Replace with the file name of the zipped CSV file
        
        tempfile <- tempfile()  
        save_object(object = "s3://shiny-soccer-data/already_used_links_zip.zip", file = tempfile)
        zipped <- unzip(tempfile)
       
        old_links <- read_csv("already_used_links.csv")
        
        unlink('already_used_links.csv')
        unlink('already_used_links_zip.zip')
#########################################################

# test the function 
links_in_upate <- pull_new_matches_urls(data_to_compare = old_links)
      
##############
      # start pulling data 
if(length(links_in_upate) == 0){
  print("No new matches to update")
  
  
  print("Finishing big data - one row per game - process the data to one row per player per season")

  older_data$league_name <- 
    case_when(
      substr(older_data$league_name,
             nchar(older_data$league_name),
             nchar(older_data$league_name)) == " " ~ 
        
        substr(older_data$league_name, 1, nchar(older_data$league_name)-1), 
      
      T ~ older_data$league_name
    )
        print("Looking for duplicate league names (if any)")
        nms <- 
          older_data %>% 
          dplyr::select(league_name) %>% 
          unique() 
        
        nms_d <- 
          data.frame(
            league_name = nms
          )
        
        nms_d$length = nchar(nms_d$league_name)
        nms_d %>% arrange(league_name, length) %>% print()
  
  roll_up <- roll_up_data(big_data = older_data)
  write_feather(roll_up, 'dash_df_rollup.fthr')
  

            
        print("Looking for duplicate league names (if any)")
        nms <- 
          roll_up %>% 
          ungroup() %>% 
          dplyr::select(league_name) %>% 
          unique() 
        
        nms_d <- 
          data.frame(
            league_name = nms
          )
        
        nms_d$length = nchar(nms_d$league_name)
        nms_d %>% arrange(league_name, length) %>% print()
   
  
  
  if(file.exists('dash_df_rollup.fthr') == T){print("created feather file")}
  }

if(length(links_in_upate) != 0){
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
  
        file_name <- "fixtures_zip.zip" # Replace with the file name of the zipped CSV file
        
        tempfile <- tempfile()  
        save_object(object = "s3://shiny-soccer-data/fixtures_zip.zip", file = tempfile)
        zipped <- unzip(tempfile)
       
        seasons <- read_csv("seasons_and_fixtures.csv")
        seasons <- seasons[,-1]
        
        unlink('seasons_and_fixtures.csv')
        unlink('fixtures_zip.zip')

  #######
  
        seasons_for_df <- seasons[seasons$competition_name %in% list_of_leagues_we_update, ] %>% 
          group_by(competition_name) %>% 
          summarize(season_end_year = max(season_end_year)) %>% 
          
          inner_join(
            seasons %>% select(competition_name, season_end_year, season), 
            by = c("competition_name", "season_end_year")
          ) %>% 
          
          rename(
                 league_name = competition_name) %>% 
          
          unique() %>% 
          
          mutate(league_name = paste0(league_name, " "))
  
  
  ##### final result
  if(!is.null(new_data)){
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
    refreshed_data$season <- gsub("-", "/", refreshed_data$season)
  
    
    refreshed_data$league_name <- 
      case_when(
        substr(refreshed_data$league_name,
               nchar(refreshed_data$league_name),
               nchar(refreshed_data$league_name)) == " " ~ 
          
          substr(refreshed_data$league_name, 1, nchar(refreshed_data$league_name)-1), 
        
        T ~ refreshed_data$league_name
      )
  }
        
  if(is.null(new_data)){
    refreshed_data = older_data
  }
##################

  ## save down links we just pulled 
  all_links <- 
    rbind(old_links[,-1], data.frame(fb_ref_match_link = links_in_upate)) %>% 
    unique()
  
  write.csv(all_links, "already_used_links.csv")
  zip(zipfile = "already_used_links_zip", files = "already_used_links.csv")
  
  if(file.exists('keys.R') == F){
    put_object(file = "already_used_links_zip.zip", 
               object = "already_used_links_zip.zip",
               bucket = Sys.getenv("bucket_name"))
  }else{
     put_object(file = "already_used_links_zip.zip", 
               object = "already_used_links_zip.zip",
               bucket = bucket_name)
  }

  unlink('already_used_links.csv')
  unlink('already_used_links_zip.zip')
  
  ########### 
  # SEND DATA TO AWS
  write.csv(refreshed_data, "dashboard_data.csv")
  zip(zipfile = "dashboard_data_csv_zip", files = "dashboard_data.csv")
  
  if(file.exists('keys.R') == F){
  put_object(file = "dashboard_data_csv_zip.zip", 
             object = "dashboard_data_csv_zip.zip",
             bucket = Sys.getenv("bucket_name"))
  }else{
     put_object(file = "dashboard_data_csv_zip.zip", 
               object = "dashboard_data_csv_zip.zip",
               bucket = bucket_name)
  }
  
  ## clean up repository once files are uploaded
  unlink('dashboard_data_csv_zip.zip')
  unlink('dashboard_data.csv')
              
        print("Looking for duplicate league names (if any)")
        nms <- 
          refreshed_data %>% 
          ungroup() %>% 
          dplyr::select(league_name) %>% 
          unique() 
        
        nms_d <- 
          data.frame(
            league_name = nms
          )
        
        nms_d$length = nchar(nms_d$league_name)
        nms_d %>% arrange(league_name, length) %>% print()
  
  print("Finishing big data - one row per game - process the data to one row per player per season")
  
  roll_up <- roll_up_data(big_data = refreshed_data)
  
        print("Looking for duplicate league names (if any)")
        nms <- 
          roll_up %>% 
          ungroup() %>% 
          dplyr::select(league_name) %>% 
          unique() 
        
        nms_d <- 
          data.frame(
            league_name = nms
          )
        
        nms_d$length = nchar(nms_d$league_name)
        nms_d %>% arrange(league_name, length) %>% print()
  
  write_feather(roll_up, 'dash_df_rollup.fthr')
  
}

            
            
            

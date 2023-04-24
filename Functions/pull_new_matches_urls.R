
pull_new_matches_urls <- function(data_to_compare){
  final <- c() # Learned in the R Efficiency data camp course that this is a horrible practive. Need to pre-allocate the vector and re-populate it.
    
        file_name <- "fixtures_zip.zip" # Replace with the file name of the zipped CSV file
        
        tempfile <- tempfile()  
        save_object(object = "s3://shiny-soccer-data/fixtures_zip.zip", file = tempfile)
        zipped <- unzip(tempfile)
       
        seasons <- read_csv("seasons_and_fixtures.csv")
        seasons <- seasons[,-1]
        
        unlink('seasons_and_fixtures.csv')
        unlink('fixtures_zip.zip')

  check <- 
    c("EFL Championship ",
    "Eredivisie " ,
    "La Liga ",
    "Liga MX ",
    "Ligue 1 " ,
    "Major League Soccer ",    
    "Premier League "   ,
    "Primeira Liga " ,
    "Serie A " ,
    "UEFA Champions League ", 
   # "UEFA Europa League"    ,       
    "Campeonato Brasileiro Série A ",  
    "Fußball-Bundesliga "
    )
    
  check2 <- 
    case_when(
    check == "EFL Championship " ~ "EFL Championship",
    check == "Eredivisie " ~ "Eredivisie",
    check == "La Liga " ~ "La Liga",
    check == "Liga MX " ~ "Liga MX",
    check == "Ligue 1 " ~ "Ligue 1",
    check == "Major League Soccer " ~ "Major League Soccer",    
    check == "Premier League " ~ "Premier League",
    check == "Primeira Liga " ~ "Primeira Liga" ,
    check == "Serie A " ~ "Serie A" ,
    check == "UEFA Champions League " ~ "UEFA Champions League", 
   # "UEFA Europa League"    ,       
    check == "Campeonato Brasileiro Série A " ~ "Campeonato Brasileiro Série A",  
    check == "Fußball-Bundesliga " ~ "Fußball-Bundesliga"
    )
    
  for(i in 1:length(check)){
    
    print(paste0("Getting new match links for ", check[i]))
    
    year_end <- max(seasons[seasons$competition_name == check2[i], ]$season_end_year)
    history <- unique(seasons[seasons$season_end_year == year_end & 
                          seasons$competition_name == check2[i], ]$comp_url)[1]
    
    list_of_fixtures <- 
      fb_match_urls_modified(
        country = "", 
        gender = "M", 
        season_end_year = year_end, 
        tier = "", 
        non_dom_league_url = history, 
        time_pause = 3, 
        seasons_df= seasons) 
    
    
    final_list_league <- list_of_fixtures[!list_of_fixtures %in% data_to_compare$fb_ref_match_link]
    
    print(paste0("Obtained ", length(final_list_league), " new links for ", check[i]))
    
    final <- c(final, final_list_league)
    
  }
  return(final)
}
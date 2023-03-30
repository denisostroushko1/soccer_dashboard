
pull_new_matches_urls <- function(data_to_compare){
  final <- c()
  
  seasons <- 
    read.csv("https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/all_leages_and_cups/all_competitions.csv", 
    stringsAsFactors = F)

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
    
  for(i in 1:length(check)){
    
    print(paste0("Getting new match links for ", check[i]))
    
    year_end <- max(seasons[seasons$competition_name == check[i], ]$season_end_year)
    history <- unique(seasons[seasons$season_end_year == year_end & 
                          seasons$competition_name == check[i], ]$comp_url)[1]
    
    list_of_fixtures <- 
      fb_match_urls(country = "", 
                    gender = "M", 
                    season_end_year = year_end, 
                    tier = "", 
                    non_dom_league_url = history)
    
    
    final_list_league <- list_of_fixtures[!list_of_fixtures %in% data_to_compare$fb_ref_match_link]
    
    print(paste0("Obtained ", length(final_list_league), " new links for ", check[i]))
    
    final <- c(final, final_list_league)
    
  }
  return(final)
}
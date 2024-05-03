
# we need to collect the data for: league, year, link to seasons 
# https://fbref.com/en/comps/


all_data_to_pull <- 
  data.frame(
    
    league_name = c(
      "English Premier League",
      "Spanish La Liga",
      "French Ligue 1", 
      "German Bundesliga", 
      "Italian Serie A", 
      "Brazilian Serie A", 
      "Mexican Liga MX", 
      "Dutch Eredevise", 
      "Portugese Primera Liga", 
      "USA MLS", 
      "English Championship", 
      
      "Champions League",
      "Europa League", 
      "Conference League"
    ), 
    
    year = c(
      2018,
      2018, 
      2018, 
      2018, 
      2018, 
      2019, 
      2019, 
      2019, 
      2019, 
      2018, 
      2019, 
      
      2018,
      2018,
      2022
    ), 
    
    link_fb_ref = c(
      "https://fbref.com/en/comps/9/history/Premier-League-Seasons",
      "https://fbref.com/en/comps/12/history/La-Liga-Seasons", 
      "https://fbref.com/en/comps/13/history/Ligue-1-Seasons", 
      "https://fbref.com/en/comps/20/history/Bundesliga-Seasons", 
      "https://fbref.com/en/comps/11/history/Serie-A-Seasons", 
      "https://fbref.com/en/comps/24/history/Serie-A-Seasons", 
      "https://fbref.com/en/comps/31/history/Liga-MX-Seasons", 
      "https://fbref.com/en/comps/23/history/Eredivisie-Seasons", 
      "https://fbref.com/en/comps/32/history/Primeira-Liga-Seasons", 
      "https://fbref.com/en/comps/22/history/Major-League-Soccer-Seasons", 
      "https://fbref.com/en/comps/10/history/Championship-Seasons", 
      
      "https://fbref.com/en/comps/8/history/Champions-League-Seasons",
      "https://fbref.com/en/comps/19/history/Europa-League-Seasons",
      "https://fbref.com/en/comps/882/history/Europa-Conference-League-Seasons"
    )
    
  )

all_data_to_pull$complete <- 
  with(all_data_to_pull, 
       case_when(
         league_name %in% 
           c(
             
      "Brazilian Serie A", 
      "Champions League",
      "Dutch Eredevise"
      
           ) ~ "Y", 
         TRUE ~ "N"
       ))

#           View(all_data_to_pull)


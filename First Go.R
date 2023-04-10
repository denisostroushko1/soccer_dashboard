

rm(list = ls())

#############
# allow app to laod for up to 3 minutes 
############
options(shiny.workerEnv = list(workerTimeout = 180))
###########

source("keys.R")
source("Master Packages.R")
source("Master Functions.R")

Sys.setenv("AWS_ACCESS_KEY_ID" = access_key,
           "AWS_SECRET_ACCESS_KEY" = secret_key,
           "AWS_DEFAULT_REGION" = aws_region)

tempfile <- tempfile()  
save_object(object = "s3://shiny-soccer-data/dashboard_data_csv_zip.zip", file = tempfile)
zipped <- unzip(tempfile)
dash_df <- read_csv("dashboard_data.csv")

########################################################################################################################
########################################################################################################################

server_side <- 
  function(input, output){
    
  }

sidebar <- 
  dashboardSidebar(
    width = 300, 
    sidebarMenu(
      menuItem("Introduction", tabName = "intro")
      ,menuItem("Data Dictionary", tabName = "data_dict")
      ,menuItem("Player Profile", tabName = "player_profile")
      ,menuItem("Team Profile", tabName = "team_profile")
      ,menuItem("Player Scouting", tabName = "player_scouting")
      ,menuItem("Two Player Comparison", tabName = "two_player_comparison")
      
    )
)

body <-
  dashboardBody(
    tabItems(
      tabItem(tabName = "intro", 
              fluidRow("Introduction")
              ),
      tabItem(tabName = "data_dict", 
              fluidRow("Data Dictionary")
              ),
      tabItem(tabName = "player_profile", 
              fluidRow("Player Profile")
              ),
      tabItem(tabName = "team_profile", 
              fluidRow("Team Profile")
              ),
      tabItem(tabName = "player_scouting", 
              fluidRow("Player Scouting")
              ),
      tabItem(tabName = "two_player_comparison", 
              fluidRow("Two Player Comparison")
              )
  ))


shinyApp(
  ui = dashboardPage(
    dashboardHeader(title =  "Soccer Data", titleWidth = 300),
    sidebar,
    body
  ),
  server = server_side
)

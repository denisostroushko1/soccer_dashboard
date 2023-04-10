

rm(list = ls())

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
  function(input, output){ }

sidebar <- 
  dashboardSidebar(
    width = 300, 
    sidebarMenu(


    #  dateInput("min_date", label = "Select Start Date:", value = (max(master_data$datetime)-15))
      
  #    ,dateInput("max_date", label = "Select End Date:", value = (max(master_data$datetime)))
      
  #    ,selectizeInput("assets", "Symbol Search: ", unique(master_data$asset))
      
#      ,sliderInput("cons_day_slider", "Select Cons. Days Highlight Cutoff", min = 1, max = 20, value = 5, step = 1)
      
#      ,sliderInput("size_dots_slider", "Select Size of Highlight Dots", min = 3, max = 20, value = 8, step = 1)
      
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

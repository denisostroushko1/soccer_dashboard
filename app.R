
# 

source("Master Packages.R")

# 
# Sys.setenv("AWS_ACCESS_KEY_ID" = access_key,
#            "AWS_SECRET_ACCESS_KEY" = secret_key,
#            "AWS_DEFAULT_REGION" = aws_region)
# 
# tempfile <- tempfile()  
# save_object(object = "s3://shiny-soccer-data/dashboard_data_csv_zip.zip", file = tempfile)
# zipped <- unzip(tempfile)
# dash_df <- read_csv("dashboard_data.csv", show_col_types = FALSE)[,-1]

# dash_df <- read_csv("dashboard_data.csv", show_col_types = FALSE)[,-1]

dash_df <- read_feather('dash_df.fthr')
dash_df <- dash_df %>% filter(!league_name %in% c('UEFA Champions League'))

########################################################################################################################
########################################################################################################################

# FUNCTIONS 
player_summary <- 
  function(LEAGUE, 
           SEASON, 
           SUMMARY_PLAYER){
    
      dash_df %>% filter(league_name == LEAGUE & 
                               season == SEASON & 
                        summary_player == SUMMARY_PLAYER) %>% 
      
      summarise(
        `Minutes Played` = sum(summary_min), 
        `Games Played` = nrow(
          dash_df %>% filter(league_name == LEAGUE &
                         season == SEASON &
                        summary_player == SUMMARY_PLAYER
                        )
        ),

        Goals = sum(summary_performance_gls), 
        `Expected Goals` = sum(summary_expected_xg), 
        
        Assists = sum(summary_performance_ast), 
        `Expected Assists` = sum(summary_expected_xag)
      ) %>% 
      t() %>% data.frame() -> int_t
    
    colnames(int_t) = c("Statistics")
    
    int_t %>% 
      kable(booktabs = T, 
            align = 'c') %>% 
      kable_styling(bootstrap_options = "striped")
  }

########################################################################################################################
########################################################################################################################

server_side <- 
  function(input, output){
    
output$display_players <- 
  renderDataTable(
    dash_df %>% filter(league_name == input$league_of_player & 
                         season == input$select_season) %>% 
      dplyr::select(summary_player, team_name) %>% 
      unique() %>% 
      arrange(summary_player) %>% 
      datatable()
  )

output$player_profile_season_stat <- 
  function(){
    player_summary(
      LEAGUE = input$league_of_player, 
      SEASON = input$select_season, 
      SUMMARY_PLAYER = input$player_typed_name
    )
    }
    
  }

sidebar <- 
  dashboardSidebar(
    width = 300, 
    sidebarMenu(
      menuItem("Introduction", tabName = "intro")
      ,menuItem("Helper Page", tabName = "helper")
      ,menuItem("Player Profile", tabName = "player_profile")
      ,menuItem("Team Profile", tabName = "team_profile")
      ,menuItem("Player Scouting", tabName = "player_scouting")
      ,menuItem("Two Player Comparison", tabName = "two_player_comparison")
      
    )
)

body <-
  dashboardBody(
    tabItems(
      tabItem(tabName = "intro"
              ),
      tabItem(tabName = "helper", 
              tabsetPanel(
                tabPanel(title = "Look Up Player Name", 
                         fluidRow(
                           column(3, 
                                  box(
                                    sidebarMenu(
                                                
                                   selectInput(inputId = 'league_of_player', 
                                               label = "Select League", 
                                               choices = sort(unique(dash_df$league_name)), 
                                               selected = "Major League Soccer"),
                                   
                                   ### need to select players based on the input of league
                                   ###    i.e. display players who are in the league 
                                   selectInput(inputId = 'select_season', 
                                               label = "Select a Season", 
                                               choices = sort(unique(dash_df$season)), 
                                               selected = '2023')
                                    )
                                  )), 
                           
                           column(9, 
                                  fluidRow(
                                    dataTableOutput('display_players')
                                  ))
                         ))
              )
              ),
      tabItem(tabName = "player_profile",
              fluidRow(
                column(3,
                       box(sidebarMenu(
                         numericInput(inputId = 'games_to_average', 
                                      label = "Running Total/Average of Games", 
                                      value = 10, 
                                      min = 2, 
                                      max = 50, 
                                      step = 1), 
                         
                         textInput(inputId = 'player_typed_name', 
                                   label = "Type in Player Name", 
                                   value = 'Robin Lod')
                         
                         
                       ), width = 12 
                       )
                ),
                column(9,
                       fluidRow(
                         tableOutput('player_profile_season_stat')
                       )
                      )
                    )
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

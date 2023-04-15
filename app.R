
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
dash_df <- dash_df %>% filter(!league_name %in% c('UEFA Champions League') & 
                                seasons %in% c("2022", 
                                               "2023", 
                                               "2021/2022", 
                                               "2022/2023"))

########################################################################################################################
########################################################################################################################

# FUNCTIONS 
player_profile_season_stat <- 
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


profile_plot_player_one_var <- 
  function(
    LEAGUE, 
    SEASON, 
    SUMMARY_PLAYER, 
    VARIABLE, 
    MA
  ){
    dash_df %>% filter(league_name == LEAGUE & 
                        season == SEASON & 
                        summary_player == SUMMARY_PLAYER) %>% 
      arrange(game_date) %>% 
      mutate(
        game_in_seq = seq(1, to = nrow(dash_df %>% filter(league_name == LEAGUE & 
                        season == SEASON & 
                        summary_player == SUMMARY_PLAYER)), by = 1), 
        
        plot_var = !!sym(VARIABLE), 
        ma = frollmean(!!sym(VARIABLE) , MA)
      ) %>% 
      
      dplyr::select(league_name, season, summary_player, game_date, game_in_seq, plot_var, ma) -> plot_df

    with(plot_df, 
         plot_df %>% 
          plot_ly() %>%
    
          add_trace(
            type = 'scatter',
            mode = 'markers+lines',
            name = 'Observed Values',
            opacity = 0.75, 
            x = ~game_in_seq,
            y = ~plot_var, 
            
            text = paste(
              "Game Date:", game_date, 
              "<br>Stat:", plot_var
            ), 
            hoverinfo = 'text'
          ) %>% 
           
          add_trace(
            type = 'scatter',
            mode = 'markers+lines',
            name = 'Moving Average',
            opacity = 0.75,  
            x = ~game_in_seq,
            y = ~ma, 
            
            text = paste(
              "Game Date:", game_date, 
              "<br>Stat MA:", ma
            ), 
            hoverinfo = 'text'
          ) %>% 
           layout(yaxis = list(autotick = F, title = VARIABLE),
                  xaxis = list(ticktext = ~paste(game_date), tickvals = ~paste(game_date)))
    )
  }

top_stats_within_season_within_league <- 
  function(
    LEAGUE, 
    SEASON, 
    SUMMARY_PLAYER, 
    TOP_VARS_N
    ){}
########################################################################################################################
########################################################################################################################

server_side <- 
  function(input, output){
    
output$display_players <- 
  renderDataTable(
    dash_df %>% filter(league_name == input$league_of_player & 
                         season == input$select_season_helper) %>% 
      dplyr::select(summary_player, team_name) %>% 
      unique() %>% 
      arrange(summary_player) %>% 
      datatable()
  )

output$player_profile_season_stat <- 
  function(){
    player_profile_season_stat(
      LEAGUE = input$league_of_player, 
      SEASON = input$select_season, 
      SUMMARY_PLAYER = input$player_typed_name
    )
    }
    
output$profile_plot_player_one_var <- 
  renderPlotly(
    profile_plot_player_one_var(
      LEAGUE = input$league_of_player, 
      SEASON = input$select_season, 
      SUMMARY_PLAYER = input$player_typed_name, 
      VARIABLE = input$profile_plot_metric,
      MA = input$games_to_average
    )
  )

### END SERER SIDE 
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
                                               selected = "Premier League"),
                                   
                                   ### need to select players based on the input of league
                                   ###    i.e. display players who are in the league 
                                   selectInput(inputId = 'select_season_helper', 
                                               label = "Select a Season", 
                                               choices = sort(unique(dash_df$season)), 
                                               selected = '2019/2020')
                                    ), width = 12
                                  )), 
                           
                           column(9, 
                                  fluidRow(
                                    dataTableOutput('display_players'), 
                                    renderPlotly('profile ')
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
                                   value = 'Kevin De Bruyne'),
                         
                         selectInput(inputId = 'select_season', 
                                               label = "Select a Season", 
                                               choices = sort(unique(dash_df$season)), 
                                               selected = '2019/2020'), 
                         
                         selectInput(inputId = 'profile_plot_metric', 
                                     label = "Select a Measurement for Summary", 
                                     choices = sort(colnames(dash_df)), 
                                     selected = 'summary_expected_xg')
                         
                         
                       ), width = 12 
                       )
                ),
                column(9,
                       fluidRow(
                         tableOutput('player_profile_season_stat'), 
                         plotlyOutput('profile_plot_player_one_var')
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

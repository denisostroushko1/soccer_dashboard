

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
  
dash_df <- read_feather('dash_df_rollup.fthr')

dash_df <- data.table(dash_df) 
dash_df <- dash_df[league_name != "UEFA Champions League"]

data_dict <- read_csv('FBref Advanced Soccer Data Disctionary.csv')[,-1]

if(T == F){
  
  DATA = dash_df
  PLAYER = "Kevin De Bruyne"
  SEASON = '2022/2023'
  N_FEATURES = 15
  MINUTES_FILTER = 450
  
}

########################################################################################################################
########################################################################################################################

game_and_min_summary <- 
  function(DATA, PLAYER, SEASON){
    
    DATA[summary_player == PLAYER & 
              season == SEASON]$team_name -> team
    
    DATA[team_name == team & season == SEASON] %>% 
      summarize(
        total_minutes = sum(summary_min), 
        games = max(games_played)
      ) %>% t() -> agg_detail
    
    sum <- DATA[summary_player == PLAYER & 
              season == SEASON] %>% 
      
      dplyr::select(summary_min, games_played) %>% t()
      
    sum <- data.frame(cbind(sum, agg_detail))
    
    sum$p_tot <- with(sum, X1/X2) 
    
    sum <- sum %>% dplyr::select(-X2)
    
    sum
    
  }

dynamic_table_summary <- 
  function(
    DATA, 
    PLAYER, 
    SEASON, 
    COLUMNS){

    func_df <- DATA[
      summary_player == PLAYER & 
        season == SEASON 
    ] %>% dplyr::select(all_of(c("summary_min", "games_played",COLUMNS)))
    
    func_df$games_played -> games
    func_df$summary_min -> minutes
    
    func_df <- func_df %>% dplyr::select(-summary_min, -games_played)
    
    func_df_per_90 <- func_df
    func_df_per_game <- func_df
    
    func_df_per_90 <- func_df_per_90 %>% mutate(across(COLUMNS, function(x){round(x/minutes * 90, 2)})) 
    func_df_per_game <- func_df_per_game %>% mutate(across(COLUMNS, function(x){round(x/games, 2)})) 
    
    f <- rbind(func_df, func_df_per_90, func_df_per_game) %>% t() %>% data.frame()
    
    colnames(f) <- c("Aggregate Numbers", "Per 90 Minutes", "Per Game Played")
    
    datatable(f, options = list(columnDefs = list(list(targets = 1:3, width = '500px',
                                                           className = 'dt-center')))) 
    
  }

best_player_features <- 
  function(
    DATA, 
    PLAYER, 
    SEASON,
    N_FEATURES, 
    MINUTES_FILTER, 
    RETURN_VECTOR){
    
    remove_colnames <- c('season', 'summary_player', 'team_name', 'league_name', 'games_played', 'summary_min')
    
    player_df <-  DATA[summary_player == PLAYER & season == SEASON ] 
    player_df$league_name -> league
    
    player_min <- player_df$summary_min
    
    league_stat <- DATA[league_name == league & season == SEASON & summary_min >= MINUTES_FILTER]
    minutes <- league_stat$summary_min
      
    league_stat <- league_stat %>% 
      dplyr::select(-all_of(remove_colnames))
    
    names <- colnames(league_stat)
    
    f <- data_frame(
      names = names,
      raw_stat = rep(NA, length(names)), 
      stat = rep(NA, length(names)), 
      percentile = rep(NA, length(names))
      )
    
    for( i in 1:length(league_stat)){
      
      col = f$names[i]
      
      player_val <- player_df %>% select(all_of(col)) %>% unlist() 
      player_val_raw <- player_val
      player_val <- player_val / player_min * 90
        
      comp_values <-  
        league_stat %>% 
        select(all_of(col)) %>% 
        unlist() 
      
      comp_values <- comp_values / minutes * 90
      
      percentile <- length(which(comp_values <= player_val)) / 
                      length(comp_values)
      
      f$raw_stat[i] = player_val_raw
      f$stat[i] = player_val
      f$percentile[i] = percentile
        
    }
    
    if(RETURN_VECTOR == "Y"){
      f %>% arrange(-percentile) %>% head(N_FEATURES) %>% dplyr::select(names) %>%  unlist()
    }
    
    if(RETURN_VECTOR == "N"){
      f$stat <- round(f$stat, 2)
      f$percentile <- round(f$percentile, 4)
    
      f <- f %>% arrange(-percentile) %>% head(N_FEATURES)
      datatable(f)
    }
    
  }

clustering <- 
  function(
    DATA, 
    PLAYER, 
    SEASON, 
    MINUTES_FILTER, 
    N_FEATURES, 
    TARGET_SIMILAR_PLAYERS
  ){
    
    ################
    # This is supposed to be created with a different 
        remove_colnames <- c('season', 'summary_player', 'team_name', 'league_name', 'games_played', 'summary_min')
        player_df <-  DATA[summary_player == PLAYER & season == SEASON ] 
        player_df$league_name -> league
        
        player_min <- player_df$summary_min
        
        league_stat <- DATA[league_name == league & season == SEASON & summary_min >= MINUTES_FILTER]
        minutes <- league_stat$summary_min
          
        league_stat <- league_stat %>% 
          dplyr::select(-all_of(remove_colnames))
        
        names <- colnames(league_stat)
        
        f <- data_frame(
          names = names,
          raw_stat = rep(NA, length(names)), 
          stat = rep(NA, length(names)), 
          percentile = rep(NA, length(names))
          )
        
        for( i in 1:length(league_stat)){
          
          col = f$names[i]
          
          player_val <- player_df %>% select(all_of(col)) %>% unlist() 
          player_val_raw <- player_val
          player_val <- player_val / player_min * 90
            
          comp_values <-  
            league_stat %>% 
            select(all_of(col)) %>% 
            unlist() 
          
          comp_values <- comp_values / minutes * 90
          
          percentile <- length(which(comp_values <= player_val)) / 
                          length(comp_values)
          
          f$raw_stat[i] = player_val_raw
          f$stat[i] = player_val
          f$percentile[i] = percentile
            
        }
    f %>% arrange(-percentile) %>% head(N_FEATURES) %>% dplyr::select(names) %>%  unlist() -> FEATURES_LIST
    ###############
    
    
    DATA[summary_player == PLAYER & season == SEASON ]$league_name -> league
    DATA[summary_player == PLAYER & season == SEASON ]$team_name -> team
    
    df <- DATA[league_name == league & season == SEASON & summary_min >= MINUTES_FILTER & team_name != team] %>% 
      
      select(all_of(
          c(FEATURES_LIST, "summary_player", "summary_min", 'team_name')
        ))
    
    df_player <- DATA[summary_player == PLAYER & season == SEASON ] %>% 
      
      select(all_of(
          c(FEATURES_LIST, "summary_player", "summary_min", 'team_name')
        ))
    
    df <- rbind(df, df_player)
    
    colnames(df) <- c(FEATURES_LIST, "summary_player", "summary_min", 'team_name')
    
    players = df$summary_player
    minutes = df$summary_min
    team_name = df$team_name
    
    df <- df %>% select(-summary_player, -summary_min, -team_name)
    
    df <- df %>% 
      mutate_all(~. / minutes * 90)
    
    km = kmeans(df, round(nrow(df)/TARGET_SIMILAR_PLAYERS)
                  )
    
    final <- 
      data.frame(
        cluster = km$cluster, 
        players = players,
        team_name = team_name
      )
    
    final %>% filter(cluster == final[final$players == PLAYER, ]$cluster)
    
  }

########################################################################################################################
########################################################################################################################

server_side <- 
  function(input, output){
    
    output$data_dict <- 
      renderDataTable(
        datatable(data_dict)
      )
      
    output$game_and_min_summary <- 
      renderTable(
        game_and_min_summary(
          PLAYER = input$player_typed_name, 
          SEASON = input$select_season,
          DATA = dash_df)
      )
        
    output$display_players <- 
      renderDataTable(
        dash_df[league_name == input$league_of_player &
                 season == input$select_season_helper ] %>%
    
          dplyr::select(summary_player, team_name) %>% 
          unique() %>% 
          arrange(summary_player) %>% 
          datatable()
      )

    output$dynamic_table_summary <- 
      renderDataTable(
        dynamic_table_summary(
          DATA = dash_df, 
          PLAYER = input$player_typed_name, 
          SEASON = input$select_season, 
          COLUMNS = input$feature
        )
      )
    
    output$best_player_features <-
      renderDataTable(
        best_player_features(
          DATA = dash_df,
          PLAYER = input$player_typed_name,
          SEASON = input$select_season,
          N_FEATURES = input$top_values_number,
          MINUTES_FILTER = input$minutes_to_limit,
          RETURN_VECTOR = "N"
      ))

    output$clustering <- 
      renderTable(
        clustering(
          DATA = dash_df,
          PLAYER = input$player_typed_name,
          SEASON = input$select_season, 
          N_FEATURES = input$top_values_number,
          MINUTES_FILTER = input$minutes_to_limit,
          TARGET_SIMILAR_PLAYERS = input$target_sim_players
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
      tabItem(tabName = "intro", 
              "Hello", 
              verbatimTextOutput('test')
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
                                               selected = '2022/2023')
                                    ), width = 12
                                  )), 
                           
                           column(9, 
                                  fluidRow(
                                    box(dataTableOutput('display_players'), width = 12)
                                  ))
                         )), 
                tabPanel(title = 'Data Dictionary', 
                         fluidRow(box(dataTableOutput('data_dict'), width = 12)))
              )
              ),
      tabItem(tabName = "player_profile",
              fluidRow(
                column(3,
                       box(sidebarMenu(
                        
                         textInput(inputId = 'player_typed_name', 
                                   label = "Type in Player Name", 
                                   value = 'Kevin De Bruyne'),
                         
                         selectInput(inputId = 'select_season', 
                                               label = "Select a Season", 
                                               choices = sort(unique(dash_df$season)), 
                                               selected = '2022/2023'), 
                         
                         selectInput(inputId="feature", 
                                     selected = c('summary_performance_goals', 'summary_performance_ast'),
                                     label="Choose Variables for Table",
                                     choices=  
                                       setdiff( # remove some columns from options here 
                                         colnames(dash_df), 
                                         c('season', 'summary_player', 'team_name', 'league_name', 'games_played', 'summary_min')
                                       ),
                                     multiple=TRUE), 
                         
                         numericInput(inputId = 'top_values_number', 
                                      label = "Number of Top Features to List", 
                                      min = 0, 
                                      max = 100, 
                                      value = 15), 
                         
                         numericInput(inputId = 'minutes_to_limit', 
                                      label = "Limit Players by Limit", 
                                      min = 0, 
                                      max = max(dash_df$summary_min), 
                                      value = 450), 
                         
                         numericInput(inputId = 'target_sim_players', 
                                      label = "Approximate Similar Players to Find", 
                                      min = 0, 
                                      max = 50, 
                                      value = 10)
                         
                         
                         
                       ), width = 12 
                       )
                ),
                column(9,
                       fluidRow(
                                box(tableOutput('game_and_min_summary'), width = 12), 
                                box(dataTableOutput('dynamic_table_summary'), width = 12), 
                                box(dataTableOutput('best_player_features'), width = 12),
                                box(tableOutput('clustering'), width = 12)
                                
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
  ui = 
    dashboardPage(
      dashboardHeader(title =  "Soccer Data", titleWidth = 300),
      sidebar,
      body
      ),
  server = server_side
)

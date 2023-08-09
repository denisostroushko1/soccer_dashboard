

source("Master Packages.R")

top_5_leagues <- 
  c("Fußball-Bundesliga", 
    "La Liga", 
    "Ligue 1" , 
    "Premier League", 
    "Serie A" 
    )

dash_df <- read_feather('dash_df_rollup.fthr')

dash_df <- data.table(dash_df) 

dash_df <- na.omit(dash_df)
dash_df <- data.table(dash_df) 

data_dict <- read_csv('FBref Advanced Soccer Data Disctionary.csv')[,-1]

data_dict <- data_dict %>% filter(!is.na(Pretty.Name.from.FBref ))

    original <- colnames(dash_df)
    reduced <- colnames(dash_df)[ which(colnames(dash_df) %in% data_dict$Data.Frame.Name) ]
    
#   original[which(!original %in% reduced)]
    
    f_colnames <- 
      c(
        'league_name', 
        'games_played', 
        'all_positions', 
        'dominant_position', 
        reduced
      )
    
dash_df <- dash_df[,..f_colnames]

# store positions in a vector now 
positions_short_names <- dash_df %>% mutate(p = substr(dominant_position, 1, 2)) %>% select(p) %>% unique() %>% unlist()
names(positions_short_names) <- positions_short_names
positions_short_names <- c('All', positions_short_names[positions_short_names != "NA"])
names(positions_short_names) <- NULL

# remove these columns for all per 90 calculations 

remove_colnames <- c('season', 'summary_player', 'team_name', 'league_name', 'games_played', 'summary_min', 'all_positions', 
                     'summary_age', 'dominant_position')

remove_colnames_dict <- data_dict %>% filter(Data.Frame.Name %in% remove_colnames) %>% select(Pretty.Name.from.FBref) %>% 
  unlist()

standard_box_style = "overflow-y: scroll;overflow-x: scroll;"
########################################################################################################################
########################################################################################################################


server_side <- 
  function(input, output){
    
    ## HELPER TAB 
     helper_reactive_seaons <- 
          reactive(dash_df[league_name %in% input$league_of_player] %>% select(season) %>% unique() %>% unlist() %>% sort() %>% 
                     set_names(NULL))
        
        output$helper_reactive_seaons_ui <- 
          renderUI({
            selectInput(inputId = 'select_season_helper', 
                         label = "Select a Season", 
                         choices = helper_reactive_seaons(), 
                         selected = helper_reactive_seaons()[length(helper_reactive_seaons())])
        })
    
        helper_reactive_teams <- 
          reactive(dash_df[league_name %in% input$league_of_player] %>% select(team_name) %>% unique() %>% unlist() %>% sort() %>% 
                     set_names(NULL))
        
        output$helper_reactive_teams_ui <- 
          renderUI({
            selectInput(inputId = 'select_team_helper', 
                         label = "Select a Team (or all)", 
                         choices = c("All", helper_reactive_teams() ), 
                         selected = "All")
        })    
    
    output$data_dict <- 
      renderDataTable(
        datatable(data_dict, rownames=FALSE)
      )
    
    output$display_players <- 
      renderDataTable(
        display_players(
          FILTER_POS = input$position_filter_helper, 
          FILTER_TEAM = input$select_team_helper, 
          FILTER_LEAGUE = input$league_of_player, 
          FILTER_SEASON = input$select_season_helper
        )
      )
         
  }
########################################################################################################################
########################################################################################################################

sidebar <- 
  dashboardSidebar(
    width = 250, 
    sidebarMenu(
      menuItem("Introduction", tabName = "intro")
      ,menuItem("Helper Page", tabName = "helper")
      ,menuItem("Player Profile", tabName = "player_profile")
      ,menuItem("Two Player Comparison", tabName = "two_player_comparison")
      ,menuItem("Team Profile", tabName = 'team_profile')
        
      # apparently R is looking for all images to be in the www folder. So I created one and stored images there. 
      # there is no need to refer to the folder explicitly
    #, tags$img(src = "ball.jpeg", width = "100%", height = "100%")
     #  , tags$img(src = "boot.png", width = "100%", height = "100%")
      
    )
)


body <-
  dashboardBody(
        tags$head(tags$style(HTML('
                                /* body */
                                .content-wrapper, .right-side {
                                   background-color: #FFFFFF;
                                }
                                
                              .skin-green .main-sidebar {
                                  background-color:  #36454f;
                                  font-size: 20px
                                                      }'
                              ))), 
    tabItems(
      tabItem(tabName = "intro", 
              HTML(
                "Last Update: ", paste0(format(Sys.time(), tz="America/Chicago"), " CST"), 
                "
                <p style='font-size: 24px; font-weight: bold;'>Welcome! </p>
                <br> 
                <ul>
                
                <li> The dahsboards provides a summary and some analytics of soccer player performance based on 
                over 80 variables, for 12 leagues, for data going back to the 2018, or 2017/2018 seasons! </li> 
                
                <li> The data is taken from FBref.com, a free to use soccer database and news website </li> 
                
                <li> FBref is a great source of data, providing both commonly used metrics, such as Expected Goals (xG) or 
                progressive carries, but also some very detailed statistics, such as the number of times a 
                player 'switches' the field, i.e. reverses the direction of the ball </li> 
                
                <li> All these stats can be used to accurately summarize players' profiles, and find similar players 
                in a pool of over 30,000 players. </li> 
                </ul>
                
                <p style='font-size: 24px; font-weight: bold;'>Navigation: </p>
                
                <ul> 
                
                <li> <b> Helper Page </b>. This tab provides assistance to the user. Here you can find a data dictionary 
                (which I also created used FBref's glossary), and a player look up tab. </li> 
                
                <li> Player look up shows you players, but also will show what years, or seasons, are 
                available for a selected competition. </li>
                
                <br> 
                
                <li> <b> Player profile </b> is a summary page for one selected player, in a selected season. </li> 
                  <ul> 
                    <li> Player's values are presented on both the aggregate scale (total per season), and a per 90 minutes 
                          scale </li> 
                    
                      <li> Each player's 'per 90 minutes' statistics are used to find a percentile of a corresponding statistics. 
                          Higher percentile means player is more of an expert in a given category than most players. </li> 
                    
                      <li> Percentiles are found using a restricted pool of players. By default, you compare a player 
                            against other players who played at least 1,000 minutes in the top 5 European leagues. </li> 
                  </ul> 
                
                
                <br> 
                
                <li> <b>Player scouting </b>attempts  to find similar players to a player selected in the previous tab </li> 
                
                  <ul> 
                  
                    <li> Similar players are found using best stats from player profile tab </li> 
                
                    <li> Using sorted distance to the player, we limit all players to be within certain age rage and  
                        to be featured in certain positions </li> 
                
                    <li> You get to see how many similar players to display, by default, you will see 10 </li> 
                
                  </ul>
                
                </ul> 
                
                <p style='font-size: 24px; font-weight: bold;'>Suggested Use: </p>
                
                <ul> 
                
                <li> I highlighted a few player who are current world stars, so you can copy and paste their names in the 
                    player profile tab to examine their statistics. </li> 
                
                <li> After examining player profile, move on to the similar players tab </li> 
                
                <li> When you find a similar player, take their name, go to the player profile tab, and start all over again </li> 
                
                </ul> 
                
                 <p style='font-size: 18px; font-weight: bold;'>Follow instructions and aqua-colored boxes for extra guidance. Have fun! </p>
                "
              )),
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
                                         
                                                uiOutput('helper_reactive_seaons_ui'), 
                                                uiOutput('helper_reactive_teams_ui'), 
                                         
                                         selectInput(inputId = 'position_filter_helper', 
                                                     label = 'Position', 
                                                     choices = positions_short_names, 
                                                     selected = 'All')
                                         
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
      tabItem(tabName = "player_profile"),
      tabItem(tabName = "two_player_comparison"), 
      tabItem(tabName = "team_profile")
    ))
########################################################################################################################
########################################################################################################################


shinyApp(
  ui = 
    dashboardPage(
      skin = "green", 
      dashboardHeader(
        title =  "FBref Infinity", 
        titleWidth = 250
        ),
      sidebar,
      body
      ),
  server = server_side
)
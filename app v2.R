                                            #####################################
                                            # LOAD DATA, PACKAGES, OTHER SET UP #
                                            #####################################
                                            
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

remove_colnames_keep_games_mins <- c('season', 'summary_player', 'team_name', 'league_name', 'all_positions', 
                     'summary_age', 'dominant_position')

remove_colnames <- c('season', 'summary_player', 'team_name', 'league_name', 'games_played', 'summary_min', 'all_positions', 
                     'summary_age', 'dominant_position')

remove_colnames_dict <- data_dict %>% filter(Data.Frame.Name %in% remove_colnames) %>% select(Pretty.Name.from.FBref) %>% 
  unlist()

standard_box_style = "overflow-y: scroll;overflow-x: scroll;"                         

                                            ##########################################
                                            # FUNCTION TO BE USED ON THE SERVER SIDE #
                                            ##########################################
                                            

display_players <- 
  function(
    FILTER_POS, 
    FILTER_TEAM, 
    FILTER_SEASON, 
    FILTER_LEAGUE
  ){
    display <- 
         
        if(FILTER_POS %in% "All" & 
           FILTER_TEAM %in% "All"){
          
              dash_df[league_name %in% FILTER_LEAGUE &
                       season %in% FILTER_SEASON]
          
          }else if(
            FILTER_POS != "All" & 
            FILTER_TEAM %in% "All"){
            
              dash_df[league_name %in% FILTER_LEAGUE &
                         season %in% FILTER_SEASON & 
                    grepl(FILTER_POS, all_positions)]
            
          }else if(
            FILTER_POS %in% "All" & 
            FILTER_TEAM != "All"){
            
            dash_df[league_name %in% FILTER_LEAGUE &
                    season %in% FILTER_SEASON & 
                    team_name %in% FILTER_TEAM]
          }else if(
            FILTER_POS != "All" & 
            FILTER_TEAM != "All"){
            
            dash_df[league_name %in% FILTER_LEAGUE &
                    season %in% FILTER_SEASON & 
                    team_name %in% FILTER_TEAM  & 
                    grepl(FILTER_POS, all_positions)]
          }
    
    display %>% dplyr::select(summary_player, summary_age, games_played, summary_min, 
                            team_name, all_positions) %>% 
            
            arrange(summary_player) %>% 
            rename(
              `Player Name` = summary_player,
              Age = summary_age, 
              `Games Played` = games_played, 
              `Minutes Played` = summary_min, 
              `Team` = team_name, 
              `Featured Positions (Games Appeared)` = all_positions
            ) %>% 
            unique() %>% 
            datatable()
  }

player_profile_reactive_df <- 
  function(
    RAW_DATA = dash_df, 
    AGGREGATE_FLAG = NA,
    TARGET_PLAYER = NA, 
    TARGET_PLAYER_SEASON = NA, 
    TARGET_PLAYER_TEAM = NA, 
    TARGET_PLAYER_LEAGUE = NA, 
    COMP_LEAGUES = NA, 
    COMP_SEASONS = NA, 
    COMP_POSITIONS = NA, 
    COMP_AGE_START = NA, 
    COMP_AGE_END = NA, 
    COMP_MINUTES_START = NA, 
    COMP_MINUTES_END = NA
  ){
   
    if(AGGREGATE_FLAG == "No"){
      
      # get a row of data for the player we want to summarize and compare with others 
      RAW_DATA %>% 
        filter(
          summary_player %in% TARGET_PLAYER, 
          season %in% TARGET_PLAYER_SEASON, 
          team_name %in% TARGET_PLAYER_TEAM, 
          league_name %in% TARGET_PLAYER_LEAGUE
        ) %>% 
        select(-all_of(remove_colnames_keep_games_mins)) -> target_df
      
      target_df <- 
        cbind(
          
          RAW_DATA %>% 
            filter(
              summary_player %in% TARGET_PLAYER, 
              season %in% TARGET_PLAYER_SEASON, 
              team_name %in% TARGET_PLAYER_TEAM, 
              league_name %in% TARGET_PLAYER_LEAGUE
            ) %>% 
            select(summary_player, league_name, team_name, season, summary_age) , 
          target_df
        )
      
      # get data for the pool of players we compare a target player with 
      RAW_DATA %>% 
        filter(
          season %in% COMP_SEASONS &
          league_name %in% COMP_LEAGUES &
          grepl(
               paste(COMP_POSITIONS, collapse = "|"),
               all_positions) &
          summary_age >= COMP_AGE_START  &
          summary_age <= COMP_AGE_END &
          
          summary_min >= COMP_MINUTES_START &
          summary_min <= COMP_MINUTES_END &
          
          !(
            summary_player %in% TARGET_PLAYER &
            season %in% TARGET_PLAYER_SEASON &
            team_name %in% TARGET_PLAYER_TEAM &
            league_name %in% TARGET_PLAYER_LEAGUE
          )
        ) %>% 
        select(-all_of(remove_colnames_keep_games_mins)) -> comp_df
      
        comp_df <- 
          cbind(
            
            RAW_DATA %>% 
              filter(
                season %in% COMP_SEASONS &
                league_name %in% COMP_LEAGUES &
                grepl(
                     paste(COMP_POSITIONS, collapse = "|"),
                     all_positions) &
                summary_age >= COMP_AGE_START  &
                summary_age <= COMP_AGE_END &
                
                summary_min >= COMP_MINUTES_START &
                summary_min <= COMP_MINUTES_END &
                
                !(
                  summary_player %in% TARGET_PLAYER &
                  season %in% TARGET_PLAYER_SEASON &
                  team_name %in% TARGET_PLAYER_TEAM &
                  league_name %in% TARGET_PLAYER_LEAGUE
                )
              ) %>% 
              select(summary_player, league_name, team_name, season, summary_age) , 
            comp_df
          )
        
        target_df <- 
          rbind(target_df, 
                comp_df)
    }else{
      
      
      players <- RAW_DATA %>% filter(league_name %in% COMP_LEAGUES) %>% select(summary_player) %>% unique() %>% unlist()
      
      player_leagues <- RAW_DATA %>% 
        filter(season %in% COMP_SEASONS &
                 !(league_name %in% c("UEFA Champions League", 
                                      "UEFA Europa Conference League", 
                                      "UEFA Europa League"))) %>% 
        select(summary_player, season, team_name, league_name) %>% unique()
      
      # get a row of data for the player we want to summarize and compare with others 
      RAW_DATA %>% 
        filter(
          summary_player %in% TARGET_PLAYER, 
          season %in% TARGET_PLAYER_SEASON, 
          team_name %in% TARGET_PLAYER_TEAM
        )  %>% 
        select(-all_of(remove_colnames_keep_games_mins)) %>% 
        colSums() %>% 
        t() %>% 
        data.frame()  -> target_df
      
      target_df <- 
        cbind(
          
          RAW_DATA %>% 
            filter(
              summary_player %in% TARGET_PLAYER, 
              season %in% TARGET_PLAYER_SEASON, 
              team_name %in% TARGET_PLAYER_TEAM
            ) %>% 
            select(summary_player, team_name, season) %>% 
            unique(), 
          target_df
        )%>% 
        
        left_join(
          player_leagues,
          by = c("summary_player", "team_name", "season")
        ) %>% select(summary_player,	season,	team_name, league_name, everything())
      
      # other players data in the comparison pool 
      
      RAW_DATA %>% 
        filter(
          season %in% COMP_SEASONS &
          summary_player %in% players &
          grepl(
               paste(COMP_POSITIONS, collapse = "|"),
               all_positions) &
          summary_age >= COMP_AGE_START  &
          summary_age <= COMP_AGE_END &
          
          summary_min >= COMP_MINUTES_START &
          summary_min <= COMP_MINUTES_END &
          
          !(
            summary_player %in% TARGET_PLAYER &
            season %in% TARGET_PLAYER_SEASON &
            team_name %in% TARGET_PLAYER_TEAM
          )
        ) %>% 
        select(-all_of(
           c("league_name","all_positions","summary_age","dominant_position")
        )) %>%
        group_by(
          summary_player, season, team_name
        ) %>% 
          # remove non-numeric columns form aggregation 
        summarise(across(everything(), sum, na.rm =T)) %>% 
        data.frame() %>% 
        
        left_join(
          player_leagues,
          by = c("summary_player", "team_name", "season")
        ) %>% 
        select(summary_player,	season,	team_name, league_name, everything())  -> comp_df
        
      target_df <- rbind(target_df, comp_df) %>% filter(summary_player == "Gabriel Barbosa")
    }
    
    return(target_df)
    
  }

                                            ########################
                                            # LOAD UP SERVER SIDE #
                                            ########################
                                            



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
    
    #### Player Profile Stuff 
        # type in target player name 
    selected_player_profile_name <- 
      eventReactive(
        input$go, {
          input$player_typed_name
        }
        )
    
        # display available competitions, seasons, teams for a selected player name 
    output$tab <- 
      renderDataTable({
        dash_df[dash_df$summary_player == selected_player_profile_name() ] %>% 
          select(summary_age, season, league_name, team_name, all_positions) %>% 
          arrange(desc(season), league_name, team_name, all_positions) %>% 
          mutate(summary_age = as.integer(summary_age)) %>% 
          setNames(c("Age", "Season", "Competition", "Team", "Positions")) %>% 
          datatable(options = list(iDisplayLength = 5,
                                   scrollX = TRUE,
                                   scrollY = TRUE
                                   ), 
                    rownames = F)
      }, 
      striped = TRUE, 
      hover = TRUE)
        # dynamic picker for a player summary team 
        
        ## sometimes we have two + players with the same name playing on different teams. If we type in such a name, we need to be able to 
        ##  select a team 
    
      same_name_teams <- 
        reactive(dash_df[summary_player %in% selected_player_profile_name() ] %>% 
                 select(team_name) %>% unique() %>% unlist() %>% 
                     set_names(NULL))
    
      output$same_name_team_picker <- 
            renderUI({
              
              selectInput(inputId = 'select_team_same_name', 
                           label = "Pick a team. Likely only 1 available", 
                           choices = same_name_teams(), 
                           selected = same_name_teams()[length(same_name_teams())])
          })
     
    
      same_name_leagues <- 
        reactive(dash_df[summary_player %in% selected_player_profile_name() ] %>% 
                 select(league_name) %>% unique() %>% unlist() %>% 
                     set_names(NULL))
    
      output$same_name_leagues_picker <- 
            renderUI({
              
              selectInput(inputId = 'select_league', 
                           label = "Select Competiton", 
                           choices = same_name_leagues(), 
                           selected = same_name_leagues()[length(same_name_leagues())])
          })
       
      player_seasons <- 
        reactive(dash_df[summary_player %in% selected_player_profile_name() ] %>% 
                 select(season) %>% unique() %>% unlist() %>% 
                     set_names(NULL))
    
      
      output$picked_player_available_seasons <- 
            renderUI({
              
              selectInput(inputId = 'select_season', 
                           label = "Select a Season", 
                           choices = player_seasons(), 
                           selected = player_seasons()[length(player_seasons())])
          })
      
      # make sure that we have observer event for the "Update Report" button 

      output$scouter_positions_filter <- 
        renderUI(
          selectInput(inputId = 'scouter_positions', 
                       label = 'Position', 
                       choices = positions_short_names, 
                       selected = 
                        strsplit(
                              gsub("[0-9()]", "", 
                                   dash_df[summary_player == selected_player_profile_name()  ]$all_positions %>% unlist()), 
                              ", ")[[1]], 
                      multiple = T
                        )
        )
      
      ########## PLAYER PROFILE SUMMARY PAGE 
      reactive_player_summary_df <- 
        reactive(
          player_profile_reactive_df(
              RAW_DATA = dash_df, 
              AGGREGATE_FLAG = input$league_cup_combine,
              TARGET_PLAYER = input$player_typed_name, 
              TARGET_PLAYER_SEASON = input$select_season, 
              TARGET_PLAYER_TEAM = input$select_team_same_name , 
              TARGET_PLAYER_LEAGUE = input$select_league, 
              COMP_LEAGUES = input$comp_leagues, 
              COMP_SEASONS = input$comp_seasons, 
              COMP_POSITIONS = input$scouter_positions, 
              COMP_AGE_START = input$similar_player_age_filter[1], 
              COMP_AGE_END = input$similar_player_age_filter[2], 
              COMP_MINUTES_START = input$minutes_to_limit[1], 
              COMP_MINUTES_END = input$minutes_to_limit[2]
            )
        )
      
      output$reactive_player_summary_df <- renderTable(reactive_player_summary_df())
  }

                                            ########################
                                            #   LOAD UP UI SIDE    #
                                            ########################

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
                                                      }
                              /* Custom styles for the tooltip container */
                              .tooltip.bs.tooltip {
                                max-width: 600px; /* Adjust the maximum width as needed */
                              }
                              '
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
      tabItem(tabName = "player_profile", 
              tabsetPanel(
                
                tabPanel(title = "Parameters", 
                         fluidRow(
                          box(width = 12,  
                              
                           column(width = 3, 
                                  actionButton(inputId = "go", 
                                            label = "Find Teams and Seasons", 
                                            width = "200px")),
                             
                             bsTooltip(id = "go", 
                                       title = "Pushing this button updates table of teams, leagues, and seasons where a player name occurs",
                                       placement = "bottom", 
                                       trigger = "hover", 
                                       options = list(container = "body")), 
               
                           column(width = 3, 
                                fluidRow(
                                  bsButton(
                                    inputId = "type_cog", 
                                    label = icon("cog"),
                                    style = "info", 
                                    size = "extra-small"
                                  ),
                                  bsButton(
                                    inputId = "type_question", 
                                    label = icon("question"),
                                    style = "info", 
                                    size = "extra-small"
                                  )
                                  ), 
                                  
                                  bsTooltip("type_cog",
                                    title = "Make sure player name includes all special charachters, refer to the Helper Tab. ",
                                    placement = "right",
                                    trigger = "hover", 
                                    options = list(container = 'body')
                                  ), 
                                bsTooltip("type_question",
                                    title = "Make sure player name has no spaces before or after the name. ",
                                    placement = "right",
                                    trigger = "hover", 
                                    options = list(container = 'body')
                                  ), 
                              
                                  textInput(inputId = 'player_typed_name', 
                                         label = "Type in Player Name", 
                                         value = 'Kevin De Bruyne'),
                                  
                                    uiOutput('same_name_team_picker'), 
                                    uiOutput('same_name_leagues_picker'), 
                                    uiOutput('picked_player_available_seasons'), 
                                
                                    radioButtons(inputId = "league_cup_combine", 
                                                 label = "Combine League and Cups?", 
                                                 choices = c("Yes", "No"), 
                                                 selected = "No"), 
                                
                                bsTooltip(id = "league_cup_combine", 
                                          title = "Aggreagtes season-wise data over leagues and cups. Applies to a player of choice and comparison pool.",
                                          trigger = "hover", 
                                          placement = "left")
                                  ),
                           
                           column(width = 6, 
                                  dataTableOutput('tab'))
                          )
                         ), 
                         fluidRow(
                           box(width = 12, 
                               
                               column(width = 3, 
                                      actionButton(inputId = "generate_report", 
                                                   label = "Generate Reports", 
                                                   width = "200px"), 
                                      
                                      bsTooltip(id = "generate_report", 
                                                placement = "bottom", 
                                                trigger = "hover", 
                                                title = "Pushing this button generates reports on two next tabs based on selected parameters. ")), 
                               
                               column(
                                 width = 3, 
                                 
                                 selectInput(inputId = 'comp_leagues', 
                                     label = "Collect Percentiles Across Leagues:", 
                                     choices = sort(unique(dash_df$league_name)), 
                                     selected = c("Major League Soccer", top_5_leagues), 
                                     multiple = T), 
                                 
                                 selectInput(inputId = 'comp_seasons', 
                                               label = "Select Seasons", 
                                               choices = sort(unique(dash_df$season)), 
                                               selected = c('2022/2023',
                                                            '2023'), 
                                     multiple = T
                                     ), 
                                 
                                 uiOutput('scouter_positions_filter'), 
                                 
                               ), 
                               
                               column(
                                 width = 3, 
                                 
                                 sliderInput(inputId = 'similar_player_age_filter', 
                                  label = "Age Range for Comparisons", 
                                  value = c(20,35), 
                                  min = min(dash_df$summary_age, na.rm = T), 
                                  max = max(dash_df$summary_age, na.rm = T)), 
                                 
                                 sliderInput(inputId = 'minutes_to_limit', 
                                      label = "Limit Players by Minutes Played:", 
                                      min = 0, 
                                      max = max(dash_df$summary_min), 
                                      value = c(350, 4000))
                               )
                               )
                         )),
                
                tabPanel(title = "Player Profile", 
                         tableOutput('reactive_player_summary_df')
                         ),
                
                tabPanel(title = "Similar Players")
                
              )),
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
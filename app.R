

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
dash_df <- dash_df[league_name != "UEFA Champions League"]

data_dict <- read_csv('FBref Advanced Soccer Data Disctionary.csv')[,-1]

    original <- colnames(dash_df)
    reduced <- colnames(dash_df)[ which(colnames(dash_df) %in% data_dict$`Data Frame Name`) ]
    
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


########################################################################################################################
########################################################################################################################


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

## This really needs to be remade 
player_season_summary <- 
  function(DATA, PLAYER, SEASON, TEAM){
    
    
    DATA[summary_player %in% PLAYER & 
           team_name %in% TEAM & 
              season %in% SEASON]$team_name %>% unlist() -> team
    
    DATA[team_name %in% TEAM & 
              season %in% SEASON]$league_name %>% unlist() -> league
    
    DATA[summary_player %in% PLAYER & 
              season %in% SEASON]$team_name %>% unlist() -> team
    
    DATA[summary_player %in% PLAYER & 
              season %in% SEASON]$summary_age %>% unlist() -> age
    
    DATA[league_name %in% league & 
              season %in% SEASON]$summary_age %>% unlist() %>% sort() -> all_ages
    
    length(which(all_ages <= age))/length(all_ages) -> age_percentile
    age_percentile <- paste0(100*round(age_percentile, 2), "%")
    
    DATA[summary_player %in% PLAYER & 
              season %in% SEASON]$games_played %>% unlist()  -> games 
    
    DATA[league_name %in% league & 
              season %in% SEASON]$games_played %>% unlist() %>% sort() -> all_games
    
    length(which(all_games <= games))/length(all_games) -> games_percentile
    games_percentile <- paste0(100*round(games_percentile, 2), "%")
    
    DATA[summary_player %in% PLAYER & 
              season %in% SEASON]$summary_min %>% unlist()  -> mins
    
    DATA[league_name %in% league & 
              season %in% SEASON]$summary_min %>% unlist() %>% sort() -> all_minutes
    
    length(which(all_minutes <= mins))/length(all_minutes) -> min_percentile
    min_percentile <- paste0(100*round(min_percentile, 2), "%")
    
    DATA[summary_player %in% PLAYER & 
              season %in% SEASON]$all_positions %>% unlist()  -> pos
    
    
    out <- 
      data.frame(
        `Player Name` = PLAYER, 
        `Age(League Pecentile)` = paste0(age, " (", age_percentile, ")"), 
        `Games(League Pecentile)` = paste0(games, " (", games_percentile, ")"),
        `Minutes(League Pecentile)` = 
          paste0(
            prettyNum(mins, big.mark = ","), 
            " (", min_percentile, ")"
            ), 
        `Position` = pos
      )
    
    rownames(out) = NULL
    colnames(out) = c('Player Name', 'Age(League Pecentile)', 'Games(League Pecentile)', 'Minutes(League Pecentile)', 
                      'Positions')
    return(out)
  }

percentile_data_frame_one_player <- 
  function(
    DATA, 
    PLAYER, 
    TEAM, 
    SEASON,
    MINUTES_FILTER,
    COMP_LEAGUES
  ){
    
    # selected player data 
    player_df <-  DATA[summary_player %in% PLAYER & season %in% SEASON  & team_name %in% TEAM ] 
    player_min <- sum(player_df$summary_min) 
    
    player_df <- player_df %>% dplyr::select(-all_of(remove_colnames)) %>% 
      summarise(across(everything(), sum))
    
    player_df_per_90 <- player_df
    player_df_per_90 <- player_df_per_90 %>% mutate_all(~. / player_min * 90)
    
    # other players data, which we compare our selected player to 
    league_stat <- DATA[league_name %in% COMP_LEAGUES & season %in% SEASON & summary_min >= MINUTES_FILTER]
    league_stat <- 
      league_stat %>% 
        group_by(summary_player) %>% 
        dplyr::select(-all_of(setdiff(remove_colnames, "summary_min"))) %>%  ## keep summary minutes in the data for now
        summarise(across(everything(), sum))
    
    league_stat <- na.omit(league_stat) # first remove observations and then create minutes vector 
    minutes <- league_stat$summary_min
    
#    colnames(league_stat)[which(colnames(league_stat) %in% remove_colnames)]
    
    league_stat <- league_stat %>% dplyr::select(-all_of(colnames(league_stat)[which(colnames(league_stat) %in% remove_colnames)]))
    
    league_stat_per_90 <- league_stat 
    league_stat_per_90 <- league_stat_per_90 %>% mutate_all(~. / minutes * 90)
    
    # percentiles for total aggregated statistics
    player_stat <- player_df %>% unlist()
    
    player_df_percentile <- 
      league_stat %>% 
        summarise(across(everything(), 
                         # calculate percentiles using unique values of metrics of other players 
                         ~sum(unique(.) <= player_stat[match(cur_column(), names(league_stat))])/length(unique(.))
                         )
                  ) -> int_res 
    
    
    names <- names(int_res)
    percentiles <- int_res %>% t()
    
    
    # percentiles for aggregated per 90
    player_stat_per_90 <- player_df_per_90 %>% unlist()
    
    player_df_percentile_per_90 <- 
      league_stat_per_90 %>% 
        summarise(across(everything(), 
                         # calculate percentiles using unique values of metrics of other players 
                         ~sum(unique(.) <= player_stat_per_90[match(cur_column(), names(league_stat_per_90))])/length(unique(.))
                         )
                  ) -> int_res2
  
    percentiles_per_90 <- int_res2 %>% t()
    
    f <- data.frame(
      names, 
      player_stat, 
      percentiles,
      player_stat_per_90, 
      percentiles_per_90
    )  
    
    return(f)
  }

find_player_features <- 
  function(
    REACTIVE_DATA, 
    N_FEATURES, 
    RETURN_VECTOR, 
    BEST_OR_WORST){
    
    
    if(RETURN_VECTOR == "Y"){
      return(REACTIVE_DATA %>%  arrange(-percentiles_per_90) %>% dplyr::select(names) %>%  unlist() %>% head(N_FEATURES))
    }
    
    if(RETURN_VECTOR == "N" & BEST_OR_WORST == "BEST"){
      
      print_res <-
        REACTIVE_DATA %>% 
        arrange(-percentiles_per_90) %>%
        
        head(N_FEATURES)

      rownames(print_res) <- NULL

      return(
        datatable(print_res,rownames=FALSE, 
                  options = list(iDisplayLength = N_FEATURES), 
                  caption = "Best Metrics for Selected Player by Percentile Per 90 Minutes") %>% 
          formatRound(columns = c(2:length(print_res)), 
                      digits = 2)
        )
    }
    
    if(RETURN_VECTOR == "N" & BEST_OR_WORST == "WORST"){
      
      print_res <-
        REACTIVE_DATA %>% 
        arrange(percentiles_per_90) %>%
        
        head(N_FEATURES)

      rownames(print_res) <- NULL

      return(
        datatable(print_res,
                  rownames=FALSE, 
                  options = list(iDisplayLength = N_FEATURES), 
                  caption = "Worst Metrics for Selected Player by Percentile Per 90 Minutes") %>% 
          formatRound(columns = c(2:length(print_res)), 
                      digits = 2)
        )
      
    }
    
  }


all_features_quantiles_density <- 
  function(
    REACTIVE_DATA
  ){
    
    f <- REACTIVE_DATA
    
    f$stat_cat <- as.factor(sub("_.*", "", f$names))

    f_plot <- 
      f %>% 
      group_by(stat_cat) %>% 
      summarize(
        dens_x =  density(percentiles_per_90)$x, 
        dens_y =  density(percentiles_per_90)$y/ sum(density(percentiles_per_90)$y)
      )
  
    with(f_plot, 
    f_plot %>% 
      plot_ly(
        x = ~dens_x, 
        y = ~dens_y, 
        color = ~stat_cat, 
        type = "scatter", 
        mode = 'lines', 
        text = 
          paste(
            "Category: ", stat_cat, 
            "<br>Approx. Percentile: ", round(dens_x, 6),
            "<br>Approx. Probability: ", round(dens_y, 6)
            
          ), 
        hoverinfo = 'text'
        
      ) %>% 
      layout(xaxis = list(range = c(0, 1), zeroline = F))
    )

    
  }

dynamic_table_summary <- 
  function(
    REACTIVE_DATA,  
    COLUMNS){

    f <- REACTIVE_DATA[names %in% COLUMNS,]
    rownames(f) <- NULL
    
    colnames(f) <- c("name", "st", "p_st", "st_90", "p_st_90")
    
    datatable(f, rownames=FALSE,
              options = list(autoWidth = TRUE,
                             columnDefs = list(list(width = '10px', targets = list(2,3,4))))) %>% 
      
      formatRound(columns = c(2:length(f)), 
                      digits = 2)
    
  }

pizza_chart <- 
  function(
    REACTIVE_DATA, 
    COLUMNS
    ){
    
    f <- REACTIVE_DATA[names %in% COLUMNS,]
    f$stat_cat <- as.factor(sub("_.*", "", f$names))
    ##################
    # https://www.gettingbluefingers.com/tutorials/radarpizzachart/
        
  ggplot(
    data = f,
    aes(x = names,
        y = percentiles_per_90)) +                       #select the columns to plot and sort it so the types of metric are grouped
  
      geom_bar(
        aes(y=1,
            fill=stat_cat),
        stat="identity",
        width=1,
        colour="white",                 #make the whole pizza first
        alpha=0.5) +     
      
  geom_bar(stat="identity"
           ,width=1,
           aes(fill=stat_cat),
           colour="white") +                     #insert the values 
  coord_polar() +  
      
  geom_text(aes(label=round(player_stat_per_90,2)), size=7, color = "black")    +                               
  scale_y_continuous(limits = c(-.10,1))+                                              #create the white part in the middle.   
      
  theme_minimal() +                                                                     #from here it's only themeing. 
  theme(plot.background = element_rect(fill = "white",color = "white"),
        panel.background = element_rect(fill = "white",color = "white"),
   #     text = element_text(size = 25), 
        legend.position = "top",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 12, angle = 0),
        plot.subtitle = element_text(hjust=0.5,size=12),
        plot.caption = element_text(hjust=0.5,size=12),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.margin = margin(1,1,1,1)) 
    
    
      }


similar_players <- 
  function(
    DATA, 
    PLAYER, 
    TEAM, 
    SEASON, 
    MINUTES_FILTER, 
    TARGET_SIMILAR_PLAYERS,
    FEATURES_LIST, 
    COMP_LEAGUES, 
    RETURN_VECTOR, 
    AGE_FILTER1,
    AGE_FILTER2
  ){
    
    df_player <- DATA[summary_player %in% PLAYER & season %in% SEASON  & team_name %in% TEAM ] %>% 
      
      select(all_of(
          c(FEATURES_LIST, "summary_player", "summary_min", 'team_name', 'summary_age', 'all_positions')
        ))
    
    team <- df_player$team_name
    player_min <- df_player$summary_min
    
    df <- DATA[league_name %in% COMP_LEAGUES & season %in% SEASON & summary_min >= MINUTES_FILTER & team_name != team] %>% 
      
      select(all_of(
          c(FEATURES_LIST, "summary_player", "summary_min", 'team_name', 'summary_age', 'all_positions')
        ))
    
    players = df$summary_player
    minutes = df$summary_min
    team_name = df$team_name
    league_name = df$league_name
    all_positions  = df$all_positions
    summary_age = df$summary_age
    
    df <- df %>% select(-summary_player, -summary_min, -team_name, -all_positions, -summary_age)
    df_player <- df_player %>% select(-summary_player, -summary_min, -team_name, -all_positions, -summary_age)
    
    df <- df %>% mutate_all(~. / minutes * 90)
    df_player <- df_player %>% mutate_all(~. / player_min * 90)
    
    df_player %>% unlist() -> vals_for_distance
    
    f <- df %>% mutate(across(.cols = everything(), ~ (. - vals_for_distance[match(cur_column(), names(df))])^2))
    
    f <- f %>% mutate(sum = sqrt(rowSums(.)))
    
    f$scaled_dist <- with(f, (sum - 0) / (max(sum) - 0))
      
    f <- cbind(f, players, minutes, team_name, league_name, all_positions, summary_age)
    
    f <- f %>% arrange(scaled_dist)
    
    if(RETURN_VECTOR == "N"){
      return(f %>% 
               select(players, league_name, team_name, minutes, all_positions, summary_age, scaled_dist) %>% 
               unique() %>% 
               filter(summary_age >= AGE_FILTER1 & summary_age <= AGE_FILTER2) %>% 
               head(TARGET_SIMILAR_PLAYERS))
    }
    
    if(RETURN_VECTOR == "Y"){
      return(f %>% filter(summary_age >= AGE_FILTER1 & summary_age <= AGE_FILTER2) %>% head(TARGET_SIMILAR_PLAYERS) %>% 
               select(players) %>% unique() %>% unlist())
    }
  }

similar_players_pca_plot <- 
  function(
    DATA, 
    PLAYER, 
    TEAM, 
    SEASON, 
    MINUTES_FILTER, 
    FEATURES_LIST, 
    COMP_LEAGUES,
    COLOR) {
    
      df_player <- DATA[summary_player %in% PLAYER & season %in% SEASON & team_name %in% TEAM ] %>% 
        
        select(all_of(
            c(FEATURES_LIST, "summary_player", "summary_min", 'team_name', "league_name", 'all_positions', 'summary_age')
          ))
      
      player <- df_player$summary_player
      team <- df_player$team_name
      player_min <- df_player$summary_min
      player_league <- df_player$league_name
      player_positions  = df_player$all_positions
      player_age = df_player$summary_age
    
      df <- DATA[league_name %in% COMP_LEAGUES & season %in% SEASON & summary_min >= MINUTES_FILTER & team_name != team] %>% 
        
        select(all_of(
            c(FEATURES_LIST, "summary_player", "summary_min", 'team_name', "league_name", 'all_positions', 'summary_age')
          ))
      
      players = df$summary_player
      minutes = df$summary_min
      team_name = df$team_name
      league_name = df$league_name
      all_positions  = df$all_positions
      summary_age = df$summary_age
      
      df <- df %>% select(-summary_player, -summary_min, -team_name, -league_name, -all_positions, -summary_age)
      df_player <- df_player %>% select(-summary_player, -summary_min, -team_name, -league_name, -all_positions, -summary_age)
      
      df <- df %>% mutate_all(~. / minutes * 90)
      df_player <- df_player %>% mutate_all(~. / player_min * 90)
    
      df_all <- rbind(df, df_player)
      
      set.seed(1)
      pca_res <- prcomp(df_all, scale. = T)
      
      percent_1 <- data.frame(summary(pca_res)[6])[2,1]
      percent_2 <- data.frame(summary(pca_res)[6])[2,2]
      
      plot_df <- 
        data.frame(
          PC1 = pca_res$x[,1],
          PC2 = pca_res$x[,2],
          summary_player = c(players, player), 
          team_name = c(team_name, team), 
          league_name = c(league_name, player_league), 
          minutes = c(minutes, player_min)
        )
      
      play_df <- plot_df %>% filter(summary_player %in% PLAYER & team_name %in% TEAM)
      similar <- plot_df %>% filter(summary_player %in% COLOR)
      
      plot_df <- plot_df %>% filter(!((summary_player %in% play_df$summary_player & team_name %in% TEAM ) | 
                                        summary_player %in% similar$summary_player))
      
        with(plot_df, 
             plot_df %>% 
                plot_ly() %>% 
                add_trace(
                  x = ~PC1, 
                  y = ~PC2, 
                  color = ~league_name, 
                  type = "scatter", 
                  mode = "markers", 
                  
                  opacity = 0.5, 
                  
                  text = paste(
                    "Player: ", summary_player, 
                    "<br>Team: ", team_name, 
                    "<br>League: ", league_name
                  ), 
                  hoverinfo = 'text'
                ) %>% 
               
               add_trace(
                 x = ~play_df$PC1, 
                 y = ~play_df$PC2, 
                 type = "scatter", 
                 mode = "markers", 
                 marker = list(color = 'purple', size = 10), 
                 opacity = .75, 
                 name = PLAYER, 
                 text = paste(
                    "Player: ", play_df$summary_player, 
                    "<br>Team: ", play_df$team_name, 
                    "<br>League: ", play_df$league_name
                  ), 
                 hoverinfo = 'text'
               ) %>% 
               
               add_trace(
                 x = ~similar$PC1, 
                 y = ~similar$PC2, 
                 type = "scatter", 
                 mode = "markers", 
                 marker = list(color = 'red', size = 8), 
                 opacity = .75, 
                 name = paste0("Players similar to ", PLAYER), 
                 text = paste(
                    "Player: ", similar$summary_player, 
                    "<br>Team: ", similar$team_name, 
                    "<br>League: ", similar$league_name
                  ), 
                 hoverinfo = 'text'
               ) %>% 
               
               layout(xaxis = list(title = paste0("PC1 Proportion of Variance: ", round(percent_1, 4)*100, "%")),
                      yaxis = list(title = paste0("PC2 Proportion of Variance: ", round(percent_2, 4)*100, "%"))
                      )
        )
  }

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
         
    ## PLAYER PROFILE STUFF 

    ## sometimes we have two + players with the same name playing on different teams. If we type in such a name, we need to be able to 
    ##  select a team 
    
    same_name_teams <- 
      reactive(dash_df[summary_player %in% input$player_typed_name & 
                         season %in% input$select_season] %>% 
                 select(team_name) %>% unique() %>% unlist() %>% sort() %>% 
                     set_names(NULL))
    
      output$same_name_team_picker <- 
            renderUI({
              
              selectInput(inputId = 'select_team_same_name', 
                           label = "Pick a team. Likely only 1 available", 
                           choices = same_name_teams(), 
                           selected = same_name_teams()[length(same_name_teams())])
          })
    
    output$player_season_summary <- 
      renderTable(
        player_season_summary(
          PLAYER = input$player_typed_name, 
          SEASON = input$select_season,
          DATA = dash_df,
          TEAM = input$select_team_same_name)
      )
        
    percentiles_data <- 
      reactive(
        percentile_data_frame_one_player(
          DATA = dash_df,
          PLAYER = input$player_typed_name,
          TEAM = input$select_team_same_name, 
          SEASON = input$select_season,
          MINUTES_FILTER = input$minutes_to_limit,
          COMP_LEAGUES = input$comp_leagues
        )
      )
    
    ##### two tables for player profile: best and worst statistics measured by percentiles per 90 
    output$best_player_features <-
      renderDataTable(
        find_player_features(
          REACTIVE_DATA = percentiles_data(),
          N_FEATURES = input$top_values_number,
          RETURN_VECTOR = "N",
          BEST_OR_WORST = "BEST"
      ))
    
    output$worst_player_features <-
      renderDataTable(
        find_player_features(
          REACTIVE_DATA = percentiles_data(),
          N_FEATURES = input$top_values_number,
          RETURN_VECTOR = "N",
          BEST_OR_WORST = "WORST"
      ))
    
    output$all_features_quantiles_density <- 
      renderPlotly(
        all_features_quantiles_density(
          REACTIVE_DATA = percentiles_data()
        )
      )
    
    output$dynamic_table_summary <- 
      renderDataTable(
        dynamic_table_summary(
          REACTIVE_DATA =  percentiles_data(), 
          COLUMNS = input$feature
        )
      )
    
    output$pizza_chart <- 
      renderPlot(
        pizza_chart(
          COLUMNS = input$feature, 
          REACTIVE_DATA = percentiles_data()

        )
      )
    
    best_player_features_vec <-
      reactive(
        find_player_features(
          DATA = dash_df,
          PLAYER = input$player_typed_name,
          TEAM = input$select_team_same_name, 
          SEASON = input$select_season,
          N_FEATURES = input$top_values_number,
          MINUTES_FILTER = input$minutes_to_limit,
          RETURN_VECTOR = "Y",
          BEST_OR_WORST = "", 
          COMP_LEAGUES = input$comp_leagues
      ))
    
    output$similar_players <- 
      renderTable(
        similar_players(
          DATA = dash_df, 
          PLAYER = input$player_typed_name,
          TEAM = input$select_team_same_name, 
          SEASON = input$select_season, 
          MINUTES_FILTER = input$minutes_to_limit,
          TARGET_SIMILAR_PLAYERS = input$target_sim_players, 
          FEATURES_LIST = best_player_features_vec(), 
          COMP_LEAGUES = input$comp_leagues, 
          RETURN_VECTOR = "N", 
          AGE_FILTER1 = input$similar_player_age_filter[1],
          AGE_FILTER2 = input$similar_player_age_filter[2]
          )
        )
    
    similar_players_vector <- 
      reactive(
        similar_players(
          DATA = dash_df, 
          PLAYER = input$player_typed_name,
          TEAM = input$select_team_same_name, 
          SEASON = input$select_season, 
          MINUTES_FILTER = input$minutes_to_limit,
          TARGET_SIMILAR_PLAYERS = input$target_sim_players, 
          FEATURES_LIST = best_player_features_vec(), 
          COMP_LEAGUES = input$comp_leagues, 
          RETURN_VECTOR = "Y", 
          AGE_FILTER1 = input$similar_player_age_filter[1],
          AGE_FILTER2 = input$similar_player_age_filter[2]
          )
      )
    
    output$similar_players_pca_plot <- 
      renderPlotly(
        similar_players_pca_plot(
          DATA = dash_df, 
          PLAYER = input$player_typed_name,
          TEAM = input$select_team_same_name, 
          SEASON = input$select_season, 
          MINUTES_FILTER = input$minutes_to_limit,
          FEATURES_LIST = best_player_features_vec(), 
          COMP_LEAGUES = input$comp_leagues, 
          COLOR = similar_players_vector()
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
      ,menuItem("Player Profile Junk", tabName = "player_profile_junk")
      ,menuItem("Similar Player Scouting", tabName = "player_scouting")
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
              fluidRow(class = "text-center", 
                 
                       #################
                       # IDENTIFY A PLAYER AND DISPLAYER THEIR BASIC BASIC PLAYING TIME AND AGE 
                       box(textInput(inputId = 'player_typed_name', 
                                   label = "Type in Player Name", 
                                   value = 'Kevin De Bruyne'), width = 4), 
                         
                        box( uiOutput('same_name_team_picker'), width = 4), 
                
                        
                        box( selectInput(inputId = 'select_season', 
                                               label = "Select a Season", 
                                               choices = sort(unique(dash_df$season)), 
                                               selected = c('2022/2023',
                                                            '2023'), 
                                     multiple = T
                                     ), width = 4
                             ), 
                       box(tableOutput('player_season_summary'), width = 12, collapsible = T), 
                       #################
                       # CREATE TABLES WITH BEST AND WORST STATS BY PERCENTILES 
                       box(
                         numericInput(inputId = 'top_values_number', 
                                      label = "Number of Top Features to List:", 
                                      min = 0, 
                                      max = 100, 
                                      value = 15), width = 4), 
                         
                        box(
                         numericInput(inputId = 'minutes_to_limit', 
                                      label = "Limit Players by Minutes Played:", 
                                      min = 0, 
                                      max = max(dash_df$summary_min), 
                                      value = 1000), width = 4), 
                         
                      box(
                        selectInput(inputId = 'comp_leagues', 
                                     label = "Collect Percentiles Across Leagues:", 
                                     choices = sort(unique(dash_df$league_name)), 
                                     selected = top_5_leagues, 
                                     multiple = T), width = 4), 
                      box(
                        tabsetPanel(
                          tabPanel(title = "Best Qualities", 
                                   dataTableOutput('best_player_features')),
                          tabPanel(title = "Worst Qualities", 
                                   dataTableOutput('worst_player_features'))
                        ), width = 9), 
                      box("some text to help explain the page", width = 3), 
                      box(
                        plotlyOutput('all_features_quantiles_density')
                        ,width = 12
                      ), 
                      box(dataTableOutput('dynamic_table_summary'), width = 6, 
                          style = "height:500px; overflow-y: scroll;overflow-x: scroll;"),
                               
                      box(plotOutput('pizza_chart'), width = 6),
                       
                    
                        
              )), 
      
      tabItem(tabName = "player_profile_junk",
              fluidRow(
                column(3,
                       box(sidebarMenu(
                        
                         textInput(inputId = 'player_typed_name', 
                                   label = "Type in Player Name", 
                                   value = 'Kevin De Bruyne'),
                         
                         
                         selectInput(inputId = 'select_season', 
                                               label = "Select a Season", 
                                               choices = sort(unique(dash_df$season)), 
                                               selected = c('2022/2023',
                                                            '2023'), 
                                     multiple = T
                                     ), 
                         
                         selectInput(inputId="feature", 
                                     selected = c(
                                                  'summary_performance_gls', 
                                                  'summary_expected_xg', 
                                                  
                                                  'passing_total_totdist', 
                                                  'passing_total_cmp', 
                                                  
                                                  'defensive_tackles_tkl', 
                                                  'defensive_challenges_att', 
                                                  
                                                  'posession_carries_mis', 
                                                  'posession_carries_prgc'
                                                  ),
                                     label="Choose Variables for Table",
                                     choices=  
                                       setdiff( # remove some columns from options here 
                                         colnames(dash_df), 
                                         remove_colnames
                                       ),
                                     multiple=TRUE), 
                         
                         
                         
                        sliderInput(inputId = 'similar_player_age_filter', 
                                      label = "Age Range for Comparisons", 
                                      value = c(23,27), 
                                      min = min(dash_df$summary_age, na.rm = T), 
                                      max = max(dash_df$summary_age, na.rm = T)), 
                      
                         numericInput(inputId = 'target_sim_players', 
                                      label = "Similar Players to Find", 
                                      min = 0, 
                                      max = 100, 
                                      value = 20)
               
                         
                         
                       ), width = 12 
                       )
                ),
                column(9,
                       fluidRow(
                                box(tableOutput('similar_players'), width = 12), 
                                box(plotlyOutput('similar_players_pca_plot'), width = 12)
                                
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

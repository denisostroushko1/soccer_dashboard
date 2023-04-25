

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
         
        if(FILTER_POS == "All" & 
           FILTER_TEAM == "All"){
          
              dash_df[league_name == FILTER_LEAGUE &
                       season == FILTER_SEASON]
          
          }else if(
            FILTER_POS != "All" & 
            FILTER_TEAM == "All"){
            
              dash_df[league_name == FILTER_LEAGUE &
                         season == FILTER_SEASON & 
                    grepl(FILTER_POS, all_positions)]
            
          }else if(
            FILTER_POS == "All" & 
            FILTER_TEAM != "All"){
            
            dash_df[league_name == FILTER_LEAGUE &
                    season == FILTER_SEASON & 
                    team_name == FILTER_TEAM]
          }else if(
            FILTER_POS != "All" & 
            FILTER_TEAM != "All"){
            
            dash_df[league_name == FILTER_LEAGUE &
                    season == FILTER_SEASON & 
                    team_name == FILTER_TEAM  & 
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

header_message <- 
  function(
    DATA, 
    PLAYER, 
    TEAM, 
    SEASON){
    
    age = DATA[
      summary_player == PLAYER & 
        season %in% SEASON  & team_name == TEAM 
    ]$summary_age
    
    g = DATA[
      summary_player == PLAYER & 
        season %in% SEASON  & team_name == TEAM 
    ]$games_played
    
    dom_pos = DATA[
      summary_player == PLAYER & 
        season %in% SEASON  & team_name == TEAM 
    ]$dominant_position
    
    dom_pos <- gsub(" ", "", dom_pos)
    p <- gsub("[^0-9]", "", dom_pos)

    
    m <- 
      paste0(PLAYER, " (", TEAM, ") is ", age, " years old. Their primary position is ",  dom_pos, ", where they featured in ", 
             p, "% of their total (", g,") league games in ", SEASON)
    
    HTML(paste0("<h5>", m))
    
  }

## This really needs to be remade 
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
    TEAM, 
    SEASON, 
    COLUMNS){

    func_df <- DATA[
      summary_player == PLAYER & 
        season %in% SEASON  & team_name == TEAM 
    ] %>% dplyr::select(all_of(c("summary_min", "games_played",COLUMNS))) %>% 
      summarise(across(everything(), sum))
    
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

find_player_features <- 
  function(
    DATA, 
    PLAYER, 
    TEAM, 
    SEASON,
    N_FEATURES, 
    MINUTES_FILTER, 
    RETURN_VECTOR, 
    COMP_LEAGUES, 
    BEST_OR_WORST){
    
    
    # selected player data 
    player_df <-  DATA[summary_player == PLAYER & season %in% SEASON  & team_name == TEAM ] 
    player_min <- sum(player_df$summary_min) 
    
    player_df <- player_df %>% dplyr::select(-all_of(remove_colnames)) %>% 
      summarise(across(everything(), sum))
    
    player_raw_stat <- player_df %>% unlist()
    player_df <- player_df %>% mutate_all(~. / player_min * 90)
    
    # other players data, which we compare our selected player to 
    league_stat <- DATA[league_name %in% COMP_LEAGUES & season %in% SEASON & summary_min >= MINUTES_FILTER]
    league_stat <- 
      league_stat %>% 
        group_by(summary_player) %>% 
        dplyr::select(-all_of(setdiff(remove_colnames, "summary_min"))) %>%  ## keep summary minutes in the data for now
        summarise(across(everything(), sum))
      
    minutes <- league_stat$summary_min
    
    colnames(league_stat)[which(colnames(league_stat) %in% remove_colnames)]
    
    league_stat <- league_stat %>% dplyr::select(-all_of(colnames(league_stat)[which(colnames(league_stat) %in% remove_colnames)]))
    league_stat <- league_stat %>% mutate_all(~. / minutes * 90)
    
    # now I need to grab percentiles of player's per 90 statistics using data of selected plaeyrs 
    player_stat <- player_df %>% unlist()
    player_df_percentile <- 
      league_stat %>% 
        summarise(across(everything(), 
                         # calculate percentiles using unique values of metrics of other players 
                         ~sum(unique(.) <= player_stat[match(cur_column(), names(league_stat))])/length(unique(.))
                         )
                  ) -> int_res 
    
    names <- names(int_res)
    int_res <- int_res %>% t()
    
    f <- data.frame(
      names, 
      player_raw_stat, 
      player_stat, 
      int_res 
    ) 

    if(RETURN_VECTOR == "Y"){
      return(f %>%  arrange(-int_res) %>% dplyr::select(names) %>%  unlist() %>% head(N_FEATURES))
    }
    
    if(RETURN_VECTOR == "N" & BEST_OR_WORST == "BEST"){
      
      print_res <-
        f %>% arrange(-int_res) %>%
          mutate(
            player_stat = round(player_stat, 4),
            int_res = round(int_res, 6)
            ) %>%
          rename(percentile = int_res) %>%
          head(N_FEATURES)

      rownames(print_res) <- NULL

      return(datatable(print_res))
    }
    
    if(RETURN_VECTOR == "N" & BEST_OR_WORST == "WORST"){
      
      print_res <- 
        f %>% arrange(int_res) %>%  
          mutate(
            player_stat = round(player_stat, 4),
            int_res = round(int_res, 6)
            ) %>%
          rename(percentile = int_res) %>% 
          head(N_FEATURES) 
     
      rownames(print_res) <- NULL
      
      return(datatable(print_res))
      
    }
    
  }


all_features_quantiles_density <- 
  function(
    DATA, 
    PLAYER, 
    TEAM, 
    SEASON,
    MINUTES_FILTER, 
    COMP_LEAGUES
  ){
    
    
    # selected player data 
    player_df <-  DATA[summary_player == PLAYER & season %in% SEASON  & team_name == TEAM ] 
    player_min <- sum(player_df$summary_min) 
    
    player_df <- player_df %>% dplyr::select(-all_of(remove_colnames)) %>% 
      summarise(across(everything(), sum))
    
    player_raw_stat <- player_df %>% unlist()
    player_df <- player_df %>% mutate_all(~. / player_min * 90)
    
    # other players data, which we compare our selected player to 
    league_stat <- DATA[league_name %in% COMP_LEAGUES & season %in% SEASON & summary_min >= MINUTES_FILTER]
    league_stat <- 
      league_stat %>% 
        group_by(summary_player) %>% 
        dplyr::select(-all_of(setdiff(remove_colnames, "summary_min"))) %>%  ## keep summary minutes in the data for now
        summarise(across(everything(), sum))
      
    minutes <- league_stat$summary_min
    
#    colnames(league_stat)[which(colnames(league_stat) %in% remove_colnames)]
    
    league_stat <- league_stat %>% dplyr::select(-all_of(colnames(league_stat)[which(colnames(league_stat) %in% remove_colnames)]))
    league_stat <- league_stat %>% mutate_all(~. / minutes * 90)
    
    # now I need to grab percentiles of player's per 90 statistics using data of selected plaeyrs 
    player_stat <- player_df %>% unlist()
    player_df_percentile <- 
      league_stat %>% 
        summarise(across(everything(), 
                         # calculate percentiles using unique values of metrics of other players 
                         ~sum(unique(.) <= player_stat[match(cur_column(), names(league_stat))])/length(unique(.))
                         )
                  ) -> int_res 
    
    names <- names(int_res)
    int_res <- int_res %>% t()
    
    f <- data.frame(
      names, 
      player_raw_stat, 
      player_stat, 
      int_res 
    ) 
    
    f$stat_cat <- sub("_.*", "", f$names)

    f_plot <- 
      f %>% 
      group_by(stat_cat) %>% 
      summarize(
        dens_x =  density(int_res)$x, 
        dens_y =  density(int_res)$y/ sum(density(int_res)$y)
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

pizza_chart <- 
  function(
    DATA, 
    COLUMNS,
    PLAYER, 
    SEASON, 
    TEAM, 
    MINUTES_FILTER, 
    COMP_LEAGUES
    ){
    
        # selected player data 
    player_df <-  DATA[summary_player == PLAYER & season %in% SEASON  & team_name == TEAM ] 
    player_min <- sum(player_df$summary_min) 
    
    player_df <- player_df %>% dplyr::select(-all_of(remove_colnames)) %>% 
      summarise(across(everything(), sum))
    
    player_raw_stat <- player_df %>% unlist()
    player_df <- player_df %>% mutate_all(~. / player_min * 90)
    
    # other players data, which we compare our selected player to 
    league_stat <- DATA[league_name %in% COMP_LEAGUES & season %in% SEASON & summary_min >= MINUTES_FILTER]
    league_stat <- 
      league_stat %>% 
        group_by(summary_player) %>% 
        dplyr::select(-all_of(setdiff(remove_colnames, "summary_min"))) %>%  ## keep summary minutes in the data for now
        summarise(across(everything(), sum))
      
    minutes <- league_stat$summary_min
    
#    colnames(league_stat)[which(colnames(league_stat) %in% remove_colnames)]
    
    league_stat <- league_stat %>% dplyr::select(-all_of(colnames(league_stat)[which(colnames(league_stat) %in% remove_colnames)]))
    league_stat <- league_stat %>% mutate_all(~. / minutes * 90)
    
    # now I need to grab percentiles of player's per 90 statistics using data of selected plaeyrs 
    player_stat <- player_df %>% unlist()
    player_df_percentile <- 
      league_stat %>% 
        summarise(across(everything(), 
                         # calculate percentiles using unique values of metrics of other players 
                         ~sum(unique(.) <= player_stat[match(cur_column(), names(league_stat))])/length(unique(.))
                         )
                  ) -> int_res 
    
    names <- names(int_res)
    int_res <- int_res %>% t()
    
    f <- data.frame(
      names, 
      player_raw_stat, 
      player_stat, 
      int_res 
    ) %>% filter(names %in% COLUMNS) 
    
    f$stat_cat <- sub("_.*", "", f$names)
    
    ##################
    # https://www.gettingbluefingers.com/tutorials/radarpizzachart/
        
  ggplot(
    data = f,
    aes(x = names,
        y = int_res)) +                       #select the columns to plot and sort it so the types of metric are grouped
  
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
      
  # geom_label(aes(label=
  #                  paste0(round(player_stat,2), "(", round(int_res, 2), "th)"), 
  #                fill=stat_cat, 
  #                ),
  #            size=2,color="white",
  #            show.legend = FALSE
  #            ) +      #add a label for the value. Change 'label=Per.90' to 'label=Percentile' to show the percentiles
  geom_text(aes(label=round(player_stat,2)), size=7, color = "black")    +                               
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
        plot.margin = margin(5,2,2,2)) 
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
    
    df_player <- DATA[summary_player == PLAYER & season %in% SEASON  & team_name == TEAM ] %>% 
      
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
    
      df_player <- DATA[summary_player == PLAYER & season %in% SEASON & team_name == TEAM ] %>% 
        
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
      
      play_df <- plot_df %>% filter(summary_player == PLAYER & team_name == TEAM)
      similar <- plot_df %>% filter(summary_player %in% COLOR)
      
      plot_df <- plot_df %>% filter(!((summary_player %in% play_df$summary_player & team_name == TEAM ) | 
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
    
    output$data_dict <- 
      renderDataTable(
        datatable(data_dict)
      )
      
    output$header_message <- 
      renderUI(
        header_message(
          DATA = dash_df, 
          TEAM = input$select_team_same_name, 
          PLAYER = input$player_typed_name, 
          SEASON = input$select_season)
      )
    
    output$game_and_min_summary <- 
      renderTable(
        game_and_min_summary(
          PLAYER = input$player_typed_name, 
          SEASON %in% input$select_season,
          DATA = dash_df)
      )
        
        helper_reactive_seaons <- 
          reactive(dash_df[league_name == input$league_of_player] %>% select(season) %>% unique() %>% unlist() %>% sort() %>% 
                     set_names(NULL))
        
        output$helper_reactive_seaons_ui <- 
          renderUI({
            selectInput(inputId = 'select_season_helper', 
                         label = "Select a Season", 
                         choices = helper_reactive_seaons(), 
                         selected = helper_reactive_seaons()[length(helper_reactive_seaons())])
        })
    
        helper_reactive_teams <- 
          reactive(dash_df[league_name == input$league_of_player] %>% select(team_name) %>% unique() %>% unlist() %>% sort() %>% 
                     set_names(NULL))
        
        output$helper_reactive_teams_ui <- 
          renderUI({
            selectInput(inputId = 'select_team_helper', 
                         label = "Select a Team (or all)", 
                         choices = c("All", helper_reactive_teams() ), 
                         selected = "All")
        })
        
    output$display_players <- 
      renderDataTable(
        display_players(
          FILTER_POS = input$position_filter_helper, 
          FILTER_TEAM = input$select_team_helper, 
          FILTER_LEAGUE = input$league_of_player, 
          FILTER_SEASON = input$select_season_helper
        )
      )

    ## sometimes we have two + players with the same name playing on different teams. If we type in such a name, we need to be able to 
    ##  select a team 
    
    same_name_teams <- 
      reactive(dash_df[summary_player == input$player_typed_name & 
                         season %in% input$select_season] %>% 
                 select(team_name) %>% unique() %>% unlist() %>% sort() %>% 
                     set_names(NULL))
    
      output$same_name_team_picker <- 
            renderUI({
              
              selectInput(inputId = 'select_team_same_name', 
                           label = "If more than one player has the same name, \npcik a team to precisely identify a player", 
                           choices = same_name_teams(), 
                           selected = same_name_teams()[length(same_name_teams())])
          })
    
    output$dynamic_table_summary <- 
      renderDataTable(
        dynamic_table_summary(
          DATA = dash_df, 
          TEAM = input$select_team_same_name, 
          PLAYER = input$player_typed_name, 
          SEASON = input$select_season, 
          COLUMNS = input$feature
        )
      )
    
    output$pizza_chart <- 
      renderPlot(
        pizza_chart(
          COLUMNS = input$feature, 
          DATA = dash_df,
          PLAYER = input$player_typed_name,
          TEAM = input$select_team_same_name, 
          SEASON = input$select_season,
          MINUTES_FILTER = input$minutes_to_limit,          
          COMP_LEAGUES = input$comp_leagues

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
    
    output$best_player_features <-
      renderDataTable(
        find_player_features(
          DATA = dash_df,
          PLAYER = input$player_typed_name,
          TEAM = input$select_team_same_name, 
          SEASON = input$select_season,
          N_FEATURES = input$top_values_number,
          MINUTES_FILTER = input$minutes_to_limit,
          RETURN_VECTOR = "N",
          BEST_OR_WORST = "BEST", 
          COMP_LEAGUES = input$comp_leagues
      ))
    
    output$worst_player_features <-
      renderDataTable(
        find_player_features(
          DATA = dash_df,
          PLAYER = input$player_typed_name,
          TEAM = input$select_team_same_name, 
          SEASON = input$select_season,
          N_FEATURES = input$top_values_number,
          MINUTES_FILTER = input$minutes_to_limit,
          RETURN_VECTOR = "N",
          BEST_OR_WORST = "WORST", 
          COMP_LEAGUES = input$comp_leagues
      ))
    
    output$all_features_quantiles_density <- 
      renderPlotly(
        all_features_quantiles_density(
          DATA = dash_df,
          PLAYER = input$player_typed_name,
          TEAM = input$select_team_same_name, 
          SEASON = input$select_season,
          MINUTES_FILTER = input$minutes_to_limit, 
          COMP_LEAGUES = input$comp_leagues
        )
      )
    
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
              fluidRow(
                column(3,
                       box(sidebarMenu(
                        
                         textInput(inputId = 'player_typed_name', 
                                   label = "Type in Player Name", 
                                   value = 'Kevin De Bruyne'),
                         
                         uiOutput('same_name_team_picker'), 
                         
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
                                                  'passing_total_cmp', 
                                                  'defensive_tackles_tkl'
                                                  ),
                                     label="Choose Variables for Table",
                                     choices=  
                                       setdiff( # remove some columns from options here 
                                         colnames(dash_df), 
                                         remove_colnames
                                       ),
                                     multiple=TRUE), 
                         
                         numericInput(inputId = 'top_values_number', 
                                      label = "Number of Top Features to List", 
                                      min = 0, 
                                      max = 100, 
                                      value = 25), 
                         
                         numericInput(inputId = 'minutes_to_limit', 
                                      label = "Limit Players by Limit", 
                                      min = 0, 
                                      max = max(dash_df$summary_min), 
                                      value = 1000), 
                         
                         numericInput(inputId = 'target_sim_players', 
                                      label = "Similar Players to Find", 
                                      min = 0, 
                                      max = 100, 
                                      value = 20),
                         
                         selectInput(inputId = 'comp_leagues', 
                                     label = "Compare From League", 
                                     choices = sort(unique(dash_df$league_name)), 
                                     selected = top_5_leagues, 
                                     multiple = T), 
                         
                         sliderInput(inputId = 'similar_player_age_filter', 
                                      label = "Oldest Player Age for Comparisons", 
                                      value = c(23,27), 
                                      min = min(dash_df$summary_age, na.rm = T), 
                                      max = max(dash_df$summary_age, na.rm = T))
               
                         
                         
                       ), width = 12 
                       )
                ),
                column(9,
                       fluidRow(
                                box(uiOutput('header_message')), 
                                box(tableOutput('game_and_min_summary'), width = 12), 
                                box(dataTableOutput('dynamic_table_summary'), width = 12), 
                                box(plotOutput('pizza_chart'), width = 12),
                                box(dataTableOutput('best_player_features'), width = 12), 
                                box(dataTableOutput('worst_player_features'), width = 12),
                                box(plotlyOutput('all_features_quantiles_density'), width = 12), 
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

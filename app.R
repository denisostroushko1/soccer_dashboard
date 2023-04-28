

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
        season %in% SEASON] %>% 
      
      group_by(league_name) %>% 
      summarise(
        summary_player = summary_player, 
        summary_age = summary_age, 
        games_played = games_played, 
        summary_min = summary_min, 
        positions = all_positions
        
      ) -> out
    
    league <- DATA[summary_player %in% PLAYER & 
     team_name %in% TEAM & 
        season %in% SEASON]$league_name %>% unlist()
    
    ages <- DATA[
     league_name %in% league & 
        season %in% SEASON]$summary_age 
    
    out$summary_age <- paste0(out$summary_age, 
                              paste0("(", 100 * round(
                                length(which(
                                  out$summary_age >= ages)) /
                                  length(ages), 2),"%)"))
    
    
    games <- DATA[
     league_name %in% league & 
        season %in% SEASON]$games_played 
    
    out$games_played <- paste0(out$games_played, 
                              paste0("(", 100 * round(
                                length(which(
                                  out$games_played >= games)) /
                                  length(games), 2),"%)"))
    
    
    minutes <- DATA[
     league_name %in% league & 
        season %in% SEASON]$summary_min 
    
    out$summary_min <- paste0(
                              prettyNum(out$summary_min, big.mark = ","), 
                              paste0("(", 100 * round(
                                length(which(
                                  out$summary_min >= minutes)) /
                                  length(minutes), 2),"%)"))
    
    out$league <- league
    out$team <- TEAM
    
    out <- out %>% select(summary_player, summary_age, 
                          team, league, games_played, summary_min,
                          positions)
    rownames(out) = NULL
    colnames(out) = c('Player Name', 'Age(League Pecentile)', 
                      "Team Name", "League Name",'Games(League Pecentile)', 'Minutes(League Pecentile)', 
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
      
      data_dict_f <- 
        data_dict %>% 
        select(
          Data.Frame.Name, 
          Pretty.Name.from.FBref, 
          stat_cat
        ) %>% 
        mutate(
          names = Data.Frame.Name, 
          descr = Pretty.Name.from.FBref
        )
      
      print_res <-
        
        REACTIVE_DATA %>% 
        
        left_join(
          data_dict_f, 
          by = "names"
        ) %>% 
        select(-names, -Data.Frame.Name, - Pretty.Name.from.FBref ) %>% 
        select(stat_cat, descr, everything() ) %>% 
        arrange(-percentiles_per_90) %>%
        
        head(N_FEATURES)

      rownames(print_res) <- NULL

      print_res$percentiles <- paste0(round(print_res$percentiles, 2) * 100, "%")
      print_res$percentiles_per_90  <- paste0(round(print_res$percentiles_per_90, 2) * 100, "%")
      
      colnames(print_res) <- c("Stat. Category", "Stat. Name", "Aggregate Per Season", "Percentile", "Scaled Per 90 Minutes", "Percentile")
      return(
        datatable(print_res,rownames=FALSE, 
                  options = list(iDisplayLength = N_FEATURES), 
                  caption = "Best Metrics for Selected Player by Percentile Per 90 Minutes") %>% 
          formatRound(columns = c(3,5), 
                      digits = 2)
        )
    }
    
    if(RETURN_VECTOR == "N" & BEST_OR_WORST == "WORST"){
        data_dict_f <- 
        data_dict %>% 
        select(
          Data.Frame.Name, 
          Pretty.Name.from.FBref, 
          stat_cat
        ) %>% 
        mutate(
          names = Data.Frame.Name, 
          descr = Pretty.Name.from.FBref
        )
      
      print_res <-
        
        REACTIVE_DATA %>% 
        
        left_join(
          data_dict_f, 
          by = "names"
        ) %>% 
        select(-names, -Data.Frame.Name, - Pretty.Name.from.FBref ) %>% 
        select(stat_cat, descr, everything() ) %>% 
        arrange(-percentiles_per_90) %>%
        
        head(N_FEATURES)

      rownames(print_res) <- NULL

      print_res$percentiles <- paste0(round(print_res$percentiles, 2) * 100, "%")
      print_res$percentiles_per_90  <- paste0(round(print_res$percentiles_per_90, 2) * 100, "%")
      
      colnames(print_res) <- c("Stat. Category", "Stat. Name", "Aggregate Per Season", "Percentile", "Scaled Per 90 Minutes", "Percentile")
      return(
        datatable(print_res,rownames=FALSE, 
                  options = list(iDisplayLength = N_FEATURES), 
                  caption = "Best Metrics for Selected Player by Percentile Per 90 Minutes") %>% 
          formatRound(columns = c(3,5), 
                      digits = 2)
        )
      
    }
    
  }


one_feature_histogram <- 
  function(
    DATA, 
    PLAYER, 
    TEAM, 
    SEASON,
    MINUTES_FILTER,
    COMP_LEAGUES, 
    PLOT_VAR
    ){
    
    data_dict %>% filter(Pretty.Name.from.FBref == PLOT_VAR) %>% 
      select(Data.Frame.Name) %>% unlist() %>% set_names(NULL) -> plot_var_df
    
    stats <- DATA[league_name %in% COMP_LEAGUES & season %in% SEASON & summary_min >= MINUTES_FILTER] %>% 
      select(
        all_of(c(plot_var_df, 'summary_player', 'season', 'team_name', 'league_name', "summary_min" ))
      )
    
    stats$per_90 <- stats[[plot_var_df]]/stats$summary_min * 90

    stats[summary_player == PLAYER & 
            team_name == TEAM & 
            season %in% SEASON] %>% select(per_90) %>% 
            unlist()%>% unlist() -> x_t
    
    plot_ly(
      data = stats,
      alpha = .75
    ) %>%
      add_histogram(
        x = ~per_90,
        color = ~league_name,
        text = '',
        hoverinfo = 'text',
        histnorm = "probability"
        ) %>%
      layout(legend = list(orientation = 'h'),
             barmode = "stack",
             yaxis = list(range = c(0,1)), 
             xaxis = list(title = ''), 
             title = paste0(PLOT_VAR," per 90 minutes"),
             shapes = list(list(type = "line", 
                                x0 = x_t, x1 = x_t, 
                                y0 = 0, y1 = 1, 
                                line = list(color = "red", width = 2)))) 
  
  }

all_features_quantiles_density <- 
  function(
    REACTIVE_DATA
  ){
    
    f <- REACTIVE_DATA

    data_dict_f <- 
        data_dict %>% 
        select(
          Data.Frame.Name, 
          Pretty.Name.from.FBref,
          stat_cat
        ) %>% 
        mutate(
          names = Data.Frame.Name, 
          descr = Pretty.Name.from.FBref
        )
    
    f <- f %>% 
      left_join(data_dict_f, 
                by = 'names')
    
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
      layout(xaxis = list(range = c(0, 1), zeroline = F, title = "Percentiles"), 
             yaxis = list(title = "Approximate Probability"))
    )

    
  }

dynamic_table_summary <- 
  function(
    REACTIVE_DATA,  
    COLUMNS){

    
    data_dict_f <- 
        data_dict %>% 
        select(
          Data.Frame.Name, 
          Pretty.Name.from.FBref,
          stat_cat
        ) %>% 
        mutate(
          names = Data.Frame.Name, 
          descr = Pretty.Name.from.FBref
        )
    
    f <- REACTIVE_DATA
    rownames(f) <- NULL
    
    f <- 
      f %>% 
      left_join(data_dict_f, 
                by = 'names') 
    
    
    f <- f[f$descr %in% COLUMNS,]
    
    f %>% 
      select(
            stat_cat, 
            descr, 
            player_stat, 
            percentiles,
            player_stat_per_90, 
            percentiles_per_90) -> f
      
#    colnames(f) <- c("Descr", "st", "p_st", "st_90", "p_st_90")
    
    f$percentiles <- paste0(round(f$percentiles, 2) * 100, "%")
    f$percentiles_per_90  <- paste0(round(f$percentiles_per_90, 2) * 100, "%")
    
    colnames(f) <- c("Stat. Category","Stat. Name", "Aggregate Per Season", "Percentile", "Scaled Per 90 Minutes", "Percentile")
    datatable(f, rownames=FALSE,
              options = list(autoWidth = TRUE,
                             columnDefs = list(list(width = '10px', targets = list(3,4,5))))) %>% 
      
      formatRound(columns = c(3,5), 
                      digits = 2)
    
  }

pizza_chart <- 
  function(
    REACTIVE_DATA, 
    COLUMNS
    ){
    
    f <- REACTIVE_DATA[REACTIVE_DATA$names %in% COLUMNS,]
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

like_pizza_but_bar_graph <- 
  function(
    REACTIVE_DATA, 
    COLUMNS
  ){
    

    data_dict_f <- 
        data_dict %>% 
        select(
          Data.Frame.Name, 
          Pretty.Name.from.FBref,
          stat_cat
        ) %>% 
        mutate(
          names = Data.Frame.Name, 
          descr = Pretty.Name.from.FBref
        )
    
    f <- REACTIVE_DATA
    rownames(f) <- NULL
    
    f <- 
      f %>% 
      left_join(data_dict_f, 
                by = 'names') 
    
    
    f <- f[f$descr %in% COLUMNS,]
    
    f$descr <- 
      factor(f$descr, 
             levels = f %>% arrange(stat_cat, percentiles_per_90) %>% select(descr) %>% unlist())
    
    with(f, 
         f %>% 
          plot_ly() %>% 
            add_trace(
              x = ~descr, 
              y = ~percentiles_per_90, 
              color = ~stat_cat, 
              
              type = 'bar', 
              
              text = paste(
                'Name: ', descr, 
                "<br>Stat per 90: ", round(player_stat_per_90, 2), 
                "<br>Percentile: ", round(percentiles_per_90,2)
              ), 
              
              hoverinfo = 'text'
            ) %>% 
           
           layout(xaxis = list(title = ""), 
                  yaxis = list(title = "Percentile"))
    )
  }

similar_players_euclid_dist_data <- 
  function(
    DATA, 
    REACTIVE_DATA, 
    PLAYER, 
    TEAM, 
    SEASON, 
    MINUTES_FILTER, 
    FEATURES_LIST, 
    COMP_LEAGUES
  ){
    
  
    df <- DATA[league_name %in% COMP_LEAGUES & season %in% SEASON & summary_min >= MINUTES_FILTER & 
                 !team_name %in% TEAM] %>% 
      
      select(all_of(
          c(FEATURES_LIST, "summary_player", "summary_min", 'team_name', 'summary_age', 'all_positions', "league_name")
        ))
    
    colnames(df) <- c(FEATURES_LIST, "summary_player", "summary_min", 'team_name', 'summary_age', 'all_positions', "league_name")
    
    minutes <- df$summary_min
    
    # df_per_90 <- df %>% select(all_of(
    #       c(FEATURES_LIST)))

    df_per_90 <- df %>%
      mutate(across(all_of(FEATURES_LIST), ~ . / minutes * 90))

    df_per_90 <- na.omit(df_per_90)
    # 
    # colnames(df_per_90) <- FEATURES_LIST

    point <- REACTIVE_DATA[REACTIVE_DATA$names %in% FEATURES_LIST, ] %>% arrange(names)

    point <- point[match(FEATURES_LIST, point$names), ]
    for_dist <- point$player_stat_per_90 %>% unlist()

    df_per_90 <- df_per_90 %>% select(-all_of(FEATURES_LIST))
    
    colnames(df_per_90) <- c("summary_player", "summary_min", 'team_name', 'summary_age', 'all_positions', "league_name", FEATURES_LIST)
    
    df_per_90_names <- df_per_90[,1:6]
    df_per_90_calc <- df_per_90[,-c(1:6)]
    df_per_90_calc <- na.omit(df_per_90_calc)

     df_per_902 <-
       df_per_90_calc %>%
        mutate(across(everything(), ~
                        (. - for_dist[match(cur_column(), colnames(df_per_90_calc))])^2
                      ))
    df_per_902 <- na.omit(df_per_902)

    df_per_902 <-
      df_per_902 %>%
      mutate(
        sum = rowSums(across(everything())),
        scaled_distance = sum/max(sum)
      ) %>%
      select(scaled_distance)

    f <-
      cbind(
        df_per_90_names, df_per_902
      ) %>% arrange(scaled_distance)
  
    return(f)

  }

similar_players_table <- 
  function(
    REACTIVE_DATA, 
    TARGET_SIMILAR_PLAYERS, 
    AGE_FILTER1,
    AGE_FILTER2, 
    POSITIONS
  ){
    f <- REACTIVE_DATA %>%
      select(
        summary_player,
        team_name,
        league_name,
        summary_age,
        summary_min,
        all_positions,
        scaled_distance
      ) %>%
      arrange(scaled_distance) %>%
      filter(summary_age >= AGE_FILTER1 & summary_age <= AGE_FILTER2 & 
               grepl(
                 paste(POSITIONS, collapse = "|"), 
                 all_positions)) %>%
      head(TARGET_SIMILAR_PLAYERS) -> out

    
    out$scaled_distance <- round(out$scaled_distance, 2)
    
    colnames(out) <- c("Player", "Team Name", "League Name", "Player Age", "Minutes Played", "Positions Featured", 
                       "Distance to Target Player")
    
     datatable(out, rownames=FALSE)
      
  }

similar_players_pca <- 
  function(
    DATA, 
    SEASON, 
    MINUTES_FILTER,
    COMP_LEAGUES,
    FEATURES_LIST,
    PLAYER,
    TEAM
    ) {
    
      df <- DATA[league_name %in% COMP_LEAGUES & season %in% SEASON & summary_min >= MINUTES_FILTER ] %>% 
      
      select(all_of(
          c("summary_player", "summary_min", 'team_name', 'summary_age', 'all_positions', "league_name",'season', FEATURES_LIST)
        ))
      
    colnames(df) <- c("summary_player", "summary_min", 'team_name', 'summary_age', 'all_positions', "league_name",'season', FEATURES_LIST)
    
    minutes <- df$summary_min
    
    df <- 
      rbind(
        df[summary_player == PLAYER & season %in% SEASON & team_name == TEAM ],  ## make sure player of interest is always on top 
        df[!(summary_player == PLAYER & season %in% SEASON & team_name == TEAM )]
      )
    
    df_names <- df[,1:7]  # need to make selection of columns more robust here 
    df_pca <-   df[,-c(1:7)]
    
    df_per_90 <- df_pca %>%
      mutate(across(all_of(FEATURES_LIST), ~ . / minutes * 90))

    
    df_per_90 <- na.omit(df_per_90)
      
      set.seed(1)
      pca_res <- prcomp(df_per_90, scale = T)
      
      return(pca_res)
  }

similar_pca_table <- 
  function(
    PCA_RES
  ){
    summary(PCA_RES)$importance %>% t() -> d
    
    d <- d[d[,3] < .85, ]
    
    d <- data.frame(d)
    
    d$pc <- as.character(rownames(d))
    
    d <- d[,-1]
    
    d[,1 ] <- paste0(round(d[,1], 4) * 100, "%")
    
    d[,2 ] <- paste0(round(d[,2], 4) * 100, "%")
    
    colnames(d) <- c("Proportion of Variance", "Cumulative", "PC")
    
    d <- d %>% select(PC, everything())
    
    d
  }

similar_pca_plot_df <- 
  function(
    PCA_RES,
    PLAYER, 
    TEAM, 
    DATA,
    COMP_LEAGUES, 
    SEASON, 
    MINUTES_FILTER
  ){
    p_df <- DATA[summary_player == PLAYER & season %in% SEASON & team_name == TEAM ] %>% 
      
      select(all_of(
          c("summary_player", "summary_min", 'team_name', 'summary_age', 'all_positions', "league_name")
        )) %>% unique()
      
    df <- DATA[league_name %in% COMP_LEAGUES & season %in% SEASON & summary_min >= MINUTES_FILTER & 
                 !(summary_player == PLAYER & season %in% SEASON & team_name == TEAM )] %>% 
      
      select(all_of(
          c("summary_player", "summary_min", 'team_name', 'summary_age', 'all_positions', "league_name")
        ))
    
    
    df <- rbind(p_df, df) # make sure player is on top here 
    
    summary(PCA_RES)$importance %>% t() -> d
    
    d <- d[d[,3] < .85, ]
    d <- data.frame(d)
    d$pc <- as.character(rownames(d))
    
    
    plot_df <- 
      cbind(df, 
            PCA_RES$x[,c(d$pc)])
    
    return(plot_df)
    
  }

similar_pca_plot <- 
  function(
    REACTIVE_PCA_DF, 
    PLAYER, 
    TEAM, 
    SIMILAR_PLAYERS, 
    X_AXIS_PC, 
    Y_AXIS_PC
  ){
    
    all_play <- REACTIVE_PCA_DF[!(REACTIVE_PCA_DF$summary_player == PLAYER & 
                                  REACTIVE_PCA_DF$team_name == TEAM) & 
                                  !REACTIVE_PCA_DF$summary_player %in% SIMILAR_PLAYERS
                                ]
    
    similar <- REACTIVE_PCA_DF[REACTIVE_PCA_DF$summary_player %in% SIMILAR_PLAYERS]
    
    selected <- REACTIVE_PCA_DF[(REACTIVE_PCA_DF$summary_player == PLAYER & 
                                  REACTIVE_PCA_DF$team_name == TEAM)]
    
    plot_ly() %>% 
      add_trace(
        data = all_play, 
        x = ~all_play[[X_AXIS_PC]], 
        y = ~all_play[[Y_AXIS_PC]], 
        
        color = ~league_name, 
        
        opacity = .5, 
        
        type = "scatter", 
        mode = "markers", 
        text = paste(
          'Name: ', all_play$summary_player, 
          '<br>League: ', all_play$league_name, 
          '<br>Team: ', all_play$team_name 
        ), 
        hoverinfo = 'text'
      )  %>% 
      
      add_trace(
        data = similar, 
        x = ~similar[[X_AXIS_PC]], 
        y = ~similar[[Y_AXIS_PC]], 
        
        name = "Similar Players", 
        
        type = "scatter", 
        mode = "markers", 
        marker = list(size = 10, color = "red"), 
        text = paste(
          'Name: ', similar$summary_player, 
          '<br>League: ', similar$league_name,
          '<br>Team: ', similar$team_name 
        ), 
        hoverinfo = 'text'
      ) %>% 
      
      add_trace(
        data = selected, 
        x = ~selected[[X_AXIS_PC]], 
        y = ~selected[[Y_AXIS_PC]], 
        
        name = PLAYER, 
        
        type = "scatter", 
        mode = "markers", 
        marker = list(size = 15, color = "purple"), 
        text = paste(
          'Name: ', selected$summary_player, 
          '<br>League: ', selected$league_name, 
          '<br>Team: ', selected$team_name 
        ), 
        hoverinfo = 'text'
      ) %>% 
      
      layout(xaxis = list(title = X_AXIS_PC, zeroline = F), 
             yaxis = list(title = Y_AXIS_PC, zeroline = F),
             legend = list(orientation = 'h'))
      
  }

similar_dist_histogram <- 
  function(
    REACTIVE_DATA
    ){
    
    plot_ly(
      data = REACTIVE_DATA,
      alpha = .75
    ) %>% 
      add_histogram(
        x = ~scaled_distance, 
        color = ~league_name, 
        text = '',
        hoverinfo = 'text'
        ) %>% 
      layout(legend = list(orientation = 'h'),
             barmode = "stack", 
             xaxis = list(title = ''))
    
  }

similar_dist_dens <- 
  function(
    REACTIVE_DATA
    ){
    
    dens_d <- 
      REACTIVE_DATA %>% 
      group_by(league_name) %>% 
      summarise(
        dens_x = density(scaled_distance)$x,
        dens_y = density(scaled_distance)$y/sum(x = density(scaled_distance)$y)
      )
    
     with(dens_d, 
        dens_d %>% 
          plot_ly(
            x = ~dens_x, 
            y = ~dens_y, 
            color = ~league_name, 
            type = "scatter", 
            mode = 'lines', 
            text = 
              paste(
                "League: ", league_name, 
                "<br>Approx. Distance: ", round(dens_x, 6),
                "<br>Approx. Probability: ", round(dens_y, 6)
                
              ), 
            hoverinfo = 'text'
            
          ) %>% 
          
      layout(xaxis = list(range = c(0, 1), zeroline = F, title = ""), 
             yaxis = list(title = "Approximate Probability"), 
                 legend = list(orientation = 'h')
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
    
    output$one_feature_histogram <- 
      renderPlotly(
        one_feature_histogram(
            DATA = dash_df, 
            PLAYER = input$player_typed_name,
            TEAM = input$select_team_same_name, 
            SEASON = input$select_season,
            MINUTES_FILTER = input$minutes_to_limit,
            COMP_LEAGUES = input$comp_leagues, 
            PLOT_VAR = input$hist_feature
        )
    )
    
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
    
    output$like_pizza_but_bar_graph <- 
      renderPlotly(
        like_pizza_but_bar_graph(
          COLUMNS = input$feature, 
          REACTIVE_DATA = percentiles_data()  
        )
      )
    #### similar player scouting poge 
    
    # a dynamic filter that by default selects players in positions that have similar to a player we are scouting 
    output$scouter_positions_filter <- 
      renderUI(
        selectInput(inputId = 'scouter_positions', 
                     label = 'Position', 
                     choices = positions_short_names, 
                     selected = 
                      strsplit(
                            gsub("[0-9()]", "", 
                                 dash_df[summary_player == input$player_typed_name & 
                                           season %in% input$select_season & 
                                           team_name %in% input$select_team_same_name ]$all_positions %>% unlist()), 
                            ", ")[[1]], 
                    multiple = T
                      )
      )
    
    best_player_features_vec <-
      reactive(
        find_player_features(
          REACTIVE_DATA = percentiles_data(),
          N_FEATURES = input$top_values_number,
          RETURN_VECTOR = "Y",
          BEST_OR_WORST = ""
      ))
    
    similar_players_euclid_dist_data_reactive <- 
      reactive(
        similar_players_euclid_dist_data(
          
          DATA = dash_df, 
          REACTIVE_DATA = percentiles_data(), 
          PLAYER = input$player_typed_name,
          TEAM = input$select_team_same_name, 
          SEASON = input$select_season, 
          MINUTES_FILTER = input$minutes_to_limit,
          FEATURES_LIST = best_player_features_vec(), 
          COMP_LEAGUES = input$comp_leagues
        )
      )
    
    output$similar_players_table <- 
      renderDataTable(
        similar_players_table(
          REACTIVE_DATA = similar_players_euclid_dist_data_reactive(), 
          TARGET_SIMILAR_PLAYERS = input$target_sim_players, 
          AGE_FILTER1 = input$similar_player_age_filter[1],
          AGE_FILTER2 = input$similar_player_age_filter[2],
          POSITIONS = input$scouter_positions
          )
        )
    
    similar_players_pca_reactive <- 
      reactive(
        similar_players_pca(
          DATA = dash_df, 
          SEASON = input$select_season, 
          MINUTES_FILTER = input$minutes_to_limit,
          COMP_LEAGUES = input$comp_leagues, 
          FEATURES_LIST = best_player_features_vec(),
          PLAYER = input$player_typed_name,
          TEAM = input$select_team_same_name
        )
      )
    
    similar_players_vector <- 
      reactive(
       similar_players_euclid_dist_data_reactive() %>% 
          arrange(scaled_distance) %>% 
          filter(summary_age >= input$similar_player_age_filter[1] & 
                   summary_age <= input$similar_player_age_filter[2] & 
                   grepl(
                     paste(input$scouter_positions, collapse = "|"), 
                     all_positions) ) %>% 
          head(input$target_sim_players) %>% 
          select(summary_player)  %>% unlist()
      )
    
    output$similar_pca_table <- 
      renderTable(
        similar_pca_table(
          PCA_RES = similar_players_pca_reactive()
        )
      )
    
    similar_pca_plot_data <- 
      reactive(
        similar_pca_plot_df(
          PCA_RES = similar_players_pca_reactive(), 
          PLAYER = input$player_typed_name,
          TEAM = input$select_team_same_name, 
          DATA = dash_df,
          COMP_LEAGUES = input$comp_leagues, 
          SEASON = input$select_season, 
          MINUTES_FILTER = input$minutes_to_limit
        )
      )
    
    output$pc_pick_1 <- 
            renderUI({
              
              selectInput(inputId = 'select_pc_plot_1', 
                           label = "Pick a PC for X-axis", 
                           choices = colnames(similar_pca_plot_data())[grep("PC", colnames(similar_pca_plot_data()))], 
                           selected = colnames(similar_pca_plot_data())[grep("PC", colnames(similar_pca_plot_data()))][1])
          })
    
    output$pc_pick_2 <- 
            renderUI({
              
              selectInput(inputId = 'select_pc_plot_2', 
                           label = "Pick a PC for Y-axis", 
                           choices = colnames(similar_pca_plot_data())[grep("PC", colnames(similar_pca_plot_data()))], 
                           selected = colnames(similar_pca_plot_data())[grep("PC", colnames(similar_pca_plot_data()))][2])
          })
    
    output$test_vector_sh <- renderTable(similar_players_vector())
    
    output$similar_pca_plot <- 
      renderPlotly(
        similar_pca_plot(
            REACTIVE_PCA_DF = similar_pca_plot_data(), 
            PLAYER = input$player_typed_name,
            TEAM = input$select_team_same_name, 
            SIMILAR_PLAYERS = similar_players_vector(), 
            X_AXIS_PC = input$select_pc_plot_1, 
            Y_AXIS_PC = input$select_pc_plot_2)
          )
    
    output$similar_dist_histogram <- 
      renderPlotly(
        similar_dist_histogram(
          REACTIVE_DATA = similar_players_euclid_dist_data_reactive()
        )
      )
    
    output$similar_dist_dens<- 
      renderPlotly(
        similar_dist_dens(
          REACTIVE_DATA = similar_players_euclid_dist_data_reactive()
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
      ,menuItem("Similar Player Scouting", tabName = "player_scouting")
      ,menuItem("Two Player Comparison", tabName = "two_player_comparison")
      ,menuItem("Team Profile", tabName = 'team_profile')
        
      # apparently R is looking for all images to be in the www folder. So I created one and stored images there. 
      # there is no need to refer to the folder explicitly
    #, tags$img(src = "ball.jpeg", width = "100%", height = "100%")
      , tags$img(src = "boot.png", width = "100%", height = "100%")
      
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
                
                <li> All these stats can be used to accurately summarize players' profiles, and fild similar players 
                in a pool of over 30,000 players. </li> 
                </ul>
                
                <p style='font-size: 24px; font-weight: bold;'>Navigation: </p>
                
                <ul> 
                
                <li> <b> Helper Page </b>. This tab provides assistance to the user. Here you can find a data dictionary 
                (which I also creted used FBref's glossary), and a player look up tab. </li> 
                
                <li> Player look up can, obviously, show you players, but also will shows what years, or seasons, are 
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
                
                <li> <b>Player scouting </b>attmepts to find similar players to a player selected in the previous tab </li> 
                
                  <ul> 
                  
                    <li> Similar players are found using best stats from player profile tab </li> 
                
                    <li> Using sorted distance to the player, we limit all players to be within certain age rage and  
                        to be featured in certain positions </li> 
                
                    <li> You get to see how many simialr players to display, by default, you will see 10 </li> 
                
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
              )
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
              fluidPage(
                 
                        HTML("
                             <p style='font-size: 24px; font-weight: bold;'>Start exploring the data here! </p>
                             <br> <p style='font-size: 16px; font-weight: bold;'>Couple notes to keep in mind before you dive in:  </p>
                             
                             
                              
                             
                             <ul>
                             
                             <li> Please type in the player of the name without any additional spaces! 
                                  Otherwise, you will see a sea of red errors everywhere! </li>
                             
                             <li> A lot of players have special characters in their names. I recommend 
                                that you find their name spelling in the Helper Page - Player Search tab 
                                and copy paste the results! </li>
                             
                             <li> This page has some <em>reactive</em> and <em>nested reactive</em>  
                             elements, so please allow a few seconds for all of them to load. 
                             Tables and other output might flicker a few times before they are 
                             ready for exploration 
                             </li>
                             
                             <li> I tried to include as much helpful notes as posisble, 
                                  please let me know if you have any other questions! 
                             </li> 
                             
                             <li> Some notable players in the game of soccer are
                             <b>Lionel Messi</b>, <b> Erling Haaland	</b>, and 
                                <b> Vinicius Júnior	 </b>. Type in their names to 
                             see some absolutely incredible stats! 
                             
                             
                             </ul>"), 
                        
                        box(width = 12, 
                            background = 'aqua', 
                            HTML(
                              "<b> First </b> you get to pick three inputs: Player name, 
                                Player team (if needed), and season(s) to explore. 
                              <br> 
                              <ul> 
                              
                              <li>Some competitions are played over the summer, so they 
                              take place in one calendar year. Most are played 
                              over the winter, so they include two years, such as 2022/2023. </li>
                              
                              <li>When you need to see what season to pick for each league, consult Helper Page - Player Lookup. 
                              When selecting a competition, the app will display what seasons are available for the competition.
                              </li>
                              
                              <li> 
                              Sometimes players have similar names, then the App will 
                              display all such players with similar names in the 
                              initial summary table. You will need to tweak the 
                              team filter and select a team you are interested. 
                              </li>
                              
                              <li> For example, 'Rodrigo' are two different players 
                              who play for 'Real Betis' and 'Leeds United'. 
                              They are two different players
                              </ul>
                              
                              <br> 
                              <p style='font-size: 16px; font-weight: bold;'>The first table you see 
                              provides some essential information about the player. 
                              </p>
                              <br>
                              Percentiles are restricted to player's respective league here, and show many 
                              many players are below a seleted player in terms of some metric we display. 
                              
                              For example, age '30 (75%)' would identify that a player is 30 years old, and 
                                is odler than 75% of league's players. 
                              "
                              )), 
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
                       box(width = 12, background = 'aqua', 
                           HTML(
                             "
                             <b> Onto More Exciting Data! </b> 
                             
                             <br> 
                             Now you get to pick how we calculate different statistics for a selected player. 
                             There are two many statistics we present for the player, based on what they did over the course of the season. 
                            <ul> 
                            
                            <li> Aggregate per season is the total value </li>
                            
                            <li> We turn aggregate statistics to 'per 90 minutes' statistic to put every player on 
                            the level ground for comparisons</li>
                            
                            </ul> 
                            
                            <br> 
                            We scale statistics to 'per 90 minutes' so that we can accurately assess how well a player 
                            does a certain action in a constained amount of time. 90 minutes is a length of a soccer match. 
                            
                            <br> All percentiles are derived using a subset of all available players. 
                            By defaul, you compare 'performance per 90 minutes' for players who played at least 1,000 minutes 
                            in the top 5 european leagues. 
                            
                            <br> Percentile gives you just one summary statistic, so feel free to select a variable of interest 
                            to display a shpae of the distribtuion. A vertical red line represents a percentile of a selected player.  
                             "
                           )), 
                       box(
                         numericInput(inputId = 'top_values_number', 
                                      label = "Number of Top Features to List:", 
                                      min = 0, 
                                      max = 100, 
                                      value = 10), width = 4), 
                         
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
                        ), width = 6), 
                      box(
                         selectInput(inputId="hist_feature", 
                                     selected = 'Expected Goals',
                                     label="Choose a Variable for Histogram",
                                     choices=  
                                       setdiff( # remove some columns from options here 
                                         data_dict %>% select(Pretty.Name.from.FBref) %>% unlist(), 
                                         remove_colnames_dict
                                         )
                         ),
                         
                         plotlyOutput('one_feature_histogram')
                         
                        , width = 6), 
                      
                      box(width = 12, background = 'aqua', 
                          HTML("
                               
                               So far you saw a summary of best and worst stats for a selected player. Density curves 
                               summarize the distrbituion of all percentiles for each statistic, colored by a major category 
                               of stats. Having a peak to the right means that a player excels in a given category. 
                               
                               ")
                          ), 
                      
                      box(
                        plotlyOutput('all_features_quantiles_density')
                        ,width = 12
                      ), 
                      
                      box(width = 12, background = 'aqua', 
                          HTML("
                               And if you want to hand pick categories for each player you can do so in this section! 
                               A filter below gives you in option to display a statistic in a tabke and on a bar graph. 
                               
                               ")
                          ), 
                      
                      box(
                        
                         selectInput(inputId="feature", 
                                     selected = c(
                                                  'Goals'
                                                  ,'Expected Goals'
                                                  ,'Passes Completed'
                                                  ,'Total Passing Distance'
                                                  ,'Number of players tackled'	
                                                  ,'Dribbles Challenged'
                                                  ,'Progressive Carries'	
                                                  ,'Miscontrols'
                                                  ),
                                     label="Choose Variables for Table",
                                     choices=  
                                       setdiff( # remove some columns from options here 
                                         data_dict %>% select(Pretty.Name.from.FBref) %>% unlist(), 
                                         remove_colnames_dict
                                       ),
                                     multiple=TRUE), width = 12
                      ), 
                      box(dataTableOutput('dynamic_table_summary'), width = 6, 
                          align = "left"),
                   
                      box(plotlyOutput('like_pizza_but_bar_graph'), width = 6)            
                  #    box(plotOutput('pizza_chart'), width = 6)
                       
                    
                        
              )), 
      
      
      tabItem(tabName = "player_scouting", 
              fluidRow(
                box(
                  HTML("<p style='font-size: 16px; font-weight: bold;'>
                          Now that you saw what stats a player excells at, are you curious what other players do the same things 
                    just as good? Maybe you are a scount that wants to find a replacement for an aging star? 
                          </p>
                    
                    Uisng a set of best statistics, on the per 90 minutes scale, we calculate euclidian distance from each 
                    player to a player of interest. This distance is then scaled to 0-1 scale, for convinience.
                    
                    On this page you will see a number of similar players. By default, we include a vast range of ages, 
                    but you can limit it and focus on scouting young outstanding players. 
                    
                    You can also pick positions that we scout, by default all positions for a player of interest are selected. 
                    "), width = 12, background = 'aqua'), 
                
                
                box(sliderInput(inputId = 'similar_player_age_filter', 
                                  label = "Age Range for Comparisons", 
                                  value = c(20,35), 
                                  min = min(dash_df$summary_age, na.rm = T), 
                                  max = max(dash_df$summary_age, na.rm = T)), 
                    width = 4), 
                    
                 box(numericInput(inputId = 'target_sim_players', 
                              label = "Similar Players to Display", 
                              min = 0, 
                              max = 100, 
                              value = 10), 
                     width = 4), 
        
                box(uiOutput('scouter_positions_filter'), width = 4),
                
                box(dataTableOutput('similar_players_table'), 
                    width = 12), 
              
                box(width = 12, 
                    background = 'aqua', 
                    HTML("
                         
                         <p style='font-size: 16px; font-weight: bold;'>
                          Tables are fun and all, but how do these 10-15 players look compared to everyone in the comparison 
                         subset!? 
                          </p>
                         
                         Unfortunately, we can't make a plot with 15 axes, so we need to reduce the dimensions. Uisng PCA, we 
                         can achive this goal. A small summary table shows how much variation if contained within the first X
                         principal components. A rule of thumb is to have around 80% of original variation. 
                         <br> 
                         We can then use the first two principal components to make a scatter plot that, sort of, shows the 
                         spead of these high dimensional data, and where similar players are located when compared to the target
                         player, as well as all other players. 
                         
                         <br> What about the distibution of distances? I don't know honestly, after I made a plot I could not find a use for it. 
                         
                         ")), 
                
                box(
                  tableOutput('similar_pca_table'), 
                  uiOutput('pc_pick_1'), 
                  uiOutput('pc_pick_2'), 
                  width = 3
                ), 
                box(plotlyOutput('similar_pca_plot'), width = 5), 
                box(plotlyOutput('similar_dist_dens'), width = 4), 
                
                box(width = 12, background = 'light-blue', 
                    HTML(
                      "
                      <p style='font-size: 24px; font-weight: bold;'>
                          Now you can start all over again, by picking a name of a similar players and looking at their 
                      stats at the previous page. Have fun! 
                          </p>
                      "
                    ))
                
              
              )
              ),
      
      tabItem(tabName = "two_player_comparison", 
              fluidRow("Two Player Comparison: COMING SOOOOOON")
              ), 
      
      tabItem(tabName = "team_profile", 
              fluidRow("Team Profile: COMING SOOOOOON, but later than player comparison")
              )
  ))


shinyApp(
  ui = 
    dashboardPage(
      skin = "green", 
      dashboardHeader(
        title =  "Soccer Data", 
        titleWidth = 300
        ),
      sidebar,
      body
      ),
  server = server_side
)

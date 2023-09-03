                                        
### data dictionary stuff

display_players <- 
  function(
    DATA, 
    FILTER_POS, 
    FILTER_TEAM, 
    FILTER_SEASON, 
    FILTER_LEAGUE
  ){
    display <- 
         
        if(FILTER_POS %in% "All" & 
           FILTER_TEAM %in% "All"){
          
              DATA[league_name %in% FILTER_LEAGUE &
                       season %in% FILTER_SEASON]
          
          }else if(
            FILTER_POS != "All" & 
            FILTER_TEAM %in% "All"){
            
              DATA[league_name %in% FILTER_LEAGUE &
                         season %in% FILTER_SEASON & 
                    grepl(FILTER_POS, all_positions)]
            
          }else if(
            FILTER_POS %in% "All" & 
            FILTER_TEAM != "All"){
            
            DATA[league_name %in% FILTER_LEAGUE &
                    season %in% FILTER_SEASON & 
                    team_name %in% FILTER_TEAM]
          }else if(
            FILTER_POS != "All" & 
            FILTER_TEAM != "All"){
            
            DATA[league_name %in% FILTER_LEAGUE &
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

### player profile - summary page 

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
            select(summary_player, league_name, team_name, season, summary_age, all_positions) , 
          target_df
        )
      
      # get data for the pool of players we compare a target player with 
      RAW_DATA %>% 
        filter(
          season %in% COMP_SEASONS &
          league_name %in% COMP_LEAGUES &
          grepl(
               paste(COMP_POSITIONS, collapse = "|"),
               dominant_position) &
          summary_age >= COMP_AGE_START  &
          summary_age <= COMP_AGE_END &
          
          summary_min >= COMP_MINUTES_START &
          summary_min <= COMP_MINUTES_END &
          
            # remove all data for a target player
          summary_player != TARGET_PLAYER 
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
                     dominant_position) &
                summary_age >= COMP_AGE_START  &
                summary_age <= COMP_AGE_END &
                
                summary_min >= COMP_MINUTES_START &
                summary_min <= COMP_MINUTES_END &
                      
                  # remove all data for a target player
                summary_player != TARGET_PLAYER 
              ) %>% 
              select(summary_player, league_name, team_name, season, summary_age, all_positions) , 
            
            comp_df
          )
        
        increment_value <- function(x) {
          value <- as.integer(gsub("\\D", "", x))
          incremented_value <- value + 3  # Increase the value by 3
          gsub("\\d+", incremented_value, x)
        }
        
        target_df <- 
          rbind(target_df, 
                comp_df) %>% 
          mutate(
            comb_positions =  gsub("(\\w)\\((\\d+)\\)", "\\1 ( \\2 )", all_positions)
          ) %>% 
          select(-all_positions)
        
    }else{
      
      players <- RAW_DATA %>% filter(league_name %in% COMP_LEAGUES) %>% select(summary_player) %>% unique() %>% unlist()
      
      player_leagues <- 
        RAW_DATA %>% 
          filter((season %in% COMP_SEASONS & league_name %in% COMP_LEAGUES) | 
                   (season == TARGET_PLAYER_SEASON & summary_player == TARGET_PLAYER) 
                 ) %>% 
          select(summary_player, season, team_name, league_name) %>% unique() 
      
      home_leagues <- 
        RAW_DATA %>% 
          filter(((season %in% COMP_SEASONS & league_name %in% COMP_LEAGUES)  | 
                    (season == TARGET_PLAYER_SEASON & summary_player == TARGET_PLAYER)) & 
                   !(league_name %in% c(
                     "UEFA Champions League", 
                     "Copa Libertadores de América", 
                     "UEFA Europa Conference League", 
                     "UEFA Europa League"
                   )
                     )) %>% 
          select(summary_player, season, team_name, league_name) %>% rename(home_league = league_name) %>% unique() 
      
      player_leagues <- 
        player_leagues %>% 
        left_join(
          home_leagues, 
          by = c('summary_player' , 'season'   , 'team_name')
        )
      
      player_leagues <- player_leagues %>% 
        mutate(
          league_name = case_when(
            !is.na(home_league) &  league_name %in% c(
                     "UEFA Champions League", 
                     "Copa Libertadores de América", 
                     "UEFA Europa Conference League", 
                     "UEFA Europa League"
                   ) ~ home_league, 
            T ~ league_name
          )
        ) %>% select(-home_league) %>% unique()
      
      # get a row of data for the player we want to summarize and compare with others 
      

      
      RAW_DATA %>% 
        filter(
          summary_player %in% TARGET_PLAYER, 
          season %in% TARGET_PLAYER_SEASON, 
          team_name %in% TARGET_PLAYER_TEAM
        )  %>% 
        
        select(-all_of( 
          setdiff(remove_colnames_keep_games_mins, "summary_age")
          )) %>%
        
        colSums() %>% 
        t() %>% 
        data.frame() -> target_df
      
      target_df <- cbind(target_df, 
          RAW_DATA %>% 
            filter(
              summary_player %in% TARGET_PLAYER & 
              season %in% TARGET_PLAYER_SEASON & 
              team_name %in% TARGET_PLAYER_TEAM
            )  %>% select(summary_player, season, team_name, all_positions) %>% 
            group_by(summary_player, team_name, season) %>% 
            mutate(
              comb_positions = combine_strings(all_positions)
            )  %>% select(comb_positions) %>% unique()
            )
      
      target_df <- 
        target_df %>% 
        
        left_join(
          player_leagues,
          by = c("summary_player", "team_name", "season")
        ) %>% select(summary_player,	season,	team_name, league_name, summary_age, comb_positions, everything())
      
      # other players data in the comparison pool 
      
      RAW_DATA %>% 
        filter(
          season %in% COMP_SEASONS &
          league_name %in% COMP_LEAGUES & 
          summary_player %in% players &
          grepl(
               paste(COMP_POSITIONS, collapse = "|"),
               dominant_position) &
          summary_age >= COMP_AGE_START  &
          summary_age <= COMP_AGE_END &
          
          summary_min >= COMP_MINUTES_START &
          summary_min <= COMP_MINUTES_END &
                      
                  # remove all data for a target player
                summary_player != TARGET_PLAYER 
        ) %>% 
        select(-all_of(
           c("league_name","all_positions","dominant_position")
        )) %>%
        group_by(
          summary_player, season, team_name
        ) %>% 
          # remove non-numeric columns form aggregation 
        summarise(across(everything(), sum, na.rm =T)) %>% 
        data.frame() %>% 
        
        left_join(
          
          RAW_DATA %>% 
            filter(
              season %in% COMP_SEASONS &
              summary_player %in% players &
              grepl(
                   paste(COMP_POSITIONS, collapse = "|"),
                   dominant_position) &
              summary_age >= COMP_AGE_START  &
              summary_age <= COMP_AGE_END &
              
              summary_min >= COMP_MINUTES_START &
              summary_min <= COMP_MINUTES_END &
                          
                      # remove all data for a target player
                    summary_player != TARGET_PLAYER 
            ) %>%
            
            group_by(summary_player, team_name, season) %>% 
            mutate(
              comb_positions = combine_strings(all_positions)
            )  %>%  select(summary_player, team_name, season, comb_positions) %>% unique(), 
          
          by = c('summary_player', 'team_name', 'season')
          
        ) %>% 
        
        left_join(
          player_leagues,
          by = c("summary_player", "team_name", "season")
        ) %>% 
        select(summary_player,	season,	team_name, league_name, comb_positions, everything())-> comp_df
        
      target_df <- rbind(target_df, comp_df) 
      
    }
    
    return(target_df)
    
  }
  
brief_summary_and_pool <- 
  function(
    REACTIVE_DATA, 
    TARGET_PLAYER, 
    TARGET_PLAYER_TEAM
  ){
    
    REACTIVE_DATA %>% filter(summary_player %in% TARGET_PLAYER & team_name %in% TARGET_PLAYER_TEAM) %>% 
      select(league_name, summary_min, games_played) -> df
    
    minutes <- sum(df$summary_min) %>% unlist()
    min_q <- length(which(REACTIVE_DATA$summary_min <= minutes))/nrow(REACTIVE_DATA)
    
    games <- sum(df$games_played) %>% unlist()
    games_q <- length(which(REACTIVE_DATA$games_played <= games))/nrow(REACTIVE_DATA)
    
    players <-  REACTIVE_DATA %>% filter(summary_player != TARGET_PLAYER & team_name != TARGET_PLAYER_TEAM) %>% nrow()
    
    outT <- 
      data.frame(
        m = paste0(
          prettyNum(minutes, big.mark = ","), " (", round(min_q, 4) * 100, "%)"), 
        
        g = paste0(
          prettyNum(games, big.mark = ","), " (", round(games_q, 4) * 100, "%)"), 
        
         c = prettyNum(players, big.mark = ",")
        )
    
    colnames(outT) <- c("Minutes Played", "Games Played", "Comparison Pool")
      
    return(outT)
  }

create_field_plot <- 
  function(
    REACTIVE_DATA, 
    TARGET_PLAYER, 
    TARGET_PLAYER_SEASON,
    TARGET_PLAYER_TEAM
  ){
    
    REACTIVE_DATA %>% 
      filter(summary_player == TARGET_PLAYER & team_name == TARGET_PLAYER_TEAM) %>% 
      select(comb_positions) %>% unlist() -> player_string
    
    player_data <- str_match_all(player_string, "([A-Z]+) \\( ([0-9]+) \\)")[[1]][, 2:3]

    if(is.null(dim(player_data))){
      player_df <- data.frame(var1 = player_data[1], var2 = as.integer(player_data[2]))
    } else{
      player_df <- data.frame(var1 = player_data[, 1], var2 = as.integer(player_data[, 2]))
    }
    # Create a data frame
    
    # 
    I = ""
    
    if("WB" %in% player_df$var1){
      player_df %>% filter(var1!= "WB") %>% mutate(RBC = substr(var1, 1, 1)) %>% filter(RBC %in% c("R", "L") ) %>%
        group_by(RBC) %>% summarize(s = sum(var2)) %>% arrange(-s) %>% head(1) %>% select(RBC) %>% unlist() -> I
    }

    ### create data frame with all positions and their coordinates on the map 
    
    position_coords =
      data.frame(
        var1 = c(
          "GK","AM","CB","CM","DF","DM","FW","LB","LM","LW","MF","RB","RM","RW","WB"
        ),

        coord1 = c(10,70,25,55,25,35,85,25,60,85,55,25, 60,90, 45),
    #    coord2 = c(50,50,35,40,65,50,55,85,80,90,60,15, 20, 15,90)
        coord2 = c(50, 50, 65, 60, 35, 50, 45, 15, 20, 10, 40, 85, 85, 85, 10)
      ) %>%

      inner_join(
        player_df,
        by = "var1"
      ) %>%
      mutate(
        coord2 = case_when(
          I == "R" & var1 == "WB" ~ 10,
          T ~ coord2
        ),
        label = paste0(var1, " (", var2, ")")
          )
    
  ggplot(data = position_coords,
       aes(x = coord1, y = coord2, label = label)) +

      annotate_pitch(colour = "white", alpha = 1, fill = "#5da15c", linewidth = 2) + 
      theme_pitch(aspect_ratio = NULL) +
      geom_point(color = "#5da15c") +
      geom_text(size=8) +
      theme(title = element_text(size = 15),
            text = element_text(size = 30), 
            plot.title = element_text(hjust = 0.25, face = "bold")
            ) +
      ggtitle(
        paste0(
          TARGET_PLAYER_SEASON, " Featured Positions"
        )
      ) + 
    coord_flip()
  }

percentile_data_frame_one_player <- 
  function(
    DATA, 
    PLAYER, 
    TEAM
  ){
  # selected player data 
    player_df <-  DATA[DATA$summary_player == PLAYER & DATA$team_name == TEAM, ] %>% select(-summary_age)
    player_min <- sum(player_df$summary_min) 
    
    col2 = colnames(player_df)
    
    player_df <- 
      player_df %>% 
      dplyr::select(
        -all_of(
          c(
            "comb_positions",
            setdiff(remove_colnames, c("dominant_position", "all_positions", "summary_age")))
        )
      ) %>% 
      summarise(across(everything(), sum))
    
    player_df_per_90 <- player_df
    player_df_per_90 <- player_df_per_90 %>% mutate_all(~. / player_min * 90)
    
    # other players data, which we compare our selected player to 
    league_stat <- DATA[!(DATA$summary_player == PLAYER & DATA$team_name == TEAM),]
 
    league_stat <- 
      league_stat %>% 
        group_by(summary_player, team_name) %>% 
      
        dplyr::select(
        -all_of(
          c(
            "comb_positions",
            setdiff(remove_colnames, c("dominant_position", "all_positions", "summary_age", "summary_min")))
        )
      ) %>%  ## keep summary minutes in the data for now
      summarise(across(everything(), sum))
    
    league_stat <- na.omit(league_stat) # first remove observations and then create minutes vector 
    minutes <- league_stat$summary_min
    
#    colnames(league_stat)[which(colnames(league_stat) %in% remove_colnames)]
    
    league_stat <- 
      league_stat %>% 
      ungroup() %>% 
      dplyr::select(-all_of(colnames(league_stat)[which(colnames(league_stat) %in% remove_colnames)]))
    
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

all_features_ranked <- 
  function(
    REACTIVE_DATA){

    
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
                by = 'names')  %>% 
      arrange(-percentiles_per_90)
  
    
    f %>% 
      select(
            Data.Frame.Name, 
            stat_cat, 
            descr, 
            player_stat, 
            percentiles,
            player_stat_per_90, 
            percentiles_per_90) -> f
      
#    colnames(f) <- c("Descr", "st", "p_st", "st_90", "p_st_90")
    
    colnames(f) <- c(
      "names", "Stat. Category","Stat. Name", "Aggregate Per Season", "Percentile", "Scaled Per 90 Minutes", "Percentile per 90")
    
    return(f)
  }


player_best_features_vector <- 
  function(
    REACTIVE_DATA, 
    N_FEATURES){
    
    REACTIVE_DATA %>% 
      arrange(-`Percentile per 90`) %>% 
      head(N_FEATURES) %>% 
      select(names) %>% unlist()
    
  }

average_quantiles_data <- 
  function(
    REACTIVE_DATA
  ){
    return(
      REACTIVE_DATA %>% 
        group_by(
          `Stat. Category`
        ) %>% 
        summarize(
          mean = mean(`Percentile per 90`), 
          sd = sd(`Percentile per 90`), 
          n = n()
        ) 
    )
  }

create_gauge <- 
  function(
    REACTIVE_DATA, 
    CATEGORY){
    
    REACTIVE_DATA %>% 
      filter(`Stat. Category` == CATEGORY) %>% 
      select(mean) %>% round(.,4) %>% 
      mutate(mean = mean*100) %>% unlist() %>% setNames(NULL)-> gauge_value
    
    REACTIVE_DATA %>% 
      filter(`Stat. Category` == CATEGORY) %>% 
      select(sd) %>% round(.,4) %>% 
      mutate(sd = sd*100) %>% unlist() %>% setNames(NULL)-> spread
    
    
    gradient_colors <- colorRampPalette(c("red", "yellow", "orange", "green"))(100)
    color_index <- round(gauge_value)
    color <- gradient_colors[color_index]
    
    m <- paste0(
     CATEGORY, "\nSpread: ",spread
    )
    
    gauge(
      value = gauge_value, 
      min = 0, 
      max = 100, 
      symbol = "%", 
      label = m,
      gaugeSectors(
        success = c(70, 100),
        warning = c(40, 70),
        danger = c(0, 40),
        color = color
      )
    )
    
  }

create_gauge_count <- 
  function(
    REACTIVE_DATA, 
    CATEGORY, 
    SLIDER_INPUT
  ){
    
    REACTIVE_DATA %>% 
      filter(`Stat. Category` == CATEGORY) %>%
      mutate(
        count = length(which(`Percentile per 90` > (SLIDER_INPUT/100)))
      ) %>% select(count) %>% unlist() %>% unique() -> gauge_value
    
    max_gauge = REACTIVE_DATA %>% 
      filter(`Stat. Category` == CATEGORY) %>% nrow()
      
    gradient_colors <- colorRampPalette(c("red", "yellow", "orange", "green"))(max_gauge)
    color_index <- round(gauge_value)
    color <- gradient_colors[color_index]
    
    m = paste0(CATEGORY, " Varaibles")
    
    gauge(
      value = gauge_value, 
      min = 0, 
      max = max_gauge, 
      symbol = "", 
      label = m,
      gaugeSectors(
        success = c(70, 100),
        warning = c(40, 70),
        danger = c(0, 40),
        color = color
      )
    )
  }

radar_quantiles_chart <- 
  function(
    REACTIVE_DATA
  ){
    with(REACTIVE_DATA, 
           
      REACTIVE_DATA %>% 
        
        mutate(
          short_descr = 
            case_when(
              `Stat. Category` == "Attacking" ~ "A", 
              `Stat. Category` == "Passing" ~ "P",
              `Stat. Category` == "Pass Type Detail" ~ "P.T.",
              `Stat. Category` == "On The Ball" ~ "OB",
              `Stat. Category` == "Misc" ~ "M",
              `Stat. Category` == "Defensive" ~ "D"
            ), 
          
          se = sd / sqrt(n)
        ) %>% 
        
        plot_ly(
          type = "scatterpolar", 
          mode = "markers", 
          fill = 'toself', 
          fillcolor = "#64ed9b",
          
          r = ~mean, 
          theta = ~short_descr, 
          
          text = paste0(
            "Category: ", `Stat. Category`, 
            "<br>Average Quantile: ", round(mean, 2), 
            "<br>Standard Deviation: ", round(sd, 2)
          ), 
          hoverinfo = 'text', 
          
          opacity = 0.5,
          
          marker = list(size = 10, color = "black"), 
          
          name = " "
          
        ) %>% 
        layout(
          polar = list(
            radialaxis = list(
              visible = T,
              range = c(0,1)
            )
          ),
          margin = list(l = 25, r = 25, t = 50, b = 25), 
          title = "Average Quantiles"
        )
      
    )
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
        dens_x =  density(`Percentile per 90`)$x, 
        dens_y =  density(`Percentile per 90`)$y/ sum(density(`Percentile per 90`)$y)
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
      layout(xaxis = list(range = c(0, 1), zeroline = F, title = "Quantiles"), 
             margin = list(l = 25, r = 25, t = 50, b = 25), 
             legend = list(orientation = "h", y = -0.25), 
             yaxis = list(title = "Approximate Probability"), 
             title = "Detailed Quantile Distribtuion")
    )

    
  }

all_features_quantiles_boxplot <- 
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
    
    f <- 
      f %>% 
      mutate(
        stat_cat = 
          factor(stat_cat, 
                 levels = 
                   # order stat categories by average quantile 
                   f %>% group_by(stat_cat) %>% 
                    summarize(m = mean(`Percentile per 90`)) %>% 
                     arrange(m) %>% select(stat_cat) %>% unlist()
          )
      )
    
    with(f, 
         plot_ly() %>% 
           
           add_markers(
             x = ~jitter(as.numeric(stat_cat)), 
             y = ~`Percentile per 90`, 
             color = ~stat_cat,
             
             marker = list(size = 6),
             hoverinfo = "text",
             text = ~paste0(`Stat. Name`,"<br>Percentile: ",round(`Percentile per 90`, 2)),
             showlegend = FALSE
             ) %>% 
           
           add_trace(

             x = ~as.numeric(stat_cat),
             y = ~`Percentile per 90`,
             color = ~stat_cat,

             type = "box",
             boxpoints = F,
             hoverinfo =  "y", 
             opacity = 0.5
           ) %>%
           
           layout(
             xaxis = list(showticklabels = FALSE, title = ""), 
             legend = list(orientation = 'h'), 
             title = "Distribtuion of Quantiles"
           )
         ) 
    
  }

bar_plot_ranked_features <- 
  function(
    REACTIVE_DATA, 
    FEATURES_TO_SHOW){
    
    
         REACTIVE_DATA %>% 
           arrange(
             -`Percentile per 90`
           ) %>% 
      
          head(FEATURES_TO_SHOW) -> plot_df 
    
          plot_df$`Stat. Name` <- factor(plot_df$`Stat. Name`, levels = plot_df$`Stat. Name`)
          
          plot_df %>% 
           plot_ly() %>% 
            add_trace(
              y = ~`Percentile per 90`, 
              x = ~`Stat. Name`, 
              color = ~`Stat. Category`, 
              
              type = 'bar', 
              
              text = paste0(
                "Statistic: ", plot_df$`Stat. Name`, 
                "<br>Aggregate: ", plot_df$`Aggregate Per Season` ,
                "<br>Per 90: ", plot_df$`Scaled Per 90 Minutes` %>% round(.,2),
                "<br>Quantile per 90: ", plot_df$`Percentile per 90` %>% round(.,2)
              ), 
              
              hoverinfo = 'text'
            ) %>% 
           
           layout(xaxis = list(title = ""), 
                  yaxis = list(title = ""), 
                  legend = list(orientation = "h", x = 0, y = 1.2),
                  margin = list(l = 25, r = 25, t = 50, b = 25)
                  )
         
    
  }


one_feature_histogram <- 
  function(
    DATA, 
    TARGET_PLAYER, 
    TARGET_PLAYER_LEAGUE, 
    TARGET_PLAYER_TEAM, 
    PLOT_VAR
    ){
    
    data_dict %>% filter(Pretty.Name.from.FBref == PLOT_VAR) %>% 
      select(Data.Frame.Name) %>% unlist() %>% set_names(NULL) -> plot_var_df
    
    DATA %>% 
      select(all_of(c("summary_player", "team_name","league_name", "summary_min", plot_var_df)))  -> plot_df
    
    plot_df$per_90 <- plot_df[[plot_var_df]]/plot_df$summary_min * 90

    plot_df %>% filter(summary_player == TARGET_PLAYER & 
                         league_name == TARGET_PLAYER_LEAGUE & 
                         team_name == TARGET_PLAYER_TEAM) %>% select(per_90) %>%  unlist()-> x_t
    
    plot_df %>% filter(!(summary_player == TARGET_PLAYER & 
                         league_name == TARGET_PLAYER_LEAGUE & 
                         team_name == TARGET_PLAYER_TEAM)) -> plot_df
    plot_ly(
      data = plot_df,
      alpha = .75
    ) %>%
      add_histogram(
        x = ~per_90,
        color = ~league_name,
        text = '',
        hoverinfo = 'text',
        histnorm = "probability"
        ) %>%
      
      add_trace(
        type = "scatter", 
        mode = "lines",
        x = c(x_t, x_t),
        y = c(0,1), 
        # x0 = x_t, x1 = x_t, 
        # y0 = 0, y1 = 1, 
        line = list(color = "red", dash = "dash", width = 2), 
        name = paste0(TARGET_PLAYER, "'s Value")
      ) %>% 
      
      layout(legend = list(orientation = 'h'),
             barmode = "stack",
             yaxis = list(range = c(0,1)), 
             xaxis = list(title = '', nticks = 5), 
             title = list(
               text = paste0(PLOT_VAR," Per 90 Minutes<br>", TARGET_PLAYER, " ",round(x_t, 2)), 
               y = 1.5, 
               font = list(size = 12))
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
    
    f$percentiles <- paste0(round(f$percentiles, 4) * 100, "%")
    f$percentiles_per_90  <- paste0(round(f$percentiles_per_90, 4) * 100, "%")
    
    colnames(f) <- c("Stat. Category","Stat. Name", "Aggregate Per Season", "Percentile", "Scaled Per 90 Minutes", "Percentile")
    f
    # datatable(f, rownames=FALSE,
    #           options = 
    #                 list(
    #                  scrollX = TRUE,
    #                  scrollY = TRUE,
    #                  autoWidth = TRUE, 
    #                  rownames = F
    #                 )
    #           ) %>% 
    #   
    #   formatRound(columns = c(5), 
    #                   digits = 2)
    # 
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
                  yaxis = list(title = "Percentile",
                               standoff = 20)
                  )
    )
  }


all_features_quantiles_histogram <- 
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
  
    f %>% 
      plot_ly(
        x =~ percentiles_per_90, 
        group =~ stat_cat, 
        color =~ stat_cat, 
        alpha = .75,
        type = "histogram",
        nbinsx = 1/.1,
        marker = list(line = list(color = "#6d8085", width = 1))
      ) %>% 
      layout(barmode = "overlay",
             bargap=0.01, 
             legend = list(orientation = "h",
                           x = 0, y = -0.2), 
             xaxis = list(title = "Quantiles of Statistics per 90 Minutes",
                          tickvals = seq(from = 0, to = 1, by = 0.05)), 
             yaxis = list(title = "Count"))

    
  }

all_quantiles_summary <- 
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
    
    f %>% 
      group_by(
        stat_cat
      ) %>% 
      summarise(
        n = n(), 
        mean = mean(percentiles_per_90), 
        median = median(percentiles_per_90), 
        spread = sd(percentiles_per_90)
      ) %>% 
      
      mutate(
        mean = paste0(round(mean, 4)*100, "%"),
        median = paste0(round(median, 4)*100, "%"),
        spread = round(spread, 4)*100
      ) # %>% 
      # 
      # datatable(rownames=FALSE,
      #           colnames = c("Stat. Category", "Number fof Features", "Average Quantile", "Median Quantile", "Standard Deviation"), 
      #           options = 
      #               list(
      #                scrollX = TRUE,
      #                scrollY = TRUE,
      #                autoWidth = TRUE, 
      #                rownames = F
      #               )
      #             )
    
  }

#### player profile -- similar players 
similar_players_euclid_dist_data <- 
  function(
    REACTIVE_DATA, 
    TARGET_PLAYER, 
    TARGET_PLAYER_TEAM, 
    SEASON, 
    FEATURES_LIST
  ){
    
    additional_features <- 
      c(
        "summary_player", "summary_min", 'team_name', "league_name", "comb_positions", "summary_age", "games_played"
      )
    
    df <- 
      rbind(
        REACTIVE_DATA %>% ungroup() %>% filter(summary_player %in% TARGET_PLAYER & team_name == TARGET_PLAYER_TEAM),
        REACTIVE_DATA %>% filter(team_name != TARGET_PLAYER_TEAM)
      ) %>% 
    
    select(all_of(
        c(FEATURES_LIST, additional_features)
      ))
  
    colnames(df) <- c(FEATURES_LIST, additional_features)
    
    minutes <- df$summary_min
    
    # df_per_90 <- df %>% select(all_of(
    #       c(FEATURES_LIST)))

    df_per_90 <- df %>%
      mutate_at(
        vars(all_of(c(FEATURES_LIST))), ~ . / summary_min * 90)
    
    df_per_90 %>% 
      select(all_of(FEATURES_LIST)) %>% 
      summarise(
        across(everything(), ~ mean(., na.rm = T))
      )  -> means 
    
    df_per_90 %>% 
      select(all_of(FEATURES_LIST)) %>% 
      summarise(
        across(everything(), ~ sd(., na.rm = T))
      )  %>% t()  -> sds
    
    df_per_90 <- 
      df_per_90 %>% 
      mutate_at(
        #across(all_of(c(FEATURES_LIST)), 
              vars(all_of(c(FEATURES_LIST))), 
               ~(. - mean(.))/sd(.)
               )
      
    df_per_90 <- na.omit(df_per_90)
    
    points <- df_per_90 %>% filter(summary_player %in% TARGET_PLAYER & team_name %in% TARGET_PLAYER_TEAM) %>%
     select(all_of(FEATURES_LIST)) %>%
     setNames(FEATURES_LIST) %>% unlist()

    df_per_902 <-
       df_per_90 %>%
        select(all_of(FEATURES_LIST)) %>%
        setNames(FEATURES_LIST) %>%
        mutate(across(everything(), ~ (. - points[match(cur_column(), names(points))])^2
                      )
               )

    dist = sqrt(rowSums(df_per_902))

    md <- max(dist)

    distance_data =
      cbind(df, dist) %>%
      mutate(scaled_dist = dist / md)

    return(distance_data)
  }

pca_results <- 
  function(
    REACTIVE_DATA, 
    TARGET_PLAYER, 
    TARGET_PLAYER_TEAM, 
    SEASON, 
    FEATURES_LIST
  ){
    
    df <- REACTIVE_DATA %>% filter(  !team_name %in% TARGET_PLAYER_TEAM | 
                                       (summary_player == TARGET_PLAYER & 
                                          team_name == TARGET_PLAYER_TEAM)) %>% as.data.table() %>% 
    
    select(all_of(
        c(FEATURES_LIST, "summary_player", "summary_min", 'team_name', "league_name")
      ))
  
    colnames(df) <- c(FEATURES_LIST, "summary_player", "summary_min", 'team_name', "league_name")
    
    minutes <- df$summary_min
    
    # df_per_90 <- df %>% select(all_of(
    #       c(FEATURES_LIST)))

    df_per_90 <- df %>%
      mutate_at(
        vars(all_of(c(FEATURES_LIST))), ~ . / summary_min * 90)
    
    df_per_90 <- na.omit(df_per_90)
      
      set.seed(1)
      pca_res <- prcomp(df_per_90 %>% select(all_of(FEATURES_LIST)), scale = T)
      
      return(pca_res)
  }
           
pca_results_data <- 
  function(
    REACTIVE_DATA, 
    TARGET_PLAYER, 
    TARGET_PLAYER_TEAM, 
    SEASON, 
    FEATURES_LIST
  ){
    
    df <- REACTIVE_DATA %>% filter(  (!team_name %in% TARGET_PLAYER_TEAM) | 
                                       (summary_player %in% TARGET_PLAYER & 
                                          team_name %in% TARGET_PLAYER_TEAM)) %>% 
    
    select(all_of(
        c(FEATURES_LIST, "summary_player", "summary_min", 'team_name', "league_name")
      ))
  
    colnames(df) <- c(FEATURES_LIST, "summary_player", "summary_min", 'team_name', "league_name")
    
    minutes <- df$summary_min
    
    # df_per_90 <- df %>% select(all_of(
    #       c(FEATURES_LIST)))

    df_per_90 <- df %>%
      mutate_at(
        vars(all_of(c(FEATURES_LIST))), ~ . / summary_min * 90)
    
    df_per_90 <- na.omit(df_per_90)
      
      set.seed(1)
      pca_res <- prcomp(df_per_90 %>% select(all_of(FEATURES_LIST)), scale = T)
      
      summary(pca_res)$importance %>% t() -> d
    
      d <- data.frame(d)
      
      
      if(d %>% filter(`Cumulative.Proportion` < .8) %>% nrow() == 1){
        d <- head(d, 2)
      }else(
        d <- d %>% filter(`Cumulative.Proportion` < .8)
      )
      
    d$pc <- as.character(rownames(d))
    
    
    plot_df <- 
      cbind(df, 
            pca_res$x[,c(d$pc)])
      
      return(plot_df)
  }       

similar_pca_table <- 
  function(
    PCA_RES
  ){
    summary(PCA_RES)$importance %>% t() -> d
    
    d <- data.frame(d)
    
    if(d %>% filter(`Cumulative.Proportion` < .8) %>% nrow() == 1){
      d <- head(d, 2)
    }else(
      d <- d %>% filter(`Cumulative.Proportion` < .8)
    )
    
    d$pc <- as.character(rownames(d))
    
    d <- d[,-1]
    
    d[,1 ] <- paste0(round(d[,1], 4) * 100, "%")
    
    d[,2 ] <- paste0(round(d[,2], 4) * 100, "%")
    
    colnames(d) <- c("Proportion of Variance", "Cumulative", "PC")
    
    d <- d %>% select(PC, everything())
    
    d
  }

similar_players_table <- 
  function(
    DATA_W_DISTANCE, 
    TOP_PLAYERS_SHOW
  ){

     DATA_W_DISTANCE  %>%
        
 #     similar_players_reactive_data %>% 
        filter(scaled_dist != 0) %>% 
        select(
          summary_player, summary_age, comb_positions, team_name, league_name, scaled_dist, summary_min, games_played
        ) %>% 
        arrange(scaled_dist) %>% 
   #     head(10) %>% 
        mutate(
          scaled_dist = round(scaled_dist, 4), 
          summary_min = prettyNum(summary_min, big.mark = ",")
          
        ) %>% 
        head(TOP_PLAYERS_SHOW)  %>% 
        rename(
          
          `Player Name` = summary_player, 
          Age = summary_age, 
          `Position (Games Playerd)` = comb_positions, 
          `Team` = team_name, 
          `League` = league_name, 
          `Similarity Score` = scaled_dist, 
          `Minutes Played` = summary_min, 
          `Games Played` = games_played
          
        ) %>% 
        
        datatable(
          options = list(
            scrollX = T,
            rownames = NULL,
            pageLength = 5
            )
          )
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
    
    
    selected <- REACTIVE_PCA_DF[(REACTIVE_PCA_DF$summary_player == PLAYER & 
                                  REACTIVE_PCA_DF$team_name == TEAM), ]
    
    similar <- REACTIVE_PCA_DF[REACTIVE_PCA_DF$summary_player %in% SIMILAR_PLAYERS$summary_player & 
                                 REACTIVE_PCA_DF$team_name %in% SIMILAR_PLAYERS$team_name & 
                                 !(REACTIVE_PCA_DF$summary_player == PLAYER & 
                                      REACTIVE_PCA_DF$team_name == TEAM), ]
    
    all_play <- 
      REACTIVE_PCA_DF[!(
        REACTIVE_PCA_DF$summary_player == PLAYER & 
        REACTIVE_PCA_DF$team_name == TEAM
      ) 
      & !(
        REACTIVE_PCA_DF$summary_player %in% SIMILAR_PLAYERS$summary_player & 
        REACTIVE_PCA_DF$team_name %in% SIMILAR_PLAYERS$team_name & 
          !(REACTIVE_PCA_DF$summary_player == PLAYER & 
            REACTIVE_PCA_DF$team_name == TEAM)
      ),
      ]
    
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
             legend = list(orientation = 'h', y = -0.25))
      
  }

similar_mds_plot <- 
  function(
    REACTIVE_DIST_DF, 
    FEATURES_LIST, 
    TARGET_PLAYER, 
    TARGET_PLAYER_TEAM, 
    SEASON,
    SIMILAR_PLAYERS
  ){
    
    df_per_90 <- REACTIVE_DIST_DF %>%
      mutate_at(
        vars(all_of(c(FEATURES_LIST))), ~ . / summary_min * 90) %>% 
      
      select(all_of(FEATURES_LIST))
    
    colnames(df_per_90) <- FEATURES_LIST
    
    df_per_90 %>% 
      select(all_of(FEATURES_LIST)) %>% 
      summarise(
        across(everything(), ~ mean(., na.rm = T))
      )  -> means 
    
    df_per_90 %>% 
      select(all_of(FEATURES_LIST)) %>% 
      summarise(
        across(everything(), ~ sd(., na.rm = T))
      )  %>% t()  -> sds
    
    df_per_90 <- 
      df_per_90 %>% 
      mutate_at(
        #across(all_of(c(FEATURES_LIST)), 
              vars(all_of(c(FEATURES_LIST))), 
               ~(. - mean(.))/sd(.)
               )
    
    mds_result <- cmdscale(dist(df_per_90), k = 2) %>% data.frame()# k = 2 for 2D MDS

    mds_result <- 
      cbind(
        mds_result, 
        REACTIVE_DIST_DF %>% 
          select(summary_player, team_name, league_name)
      )
    
    selected <- mds_result[(mds_result$summary_player == TARGET_PLAYER & 
                                  mds_result$team_name == TARGET_PLAYER_TEAM), ]
    
    similar <- mds_result[mds_result$summary_player %in% SIMILAR_PLAYERS$summary_player & 
                                 mds_result$team_name %in% SIMILAR_PLAYERS$team_name & 
                                 !(mds_result$summary_player == TARGET_PLAYER & 
                                      mds_result$team_name == TARGET_PLAYER_TEAM), ]
    
    all_play <- 
      mds_result[!(
        mds_result$summary_player == TARGET_PLAYER & 
        mds_result$team_name == TARGET_PLAYER_TEAM
      ) 
      & !(
        mds_result$summary_player %in% SIMILAR_PLAYERS$summary_player & 
        mds_result$team_name %in% SIMILAR_PLAYERS$team_name & 
          !(mds_result$summary_player == TARGET_PLAYER & 
            mds_result$team_name == TARGET_PLAYER_TEAM)
      ),
      ]
    
    plot_ly() %>% 
      add_trace(
        data = all_play, 
        x = ~all_play$X1, 
        y = ~all_play$X2, 
        
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
        x = ~similar$X1, 
        y = ~similar$X2, 
        
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
        x = ~selected$X1, 
        y = ~selected$X2,
        
        name = TARGET_PLAYER, 
        
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
      
      layout(xaxis = list(title = '', zeroline = F), 
             yaxis = list(title = '', zeroline = F),
             legend = list(orientation = 'h', y = -0.25))
      
    
  }
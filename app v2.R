                                            #####################################
                                            # LOAD DATA, PACKAGES, OTHER SET UP #
                                            #####################################
                                            
source("Master Packages.R")

                                            ##############################
                                            # FUNCTIONS WITH NO HOME YET #
                                            ##############################
                                            
combine_strings <- function(input_strings) {
  # Initialize an empty list to store parsed data from all strings
  parsed_data <- list()
  
  # Loop through each input string
  for (input_str in input_strings) {
    # Split and parse the current input string
    pairs <- strsplit(gsub("\\s+", "", input_str), ",")[[1]]
    parsed <- setNames(as.numeric(gsub("\\D+", "", pairs)), gsub("\\d+", "", pairs))
    names(parsed) <- gsub("\\(.*\\)", "", names(parsed))
    
    parsed_data <- c(parsed_data, parsed)
  }
  
  # Combine and aggregate the values
  combined <- do.call(c, parsed_data)
  aggregated <- aggregate(values ~ ind, data = stack(combined), sum)
  
  # Construct the final combined string with spaces
  combined_string <- paste(aggregated$ind, "(", aggregated$values, ")", collapse = ", ")
  
  return(combined_string)
}

                                            ####################################
                                            # APP MACRO ENVIRONMENT VARIABLES #
                                            ###################################
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
                                            
### data dictionary stuff

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
          select(-all_positions, -summary_age)
        
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
        select(-all_of(remove_colnames_keep_games_mins)) %>% 
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
        ) %>% select(summary_player,	season,	team_name, league_name, comb_positions, everything())
      
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
           c("league_name","all_positions","summary_age","dominant_position")
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
        coord2 = c(50,50,35,40,65,50,55,85,80,90,60,15, 20, 15,90)
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
      )
  }

percentile_data_frame_one_player <- 
  function(
    DATA, 
    PLAYER, 
    TEAM
  ){
  # selected player data 
    player_df <-  DATA[DATA$summary_player == PLAYER & DATA$team_name == TEAM, ] 
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
                  margin = list(l = 25, r = 25, t = 50, b = 25))
         
    
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
    
    df <- 
      rbind(
        REACTIVE_DATA %>% ungroup() %>% filter(summary_player %in% TARGET_PLAYER & team_name == TARGET_PLAYER_TEAM),
        REACTIVE_DATA %>% filter(team_name != TARGET_PLAYER_TEAM)
      ) %>% 
    
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
    
    d <- d[d[,3] < .85, ]
    d <- data.frame(d)
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
    
    all_play <- REACTIVE_PCA_DF[!(REACTIVE_PCA_DF$summary_player == PLAYER & 
                                  REACTIVE_PCA_DF$team_name == TEAM) & 
                                  !(REACTIVE_PCA_DF$summary_player %in% SIMILAR_PLAYERS & 
                                      REACTIVE_PCA_DF$team_name %in% SIMILAR_PLAYERS$team_name), 
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
             legend = list(orientation = 'h'))
      
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
    
        # dynamic picker for a player summary team 
        
        ## sometimes we have two + players with the same name playing on different teams. If we type in such a name, we need to be able to 
        ##  select a team 
       
      player_seasons <- 
        reactive(dash_df[summary_player %in% selected_player_profile_name() ] %>% 
                 select(season) %>% unique() %>% unlist() %>% 
                     set_names(NULL))
    
      
      output$picked_player_available_seasons <- 
            renderUI({
              
              selectInput(inputId = 'select_season', 
                           label = "Select a Season", 
                           choices = player_seasons(),  
                           selected = dash_df[dash_df$summary_player == selected_player_profile_name()]$season %>% tail(1) %>% unlist()
                          )
          })
    
      same_name_teams <- 
        reactive(dash_df[summary_player %in% selected_player_profile_name() & 
                           season == input$select_season ] %>% 
                 select(team_name) %>% unique() %>% unlist() %>% sort() %>% 
                     set_names(NULL))
    
      output$same_name_team_picker <- 
            renderUI({
              
              selectInput(inputId = 'select_team_same_name', 
                           label = "Pick a team. Likely only 1 available", 
                           choices = same_name_teams(), 
                           selected = dash_df[dash_df$summary_player == selected_player_profile_name() ][1,]$team_name
                          )
          })
     
    
      same_name_leagues <- 
        reactive(dash_df[summary_player %in% selected_player_profile_name() & 
                           season == input$select_season] %>% 
                 select(league_name) %>% unique() %>% unlist() %>% sort() %>% 
                     set_names(NULL))
    
      output$same_name_leagues_picker <- 
            renderUI({
              
              selectInput(inputId = 'select_league', 
                           label = "Select Competiton", 
                           choices = same_name_leagues(), 
                           selected = dash_df[dash_df$summary_player == selected_player_profile_name()][1,]$league_name
                          )
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
                                   dash_df[summary_player == selected_player_profile_name()  ]$all_positions %>% unlist() %>% sort()), 
                              ", ")[[1]], 
                      multiple = T
                        )
        )
      
          output$table_w_names <- 
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
          
      ########## PLAYER PROFILE SUMMARY PAGE 
      
      ## big name print 
      output$rendered_player_header <- 
        renderUI(
          tags$h1(paste0(input$player_typed_name, " Player Profile"), 
                  style = paste0("font-size:", "30px", "; text-align:center; font-weight: bold;"))
        )
      
      ##### data frame that is limited to front page parameters 
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
          
      output$brief_summary_and_pool <- 
        renderTable(
          brief_summary_and_pool(
            REACTIVE_DATA = reactive_player_summary_df(), 
            TARGET_PLAYER = input$player_typed_name, 
            TARGET_PLAYER_TEAM = input$select_team_same_name
          ), 
          hover = T, align = 'c', striped = T, width = "75%"
        )
      output$create_field_plot <- 
        renderPlot(
          create_field_plot(
            REACTIVE_DATA = reactive_player_summary_df(),
            TARGET_PLAYER = input$player_typed_name, 
            TARGET_PLAYER_SEASON = input$select_season,
            TARGET_PLAYER_TEAM = input$select_team_same_name
            )
        )

      percentile_data_frame_one_player_df <-
        reactive(
          percentile_data_frame_one_player(
            DATA = reactive_player_summary_df(),
            PLAYER = input$player_typed_name,
            TEAM = input$select_team_same_name
          ))

      all_features_ranked_w_names <- 
        reactive(
          all_features_ranked(
            REACTIVE_DATA =  percentile_data_frame_one_player_df()
            )
        )
      
      average_quantiles_data_df <- 
        reactive(
          average_quantiles_data(
            REACTIVE_DATA = all_features_ranked_w_names()
          )
        )
      
      output$attacking_gauge <- 
        renderGauge(
          create_gauge(
            REACTIVE_DATA = average_quantiles_data_df(), 
            CATEGORY = "Attacking"
          )
        )
      
      output$defensive_gauge <- 
        renderGauge(
          create_gauge(
            REACTIVE_DATA = average_quantiles_data_df(), 
            CATEGORY = "Defensive"
          )
        )
      
      output$misc_gauge <- 
        renderGauge(
          create_gauge(
            REACTIVE_DATA = average_quantiles_data_df(), 
            CATEGORY = "Misc"
          )
        )
      
      output$ball_gauge <- 
        renderGauge(
          create_gauge(
            REACTIVE_DATA = average_quantiles_data_df(), 
            CATEGORY = "On The Ball"
          )
        )
      
      output$pass_type_gauge <- 
        renderGauge(
          create_gauge(
            REACTIVE_DATA = average_quantiles_data_df(), 
            CATEGORY = "Pass Type Detail"
          )
        )
      
      output$Passing_type_gauge <- 
        renderGauge(
          create_gauge(
            REACTIVE_DATA = average_quantiles_data_df(), 
            CATEGORY = "Passing"
          )
        )
      
      output$attacking_gauge_slider <- 
        renderGauge(  
          create_gauge_count(
              REACTIVE_DATA = all_features_ranked_w_names(), 
              CATEGORY = "Attacking", 
              SLIDER_INPUT = input$quantile_cutoffs
            )
        )
      
      output$defensive_gauge_slider <- 
        renderGauge(
          create_gauge_count(
            REACTIVE_DATA = all_features_ranked_w_names(), 
            CATEGORY = "Defensive", 
              SLIDER_INPUT = input$quantile_cutoffs
          )
        )
      
      output$misc_gauge_slider <- 
        renderGauge(
          create_gauge_count(
            REACTIVE_DATA = all_features_ranked_w_names(), 
            CATEGORY = "Misc", 
              SLIDER_INPUT = input$quantile_cutoffs
          )
        )
      
      output$ball_gauge_slider <- 
        renderGauge(
          create_gauge_count(
            REACTIVE_DATA = all_features_ranked_w_names(), 
            CATEGORY = "On The Ball", 
              SLIDER_INPUT = input$quantile_cutoffs
          )
        )
      
      output$pass_type_gauge_slider <- 
        renderGauge(
          create_gauge_count(
            REACTIVE_DATA = all_features_ranked_w_names(), 
            CATEGORY = "Pass Type Detail", 
              SLIDER_INPUT = input$quantile_cutoffs
          )
        )
      
      output$Passing_type_gauge_slider <- 
        renderGauge(
          create_gauge_count(
            REACTIVE_DATA = all_features_ranked_w_names(), 
            CATEGORY = "Passing", 
              SLIDER_INPUT = input$quantile_cutoffs
          )
        )
      
      output$radar_quantiles_chart <- 
        renderPlotly(
          radar_quantiles_chart(
            REACTIVE_DATA = average_quantiles_data_df()
          )
        )
      
      output$all_features_quantiles_density <- 
        renderPlotly(
          all_features_quantiles_density(
            REACTIVE_DATA = all_features_ranked_w_names()
          )
        )
    
      output$bar_plot_ranked_features <- 
        renderPlotly(
          bar_plot_ranked_features(
            REACTIVE_DATA = all_features_ranked_w_names(), 
            FEATURES_TO_SHOW = input$show_features_plotly)
        )
      
      ########## 
      # similar player stuff 
      
      best_features_vector <- 
        reactive(
          player_best_features_vector(
            REACTIVE_DATA = all_features_ranked_w_names(), 
            N_FEATURES = input$top_features_used
          )
        )
      
      similar_players_euclid_dist_data_df <- 
        reactive(
          similar_players_euclid_dist_data(
            REACTIVE_DATA = reactive_player_summary_df(), 
            TARGET_PLAYER = input$player_typed_name, 
            TARGET_PLAYER_TEAM = input$select_team_same_name, 
            SEASON = input$select_season, 
            FEATURES_LIST = best_features_vector()
          )
        )
      
      data_from_pca <- 
        reactive(
          pca_results_data(
            REACTIVE_DATA = reactive_player_summary_df(), 
            TARGET_PLAYER = input$player_typed_name, 
            TARGET_PLAYER_TEAM = input$select_team_same_name, 
            SEASON = input$select_season, 
            FEATURES_LIST = best_features_vector()
          )
        )
      
      pca_res <- 
        reactive(
          pca_results(
            REACTIVE_DATA = reactive_player_summary_df(), 
            TARGET_PLAYER = input$player_typed_name, 
            TARGET_PLAYER_TEAM = input$select_team_same_name, 
            SEASON = input$select_season, 
            FEATURES_LIST = best_features_vector()
          )
        )
      
       output$pc_pick_1 <- 
            renderUI({
              
              selectInput(inputId = 'select_pc_plot_1', 
                           label = "Pick a PC for X-axis", 
                           choices = colnames(data_from_pca())[grep("PC", colnames(data_from_pca()))], 
                           selected = colnames(data_from_pca())[grep("PC", colnames(data_from_pca()))][1])
          })
    
       output$pc_pick_2 <- 
            renderUI({
              
              selectInput(inputId = 'select_pc_plot_2', 
                           label = "Pick a PC for Y-axis", 
                           choices = colnames(data_from_pca())[grep("PC", colnames(data_from_pca()))], 
                           selected = colnames(data_from_pca())[grep("PC", colnames(data_from_pca()))][2])
          })
      
      output$test_table_g <- 
        renderDataTable({
          
          similar_players_euclid_dist_data_df()  %>%
            arrange(scaled_dist) %>% 
            head(input$target_sim_players)  %>% 
            datatable(options = list(scrollX = T))
          
        })
      
      simialr_players_df <- 
        reactive(
          similar_players_euclid_dist_data_df()  %>% 
            arrange(scaled_dist) %>% 
            head(input$target_sim_players) 
        )
      
      output$similar_pca_plot <-
        renderPlotly(
          similar_pca_plot(
              REACTIVE_PCA_DF = data_from_pca(),
              PLAYER = input$player_typed_name,
              TEAM = input$select_team_same_name,
              SIMILAR_PLAYERS = simialr_players_df(),
              X_AXIS_PC = input$select_pc_plot_1, 
              Y_AXIS_PC = input$select_pc_plot_2
              )
        )
      
    output$league_check <- 
      renderTable(
        data_from_pca() %>% 
          group_by(league_name) %>% summarise(n = n())
      )
      
    output$similar_pca_table <- 
      renderTable(
        similar_pca_table(
          PCA_RES = pca_res()
        )
      )
    
      # output$dynamic_table_summary <- 
      #   renderTable({
      #     dynamic_table_summary(
      #       REACTIVE_DATA =  percentile_data_frame_one_player_df(), 
      #       COLUMNS = input$feature
      #       )}, 
      #     striped = T
      #     )
      # 
      # output$like_pizza_but_bar_graph <- 
      #   renderPlotly(
      #     like_pizza_but_bar_graph(
      #       COLUMNS = input$feature, 
      #       REACTIVE_DATA = percentile_data_frame_one_player_df()  
      #     )
      # )
      # 
      # output$all_features_quantiles_histogram <- 
      #   renderPlotly(
      #     all_features_quantiles_histogram(
      #       REACTIVE_DATA = percentile_data_frame_one_player_df()
      #     )        
      #   )
      # 
      # output$all_quantiles_summary <- 
      #   renderTable(
      #     all_quantiles_summary(
      #       REACTIVE_DATA = percentile_data_frame_one_player_df()
      #     )        
      #   )
      
      ##### data frame
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
        tags$style(HTML('
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
                              
                              /* Add custom CSS rules here */
                              .gauge-text {
                                font-size: 20px; /* Adjust the font size as needed */
                              }
                              '
                              )), 
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
                                
                                    uiOutput('picked_player_available_seasons') %>% withSpinner(color="#0dc5c1"), 
                                    uiOutput('same_name_team_picker') %>% withSpinner(color="#0dc5c1"), 
                                    uiOutput('same_name_leagues_picker') %>% withSpinner(color="#0dc5c1"), 
                                
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
                                  dataTableOutput('table_w_names'))
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
                                                title = "Pushing this button generates reports on two next 
                                                tabs based on selected parameters. ")), 
                               
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
                         fluidPage(
                           fluidRow(
                             column(width = 6, align="center", 
                                    fluidRow(
                                      
                                    uiOutput('rendered_player_header') %>% withSpinner(color="#0dc5c1"), 
                                    tableOutput('brief_summary_and_pool') %>% withSpinner(color="#0dc5c1"), 
                                    plotOutput('create_field_plot') %>% withSpinner(color="#0dc5c1")
                                    )
                                    ), 
                             column(width = 6, 
                                    tabsetPanel(
                                      tabPanel("Main", 
                                               plotlyOutput('radar_quantiles_chart') %>% withSpinner(color="#0dc5c1")
                                               ), 
                                      tabPanel("Quantile Distribution", 
                                               plotlyOutput('all_features_quantiles_density') %>% withSpinner(color="#0dc5c1")
                                               )
                                    )
                                    )
                           ), 
                           fluidRow(
                           fluidRow(
                             column(width = 6, 
                              box(width = 12, title = "Average Quantiles By Category", height = "325px", 
                                column(gaugeOutput('attacking_gauge',
                                                  width = "100%", height = "125px") %>% withSpinner(color="#0dc5c1"), 
                                      gaugeOutput('defensive_gauge',
                                                  width = "100%", height = "125px") %>% withSpinner(color="#0dc5c1"), width = 4), 
                                column(gaugeOutput('ball_gauge',
                                                  width = "100%", height = "125px") %>% withSpinner(color="#0dc5c1"), 
                                      gaugeOutput('pass_type_gauge',
                                                  width = "100%", height = "125px") %>% withSpinner(color="#0dc5c1"), width = 4), 
                                column(gaugeOutput('Passing_type_gauge',
                                                  width = "100%", height = "125px") %>% withSpinner(color="#0dc5c1"), 
                                      gaugeOutput('misc_gauge',
                                                  width = "100%", height = "125px") %>% withSpinner(color="#0dc5c1"), width = 4)
                           )
                          ),
                           column(width = 6,
                            box(width = 12, title = "Number of Quantiles By Anove Threshold", height = "325px", 
                                column(gaugeOutput('attacking_gauge_slider',
                                                  width = "100%", height = "125px") %>% withSpinner(color="#0dc5c1"), 
                                      gaugeOutput('defensive_gauge_slider',
                                                  width = "100%", height = "125px") %>% withSpinner(color="#0dc5c1"), width = 4), 
                                column(gaugeOutput('ball_gauge_slider',
                                                  width = "100%", height = "125px") %>% withSpinner(color="#0dc5c1"), 
                                      gaugeOutput('pass_type_gauge_slider',
                                                  width = "100%", height = "125px") %>% withSpinner(color="#0dc5c1"), width = 4), 
                                column(gaugeOutput('Passing_type_gauge_slider',
                                                  width = "100%", height = "125px") %>% withSpinner(color="#0dc5c1"), 
                                      gaugeOutput('misc_gauge_slider',
                                                  width = "100%", height = "125px") %>% withSpinner(color="#0dc5c1"), width = 4)
                           )
                           )
                ), 
                fluidRow(
                                      sliderInput(inputId = "quantile_cutoffs", 
                                                  label = "Cutoff for Count", 
                                                  min = 0, max = 100, 
                                                  value = 80, 
                                                  step = 1
                                                    
                                                    ), 
                                      numericInput(inputId = 'show_features_plotly', 
                                                   label  = "Number of Top Features To Show", 
                                                   min = 5, 
                                                   max = 100, 
                                                   value = 20)), 
                          plotlyOutput('bar_plot_ranked_features') %>% withSpinner(color="#0dc5c1"))
                )
                ), 
                
                tabPanel(title = "Similar Players", 
                         tabsetPanel(
                           tabPanel("Best Features Based", 
                                    fluidPage(
                                      column(width = 3,
                                             numericInput(inputId = 'target_sim_players', 
                                                              label = "Number of Similar Players to Find", 
                                                              min = 0, 
                                                              max = 1000, 
                                                              value = 30), 
                                             numericInput(inputId = 'top_features_used', 
                                                          label = "Number of Features to Use", 
                                                          min = 0, 
                                                          max = 100, 
                                                          value = 20), 
                                            
                                            tableOutput('similar_pca_table'), 
                                             uiOutput('pc_pick_1') %>% withSpinner(color="#0dc5c1"), 
                                             uiOutput('pc_pick_2') %>% withSpinner(color="#0dc5c1"), 
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
                                                         multiple=TRUE)
                                             
                                            ), 
                                      column(width = 9, 
                                             dataTableOutput('test_table_g') %>% withSpinner(color="#0dc5c1"), 
                                             plotlyOutput('similar_pca_plot') %>% withSpinner(color="#0dc5c1"), 
                                             tableOutput('league_check')
                                             )
                                    )
                                    ),
                           tabPanel("All Features Based",
                                    fluidPage(
                                      column(width = 3), 
                                      column(width = 9)
                                    )),
                           tabPanel("Selected Features Based",
                                    fluidPage(
                                      column(width = 3), 
                                      column(width = 9)
                                    ))
                         ))
                
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
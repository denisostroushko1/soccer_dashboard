

roll_up_data <- 
  function(big_data){
    
    big_data %>% 
      mutate(summary_age = as.numeric(substr(summary_age, 1, 2))) %>% 
      select(season, summary_player, team_name, league_name, summary_pos, summary_age) %>% 
      separate_rows(summary_pos) %>% 
      group_by(season, summary_player, team_name, league_name, summary_pos) %>% 
      summarise(
        games_played = n(),
        summary_age = max(summary_age)
      )  %>% 
      mutate(p_played = paste0(round(prop.table(games_played), 4) * 100 , "%"))  %>% 

      mutate(
        detailed = paste0(summary_pos, "(", games_played, ")"), 
        detailed2 = paste0(summary_pos, "(", p_played, ")" ),
      ) %>% 
      arrange(season, summary_player, team_name, league_name, -games_played) %>% 
      select(-summary_pos, -games_played) %>% 
      summarise(
        all_positions = paste(unique(detailed), collapse = ", "),
        dominant_position = paste(unique(detailed2), collapse = "                  "),
        summary_age = max(summary_age)
      ) %>% 
      mutate(
        dominant_position = substr(dominant_position, 1, 10)
        ) %>% 
      
      arrange(summary_player, season, team_name) %>% unique() -> positions_data
    
    big_data %>% 
      select(
        -fb_ref_match_link, 	       # to granular 
        -game_date,                 # to granular  
        -summary_jersey_number,     # players change jersey number over the season and we have duplicate records per season 
        -summary_nation,            # same as jersey number I guess 
       # -X,                         # byproduct of import 
        -summary_pos,               # already used 
        -summary_age                # already used 
      ) %>% 
      
      group_by(season, summary_player, team_name, league_name) %>% 
    
      summarize(
        games_played = n(), 
        across(where(is.numeric), sum)
        ) -> rollup_data 
    
    matches <- grep("prct", colnames(rollup_data), ignore.case = TRUE, value = TRUE)
    
    rollup_data <- rollup_data[ ,-which(colnames(rollup_data) %in% matches)]

    rollup_data <- 
      positions_data %>% 
      left_join(rollup_data, 
                by = c("season", 'summary_player', 'team_name', 'league_name'))
    
    return(rollup_data)
  }


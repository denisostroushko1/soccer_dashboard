

roll_up_data <- 
  function(big_data){
    
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
        across(where(is.numeric), sum)
        ) -> rollup_data 
    
    matches <- grep("prct", colnames(rollup_data), ignore.case = TRUE, value = TRUE)
    
    rollup_data <- rollup_data[ ,-which(colnames(rollup_data) %in% matches)]

    return(rollup_data)
  }


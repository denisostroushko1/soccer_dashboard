
source('Master Packages.R')

detail_fb_data_for_all_matches <- 
  function(matches_urls, time_pause){
    
   incomplete_counter = 0
   res_store <- matrix(nrow = length(matches_urls) * 34 + 1 # add an extra row to just be safe 
                        , ncol = 126)
    
    pb <- progress::progress_bar$new(total = length(matches_urls))
    
    print("Started looping to fill in results from FBref")
    
    for(i in 1:length(matches_urls)){

       Sys.sleep(time_pause)
       
       match_page <- load_page(matches_urls[i]) # get 'html' code for the webpage containing tables we need 
       match_page_char <- as.character(match_page) # get all of html code as one giant text string. Need it to find IDs of tables 
      
        #home team name - in the first position 
          tryCatch({
                home_team <- match_page %>% rvest::html_nodes("div+ strong a") %>% 
                  rvest::html_text() %>% .[1]
              }, error = function(e) {
                home_team <- NA
              })
        #away team name - in the second position 
            tryCatch({
                away_team <- match_page %>% rvest::html_nodes("div+ strong a") %>% 
                  rvest::html_text() %>% .[2]
              }, error = function(e) {
                away_team <- NA
              })
       
       tryCatch({league_name <- get_league_name(matches_urls[i])}, error = function(e){league_name = NA})
       
        #game date
          match_date <- match_page %>% rvest::html_nodes(".venuetime") %>% 
            rvest::html_attr("data-venue-date")
          
          # if match date is not available at the website then we just say the date was empty. 
          # at this point we are not looking at specific match level data so we can use placeholders for missing dates 
          match_date <- ifelse(length(match_date) == 0, "",  match_date)
          
        find_res <- gregexpr('switcher_player_stats_', match_page_char) # these are positions of where the tables are within the giant HTML text 
        
        
        # find_res[[1]] this element contains positions where tables ID are stored in the table 
        # we will need find_res[[1]][1] for team 1 and find_res[[1]][3] for team 2 
            
            # team 1 is a home team 
        
        team_1_table_id <- substr(match_page_char, find_res[[1]][1], find_res[[1]][1] + 29)
        team_1_table_id_f <- paste0("#", team_1_table_id) # add a pound here cause that's how we need this later to use with rvest 
        
        ### APPARENTLY SOMETIMES THERE ARE JSUT NO ADVANCED STATS FOR A TEAM: 
        # Example: all field players for boremouth had stats missing
        # this link contains an example: 
        # https://fbref.com/en/matches/9a63d65c/Bournemouth-Watford-January-2-2019-Premier-League
        
        
        
            # team 2 is an away team 
        
        team_2_table_id <- substr(match_page_char, find_res[[1]][3], find_res[[1]][3] + 29)
        team_2_table_id_f <- paste0("#", team_2_table_id)
      
        ### empty matrix to repopulate with results: I estimate that FBref has 122 columns for all 6 tables that we want 
        ### then, in each match, we have 11 players plus up to 5 subs, so a total of 16 players for a team 
        ### that times two is 32 rows. 
        ### then we have number of matches times 32 = total number of rows. 
        
        # this variable counts how many matches in the update have incomplete information 
        if(find_res[[1]][1] == -1 | 
           is.na(find_res[[1]][3])){
          
            incomplete_counter = incomplete_counter + 1
        }
        
        if(find_res[[1]][1] != -1 & 
           !is.na(find_res[[1]][3])){ # this condition handles the fact when both tables are not populated for both teams. 
                                    # this happens when we have games cancelled, such as due to COVID in 2020.
                                    # we also handle Champions leagues and cup games where participating teams do not have 
                                    # proper statistics recore. Often in the qualifying rounds small teams will have 
                                    # no advanced statistics 
          
          # save 
          
          team_1_table <- # this stores 6 tables in the results 
              match_page %>% rvest::html_nodes(team_1_table_id_f) %>% 
              rvest::html_nodes("div")
          
          team_2_table <- # this stores 6 tables in the results 
              match_page %>% rvest::html_nodes(team_2_table_id_f) %>% 
              rvest::html_nodes("div")
          
          last_saved_team_2_table <- team_2_table
          
          # now we need to use these tables and store them in the results matrix
          # columns 1 to 31 - summary from element 1 
          # 32 to 54 - passing from element 2, get columns 7 to last one 
          # 55 to 67 - pass type from element 3, get form column 7 
          # 68 to 84 - defensive actions element 4 
          # 85 to 107 - possession 
          # 108 to 124 - misc. 
          
          team_1_row_from <- 1 + 17 * (i-1)
     #     team_1_row_to   <- 16 + 16 * (i-1)
          team_1_row_to <- team_1_row_from + nrow(get_data_matrix(team_1_table, element = 2)) - 1
          
          team_2_row_from <- 1 + length(matches_urls) * 17 + 17 * (i-1)
     #     team_2_row_to   <- 16 + length(matches_urls) * 16 + 16 * (i-1)
          team_2_row_to   <- team_2_row_from + nrow(get_data_matrix(team_2_table, element = 2)) - 1
            
          #store team 1 results 
          if(!is.null(nrow(get_data_matrix(team_1_table, element = 1)))){
              # basically if we have no data in the first table, then as of 3/16/23 I assume that all 6 tables are missing, 
              # and I just discard the data. Of course I am making an assumption that the data for a team
              # in any given match can be missing for an absolutely random reason 
            
            res_store[team_1_row_from:team_1_row_to,1:31]    <- get_data_matrix(team_1_table, element = 1)
            res_store[team_1_row_from:team_1_row_to,32:53]   <- get_data_matrix(team_1_table, element = 2)[,-c(1:6)]
            res_store[team_1_row_from:team_1_row_to,54:68]   <- get_data_matrix(team_1_table, element = 3)[,-c(1:6)]
            res_store[team_1_row_from:team_1_row_to,69:84]   <- get_data_matrix(team_1_table, element = 4)[,-c(1:6)]
            res_store[team_1_row_from:team_1_row_to,85:106]  <- get_data_matrix(team_1_table, element = 5)[,-c(1:6)]
            res_store[team_1_row_from:team_1_row_to,107:122] <- get_data_matrix(team_1_table, element = 6)[,-c(1:6)]
          }
          #store team 2 results
          
          if(!is.null(nrow(get_data_matrix(team_2_table, element = 1)))){
            res_store[team_2_row_from:team_2_row_to,1:31]    <- get_data_matrix(team_2_table, element = 1)
            res_store[team_2_row_from:team_2_row_to,32:53]   <- get_data_matrix(team_2_table, element = 2)[,-c(1:6)]
            res_store[team_2_row_from:team_2_row_to,54:68]   <- get_data_matrix(team_2_table, element = 3)[,-c(1:6)]
            res_store[team_2_row_from:team_2_row_to,69:84]   <- get_data_matrix(team_2_table, element = 4)[,-c(1:6)]
            res_store[team_2_row_from:team_2_row_to,85:106]  <- get_data_matrix(team_2_table, element = 5)[,-c(1:6)]
            res_store[team_2_row_from:team_2_row_to,107:122] <- get_data_matrix(team_2_table, element = 6)[,-c(1:6)]
          }
          #store team names 
          # store match date 
          
          res_store[team_1_row_from:team_1_row_to,123] <- home_team
          res_store[team_2_row_from:team_2_row_to,123] <- away_team
          res_store[team_1_row_from:team_1_row_to,124] <- match_date
          res_store[team_2_row_from:team_2_row_to,124] <- match_date
          
          res_store[team_1_row_from:team_1_row_to,125] <- matches_urls[i]
          res_store[team_2_row_from:team_2_row_to,125] <- matches_urls[i]
          
          res_store[team_1_row_from:team_1_row_to,126] <- league_name
          res_store[team_2_row_from:team_2_row_to,126] <- league_name
        }
        
        pb$tick()
    }
    
    print(
      paste0(
        incomplete_counter, " of ", length(matches_urls), " matches had missing, incomplete, or non-standard data. ", 
        paste0(round(incomplete_counter/length(matches_urls), 4) * 100, "%"), " of matches in the update are discarded."
      )
    )
    
    if(nrow(na.omit(res_store)) == 0){print("No new data to add to the database ")}
    
    if(nrow(na.omit(res_store)) != 0){
      
     true_colnames_prefix <- c(
            paste0("summary_", 
              colnames(get_data_matrix(last_saved_team_2_table, element = 1))
              ),
            paste0("passing_", 
              colnames(get_data_matrix(last_saved_team_2_table, element = 2)[,-c(1:6)])
              ),
            paste0("pass_types_", 
              colnames(get_data_matrix(last_saved_team_2_table, element = 3)[,-c(1:6)])
              ),
            paste0("defensive_", 
              colnames(get_data_matrix(last_saved_team_2_table, element = 4)[,-c(1:6)])
              ),
            paste0("posession_", 
              colnames(get_data_matrix(last_saved_team_2_table, element = 5)[,-c(1:6)])
              ), 
            paste0("misc_stat_", 
              colnames(get_data_matrix(last_saved_team_2_table, element = 6)[,-c(1:6)])
              ),
            "Team", "Game_Date", "FB_ref_match_link", "League_name"
          )
     
    print("Finished looping to fill in results from FBref. Modifying final data frame")
    
    colnames(res_store) <- true_colnames_prefix
    
    # we have to make final column names from separate palces 
    # part is on the first row of the data 
    # part is column names right now 
    # part need to a prefix based on the column posiition 
    
    secondary_col_names <- res_store[min(which(res_store[,1] == "Player")),]
    
    #rename some potentially problematic columns
    for(i in c("#","Cmp%", "1/3", "Def 3rd", "Mid 3rd", "Att 3rd", "Tkl%", "Tkl+Int", "Succ%", "Tkld%", "Won%")){
      
      replace <- 
        case_when(
          i == "#" ~ "jersey_number", 
          i == "Cmp%" ~ "comp_prct", 
          i == "1/3" ~ "into_final_third", 
          i == "Def 3rd" ~ "def_thrid", 
          i == "Mid 3rd" ~ "mid_third", 
          i == "Att 3rd" ~ "att_third", 
          i == "Tkl%" ~ "tkl_prct", 
          i == "Tkl+Int" ~ "tkl_and_int", 
          i == "Succ%" ~ "succ_prct", 
          i == "Tkld%" ~ "tkld_prct", 
          i == "Won%" ~ "won_prct"
        )
      
      secondary_col_names[which(secondary_col_names == i)] <- replace
    }
    
    
    # now modify actual column names
    col_names_work <- 
      colnames(res_store)
    
    #now both set of column names need to be set to lowercase 
    col_names_work <- tolower(col_names_work)
    secondary_col_names <- tolower(secondary_col_names)
    
    # put date and team name to the beginning of the matrix 
    
    final_colnames <- paste(col_names_work, secondary_col_names)
    #if we have a space, or '-', replace with a "_"
    final_colnames <- gsub(" ", "_", final_colnames)
    final_colnames <- gsub("-", "_", final_colnames)
    final_colnames <- gsub("__", "_", final_colnames)
    
    final_colnames[123:126] <- c("team_name", "game_date", "fb_ref_match_link", "league_name") 
    # finally, make final prefixes for each set of columns, based on their origin
    
    
    colnames(res_store) <- final_colnames
    
    res_store <- as.data.frame(res_store)
    
    
    res_store <- res_store[-which(res_store[,1] == "Player"), ]
    res_store <- res_store[-which(is.na(res_store[,1])), ]
    
    res_store[,6:122] <- sapply(res_store[,6:122], FUN = as.numeric)
   
    
    # res_store$summary_age <- as.numeric(substr(res_store$summary_age, 1, 2)) 
     
    # 
    # res_store <- 
    #   res_store %>% 
    #   select(
    #     -summary_performance_ast, 
    #     -summary_performance_crdy,
    #     -summary_performance_crdr,
    #     -summary_performance_touches,
    #     -summary_performance_tkl,
    #     -summary_performance_int,
    #     -summary_performance_blocks,
    #     -summary_expected_xag,
    #     -summary_passes_cmp,
    #     -summary_passes_att,
    #     -summary_passes_comp_prct,
    #     -summary_passes_prgp,
    #     -summary_carries_carries,
    #     -summary_carries_prgc,
    #     -summary_take_ons_att,
    #     -summary_take_ons_succ,
    #     -pass_types_att, 
    #     -pass_types_outcomes_cmp, 
    #     -defensive_tkl_and_int, 
    #     -misc_stat_performance_crs,
    #     -misc_stat_performance_int,
    #     -misc_stat_performance_tklw
    #   )
    #     
    return(res_store)
    }
  }



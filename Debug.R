
DATA = dash_df
PLAYER = "Kevin De Bruyne"
SEASON = '2022/2023'
TEAM = 'Manchester City'
FEATURES_LIST = colnames(dash_df)[15:40]
COMP_LEAGUES = top_5_leagues
MINUTES_FILTER = 800
PLOT_VAR = "Expected Goals"


FEATURES_LIST = c(
    'summary_sca_sca',
    'passing_xag',
    'passing_xa',
    'passing_kp',
    'passing_ppa',
    'summary_sca_gca',
    'passing_ast',
    'posession_touches_att_third',
    'pass_types_pass_types_tb',
    'pass_types_pass_types_crs',
    'passing_prgp',
    'posession_carries_into_final_third',
    'pass_types_corner_kicks_str',
    'pass_types_outcomes_off'	,
    'posession_carries_prgc'
    )


REACTIVE_DATA <- 
  percentile_data_frame_one_player(
          DATA = dash_df,
          PLAYER = PLAYER,
          TEAM = TEAM, 
          SEASON =SEASON,
          MINUTES_FILTER = MINUTES_FILTER,
          COMP_LEAGUES = top_5_leagues
        )


similar_players_pca(
    DATA = dash_df, 
    SEASON = SEASON, 
    MINUTES_FILTER = MINUTES_FILTER,
    COMP_LEAGUES = top_5_leagues,
    FEATURES_LIST = REACTIVE_DATA %>% select(names) %>%  head(15) %>% unlist()
    ) -> PCA_RES
  
similar_pca_plot_df(
  PCA_RES = PCA_RES
) -> REACTIVE_PCA_DF
  
  similar_players_vector <- 
   
       similar_players_euclid_dist_data(
          
          DATA = dash_df, 
          REACTIVE_DATA = REACTIVE_DATA, 
          PLAYER = PLAYER,
          TEAM = TEAM, 
          SEASON = SEASON, 
          MINUTES_FILTER = MINUTES_FILTER,
          FEATURES_LIST = FEATURES_LIST, 
          COMP_LEAGUES = top_5_leagues
        ) %>% 
    
          arrange(scaled_dist) %>% 
          filter(summary_age >= input$similar_player_age_filter[1] & 
                   summary_age <= input$similar_player_age_filter[2]) %>% 
          head(input$target_sim_players) %>% 
          select(players) %>% unlist()
      
  
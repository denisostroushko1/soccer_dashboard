
DATA = dash_df
PLAYER = "Kevin De Bruyne"
SEASON = '2022/2023'
TEAM = 'Manchester City'
FEATURES_LIST = colnames(dash_df)[15:40]
COMP_LEAGUES = top_5_leagues
MINUTES_FILTER = 800


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

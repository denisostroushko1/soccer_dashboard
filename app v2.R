

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

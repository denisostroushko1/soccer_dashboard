
data_dict <- 
  read.csv(
    'FBref Advanced Soccer Data Disctionary.csv'
    )[,-1]

data_dict$stat_cat <- sub("_.*", "", data_dict$Data.Frame.Name)

data_dict[
  data_dict$Data.Frame.Name %in% 
    c("summary_performance_gls",
       "summary_performance_pk",
       "summary_performance_pkatt",
       "summary_performance_sh",
       "summary_performance_sot",
        "summary_expected_xg",
       "summary_expected_npxg",
       "summary_sca_sca",
       "summary_sca_gca" ), ]$stat_cat = "attacking"
   
data_dict[
  data_dict$stat_cat == "pass", ]$stat_cat = "pass type detail"

data_dict[
  data_dict$stat_cat == "misc", ]$stat_cat = "other statistics"

data_dict[
  data_dict$stat_cat == "posession", ]$stat_cat = "on the ball"

data_dict$stat_cat <- str_to_title(data_dict$stat_cat)

write.csv(data_dict, 'FBref Advanced Soccer Data Disctionary.csv')

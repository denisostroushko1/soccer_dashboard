
source('keys.R')
source("Master Packages.R")
source("Master Functions.R")

prefix <- './Collect Data/One Time Pull/'

source(paste0(prefix, 'Brazilian Serie A.R')) # 2023-03-20 19:12:36 to 21:00:24
source(paste0(prefix, 'Champions League.R')) # 2023-03-20 22:07:11 to 23:33:43
source(paste0(prefix, 'Conference League.R')) # for some reason we can't pull these data, couldn't find a bug reason yet 
source(paste0(prefix, 'Dutch Eredevise.R')) # 2023-03-21 10:50:42 to 12:45:31
source(paste0(prefix, 'English Premier League.R')) # 2023-03-22 00:53:57 to 03:19:20
source(paste0(prefix, 'Europa League.R')) # error with dates in the first year --> debug 
source(paste0(prefix, 'French Ligue 1.R')) # 2023-03-22 08:45:18 to 11:40:57
source(paste0(prefix, 'German Bundesliga.R')) # 2023-03-22 14:49:23 to 17:38:21
source(paste0(prefix, 'Italian Serie A.R')) # this took the whole evening. it kept crashing with 5XX HTML errors
source(paste0(prefix, 'Mexican Liga MX.R')) # 2023-03-23 17:07:46 to 20:19:23
source(paste0(prefix, 'MLS.R')) # 2023-03-23 20:58:42 to 00:53:01
source(paste0(prefix, 'Portugese Liga.R')) # 2023-03-24 01:48:33
source(paste0(prefix, 'Spanish La Liga.R')) # 

data_all <- 
  rbind(
    seria_bra_1_f, 
    champ_1_f, 
    ere_1_f, 
    eng_2_championship_1_f, 
    premier_league_f, 
#    el_1_f, 
    ligue_1_f, 
    bundes_1_f, 
    seria_ita_1_f, 
    liga_mx_1_f,
    mls_1_f, 
    potugese_1_f,
    la_liga_f
  )

data_all$league <- 
  with(data_all, 
       case_when(
         league == "Brazilian Seria A" ~ "Campeonato Brasileiro Série A ",
         league == "European Champions League" ~ "UEFA Champions League ",
         league == "Dutch Eredevise" ~ "Eredivisie ", 
         league == "English Championship" ~ "EFL Championship ", 
         league == "English Premier League" ~ "Premier League ", 
         league == "French Ligue 1" ~ "Ligue 1 ", 
         league == "German Bundesliga" ~ "Fußball-Bundesliga ", 
         league == "Italian Seria A" ~ "Serie A ", 
         league == "Mexican Liga MX" ~ "Liga MX ", 
         league == "MLS" ~ "Major League Soccer ", 
         league == "Portugese Liga" ~ "Primeira Liga ", 
         league == "Spanish La Liga" ~ "La Liga "
           
       ))


########################
# TESTING THE BEST WAY TO STORE THE DATA: 
###     GOAL: FASTEST LLOAD TIME OF THE DASHBOARD
###     TEST THE FOLLOWING COMBOS: 
#####       CSV and FEATEHR 
#####       ZIPED AND NON-ZIPPED 
#####       IN GOOGLE DRIVE AND IN AWS 


### Step 1: create csv file: 
write.csv(data_all, "dashboard_data.csv")
### Step 2: zip file
zip(zipfile = "dashboard_data_csv_zip", files = "dashboard_data.csv")

  ### TO GOOGLE DRIVE, zipped and original 
      # upload of csv file to google drive is impossible, file is too large or soemthing
  soccer_df <- 
    drive_put(
      media = "dashboard_data_csv_zip.zip", 
      path = drive_my_path,
      name = "dashboard_data_csv_zip.zip"
      )
  soccer_df %>% drive_share_anyone()
  
  # save links for the more efficient update of the data 
  data_all %>% 
    select(fb_ref_match_link) %>% 
    unique() -> fb_links
  
  write.csv(fb_links, "already_used_links.csv")
  zip(zipfile = "already_used_links_zip", files = "already_used_links.csv")
  
  links_df <- 
    drive_put(
      media = "already_used_links_zip.zip", 
      path = drive_my_path,
      name = "already_used_links_zip.zip"
      )
  links_df %>% drive_share_anyone()
  
  # save 
## clean up repository once files are uploaded
unlink('dashboard_data_csv_zip.zip')
unlink('dashboard_data.csv')
unlink('already_used_links_zip.zip')
unlink('already_used_links.csv')

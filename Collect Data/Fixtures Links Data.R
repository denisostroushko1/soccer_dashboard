
rm(list = ls())

source("keys.R")
source("Master Packages.R")
source("Master Functions.R")

# link to FB ref where all poissble competitions on their website are stored. 
mega_url = "https://fbref.com/en/comps/"

page <- read_html(mega_url)

# extract all the tables from the webpage
tables <- page %>%
  html_nodes("table")

# create an empty list to store the tables
table_list <- list()

# create an empty list to store the hyperlinks
link_list <- list()

### There are 9 'parts' to these tables, so we will look over them and create one ginat table
    
    table_f <- 
      (tables[1] %>%
      html_table(fill = TRUE))[[1]]
    
    if(!"Tier" %in% colnames(table_f)){table_f$`Tier` <- ""}
    if(!"Governing Body" %in% colnames(table_f)){table_f$`Governing Body` <- ""}
    if(!"Country" %in% colnames(table_f)){table_f$Country <- ""}
    
    tables[1] %>% html_nodes("a") %>% html_attr("href") -> hrefs
    hrefs <- paste0("https://fbref.com/", hrefs[which(str_detect(hrefs, pattern = ".*history*."))])
    
    table_f$link <- hrefs
    
    for(i in 2:length(tables)){
      table_i <- 
      
        (tables[i] %>%
        html_table(fill = TRUE))[[1]]
    
      if(!"Tier" %in% colnames(table_i)){table_i$`Tier` <- ""}
      if(!"Governing Body" %in% colnames(table_i)){table_i$`Governing Body` <- ""}
      if(!"Country" %in% colnames(table_i)){table_i$Country <- ""}
      
      tables[i] %>% html_nodes("a") %>% html_attr("href") -> hrefs
      hrefs <- paste0("https://fbref.com", hrefs[which(str_detect(hrefs, pattern = ".*history*."))])
    
      table_i$link <- hrefs
      
      table_f <- rbind(table_f, table_i)
    }
    
    # put together final table
    step_1 <- table_f

# We collected a lot more competitions than we need, so subset only those that we truly want
#### list of competitions: 
  check <- 
    c("EFL Championship",
    "Eredivisie" ,
    "La Liga",
    "Liga MX",
    "Ligue 1" ,
    "Major League Soccer",    
    "Premier League"   ,
    "Primeira Liga" ,
    "Serie A" ,
    "UEFA Champions League", 
    "UEFA Europa League"    ,       
    "UEFA Europa Conference League", 
    "Copa Libertadores de América", 
    "Campeonato Brasileiro Série A",  
    "Fußball-Bundesliga"
    )
  
  step_1$`Competition Name` %>% unique() %>% sort() %>% unlist()-> comps
  comps[comps %in% check] -> comps_res
  
  step_1 <- 
    step_1 %>% 
    filter(`Competition Name` %in% comps_res) %>% 
    arrange(`Competition Name`, `Last Season`) %>% 
    select( -`Governing Body` , -`First Season` , -`Last Season`, - Tier, -Country) %>% 
    unique()

  # We have 13 competitions now, moving on!! 
  
  
### Completed first roster of all competition and links to further information
### now loop over all competitions, find a link that contains 'Competition name-Stats'
  ### for those links, find all that contains 'Scores and Fixtures' 
  ### Store fixtures URLS for all years for the competitions that we are interested in
final_df <- 
  data.frame(
    comp = "", 
    end_year = NA, 
    fixtrures_urls = "", 
    comp_url = ""
  )

for( i in 1:nrow(step_1)){

  competition <- step_1[i,]$`Competition Name`
  hist_link <- step_1[i,]$link
  
  print(paste0("Collecting links for ", competition, ". This is iteration ", i, " of ", nrow(step_1)))
  
  page2 <- read_html(step_1$link[i])
  
  tables2 <- page2 %>%
        html_nodes("table")

  # we will 
  competition2 <- case_when(
    competition == 'Campeonato Brasileiro Série A' ~ 'Serie-A'                 ,
    competition == 'Copa Libertadores de América'  ~ 'Copa-Libertadores'       , 
    competition == 'EFL Championship'              ~ 'Championship'            ,
    competition == 'Eredivisie'                    ~ 'Eredivisie'              ,  
    competition == 'Fußball-Bundesliga'            ~ 'Bundesliga'              ,
    competition == 'La Liga'                       ~ 'La-Liga'                 ,  
    competition == 'Liga MX'                       ~ 'Liga-MX'                 ,
    competition == 'Ligue 1'                       ~ 'Ligue-1'                 , 
    competition == 'Major League Soccer'           ~ 'Major-League-Soccer'     , 
    competition == 'Premier League'                ~ 'Premier-League'          ,  
    competition == 'Primeira Liga'                 ~ 'Primeira-Liga'           ,             
    competition == 'Serie A'                       ~ 'Serie-A'                 ,
    competition == 'UEFA Champions League'         ~ 'Champions-League'        ,
    competition == 'UEFA Europa Conference League' ~ 'Europa-Conference-League',
    competition == 'UEFA Europa League'            ~ 'Europa-League'
  )
  
  tables2[1] %>% html_nodes("a") %>% html_attr("href") %>% sort() -> hrefs
  
  hrefs <- paste0("https://fbref.com", 
                hrefs[which(str_detect(hrefs, 
                                       pattern = 
                                         paste0('.*', paste0(competition2, "-Stats"),'*.')
                                       ))]) %>% unique()
  
  n_rows_expand <- length(hrefs)
  
  empty_years <- c()
  empty_links <- c()
  
  ### 
  for(j in 1:length(hrefs)){
 
    Sys.sleep(5)
    print(paste0(
      "Finding scores and fixtures for ", competition, ". Processing link ", j, " of ", length(hrefs)
    ))
    
    webpage <- read_html(hrefs[j])
    
    link <- html_nodes(webpage, "a:contains('Scores & Fixtures')")
    
    link_url <- html_attr(link, "href")
    
    link_url <- paste0("https://fbref.com", link_url[-which(str_detect(link_url, pattern = ".*matches*."))] %>% unique())
    empty_links <- c(empty_links, link_url)
    
    year <- as.numeric(gsub(".*\\b(20\\d{2})\\b.*", "\\1", link_url))
    
    year <- ifelse(
      is.na(year) ,
      as.integer(format(Sys.Date(), "%Y")), 
      year
    )
    
    empty_years <- c(empty_years, year)

  }
  
  final_df <- 
    rbind(final_df, 
          data.frame(
            comp = rep(competition, n_rows_expand), 
            comp_url = rep(hist_link, n_rows_expand), 
            end_year = empty_years, 
            fixtrures_urls = empty_links
          )
          )
}

final_df2 <- final_df

# extract year from fixtures URL
final_df2$true_yr <- 
  with(final_df2, 
       case_when(
         !is.na(as.numeric(gsub(".*\\b(20\\d{2})\\b.*", "\\1", final_df2$fixtrures_urls))) ~ 
           as.numeric(gsub(".*\\b(20\\d{2})\\b.*", "\\1", final_df2$fixtrures_urls)), 
         
         !is.na(as.numeric(gsub(".*\\b(19\\d{2})\\b.*", "\\1", final_df2$fixtrures_urls))) ~ 
           as.numeric(gsub(".*\\b(19\\d{2})\\b.*", "\\1", final_df2$fixtrures_urls)),
         
         !is.na(as.numeric(gsub(".*\\b(18\\d{2})\\b.*", "\\1", final_df2$fixtrures_urls))) ~ 
           as.numeric(gsub(".*\\b(18\\d{2})\\b.*", "\\1", final_df2$fixtrures_urls))
         
       ))

# replace NA for current season with current year 
final_df2$true_yr <- 
  with(final_df2, 
     ifelse(end_year == as.integer(format(Sys.Date(), "%Y")) & is.na(true_yr) ,
            as.integer(format(Sys.Date(), "%Y")), 
            true_yr
           )
     )

# now filter out all garbage and years we are not intereseted in
final_df2 <- final_df2 %>% filter(fixtrures_urls != "https://fbref.com")
final_df2 <- final_df2 %>% filter(comp != "")

# most current season for double year competitions has the same end year as the one before that. 
# fix that now 

final_df2$counter = 1

for(i in 2:nrow(final_df2)){
  
  if(
    final_df2$comp[i-1] == final_df2$comp[i] & 
    final_df2$end_year[i-1] == final_df2$end_year[i]
  ){
    final_df2$counter[i] =  final_df2$counter[i] + 1
    final_df2$end_year[i] = final_df2$end_year[i] + 1
    final_df2$true_yr[i] = final_df2$true_yr[i] + 1
  }
  
}

##################################################################

# need to fix an issue with the seasons of Brazilian league, not sure how, but the year of the league 
# switched from 2024 to 2025 sometime recently, causing a data quality issue 
# 
# update and fix added on 5/6/2024

##################################################################

final_df2 <- final_df2 %>% filter(end_year == true_yr)
final_df2 <- final_df2 %>% filter(end_year >= 2017)

final_df2 <- final_df2 %>% arrange(comp, end_year) %>% select(-true_yr, -counter)

colnames(final_df2) <- c('competition_name', 'season_end_year', 'fixtures_url', "comp_url")

final_df2$season <- 
  with(final_df2, 
       case_when(
         competition_name %in% c("Major League Soccer", "Campeonato Brasileiro Série A", "Copa Libertadores de América" ) ~ as.character(season_end_year), 
         TRUE ~ paste0(season_end_year - 1, "-", season_end_year)
       )
       )
  

final_df2$country = "" 
final_df2$gender = "M" 
final_df2$tier = ""

Fix_links_n <- nchar("https://fbref.com//")

final_df2$comp_url <- 
  with(final_df2, 
       case_when(
         substr(comp_url, 1, Fix_links_n) == "https://fbref.com//" ~ 
           paste0("https://fbref.com/", substr(comp_url, Fix_links_n+1, 300)), 
         T ~ comp_url
       ))


  write.csv(final_df2, "seasons_and_fixtures.csv")
  ### Step 2: zip file
  zip(zipfile = "fixtures_zip.zip", files = "seasons_and_fixtures.csv")
  
  Sys.setenv("AWS_ACCESS_KEY_ID" = access_key,
                   "AWS_SECRET_ACCESS_KEY" = secret_key,
                   "AWS_DEFAULT_REGION" = 'us-east-2')
  
  put_object(file = "fixtures_zip.zip", 
                 object = "fixtures_zip.zip",
                 bucket = bucket_name)
  
  ## clean up repository once files are uploaded
  unlink('fixtures_zip.zip')
  unlink('seasons_and_fixtures.csv')
  
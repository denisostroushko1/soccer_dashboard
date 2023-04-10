rm(list = ls())

make_fbref_hist_links <- 
  function(mega_url = "https://fbref.com/en/comps/"){
    
    page <- read_html(mega_url)
    
    # extract all the tables from the webpage
    tables <- page %>%
      html_nodes("table")
    
    # create an empty list to store the tables
    table_list <- list()
    
    # create an empty list to store the hyperlinks
    link_list <- list()
    
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
      hrefs <- paste0("https://fbref.com/", hrefs[which(str_detect(hrefs, pattern = ".*history*."))])
  
      table_i$link <- hrefs
      
      table_f <- rbind(table_f, table_i)
    }
    
    return(table_f)
  }

step_1 <- make_fbref_hist_links()

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
 # "UEFA Europa League"    ,       
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

### Completed first roster of all competition and links to further information

for( i in 1:nrow(step_1)){
  
  page2 <- read_html(step_1$link[i])
  
  tables2 <- page2 %>%
        html_nodes("table")
  
  competition <- step_1[i,]$`Competition Name`
  
  competition <- case_when(
    competition == "UEFA Champions League" ~ 'Champions-League', 
    competition == 'Campeonato Brasileiro Série A' ~ "Serie-A", 
    competition == "EFL Championship" ~ "Championship", 
    competition == "Fußball-Bundesliga" ~ "Bundesliga", 
    competition == "La Liga" ~ "La-Liga", 
    competition == "Liga MX" ~ "Liga-MX", 
    competition == "Major League Soccer" ~ "Major-League-Soccer", 
    competition == "Premier League" ~ "Premier-League", 
    competition == "Primeira Liga" ~ "Primeira-Liga", 
    competition == "Serie A" ~ "Serie-A", 
    T ~ competition
  )
  
  tables2[1] %>% html_table(fill = TRUE)
  tables2[1] %>% html_nodes("a") %>% html_attr("href") %>% sort() -> hrefs
  
  hrefs <- paste0("https://fbref.com/", 
                hrefs[which(str_detect(hrefs, 
                                       pattern = 
                                         paste0('.*', paste0(competition, "-Stats"),'*.')
                                       ))]) %>% unique()
  
  ### 
  for(j in 1:length(hrefs)){
    webpage <- read_html(hrefs[j])
    
    link <- html_nodes(webpage, "a:contains('Scores & Fixtures')")
    
    link_url <- html_attr(link, "href")
    
    link_url <- paste0("https://fbref.com", link_url[-which(str_detect(link_url, pattern = ".*matches*."))] %>% unique())
  }
  
}


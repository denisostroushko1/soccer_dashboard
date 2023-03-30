
https://www.transfermarkt.us/serie-a/marktwertaenderungen/wettbewerb/IT1/plus/1/sort/marktwert.desc/page/2

seria_a_23_link <- "https://www.transfermarkt.us/serie-a/marktwertaenderungen/wettbewerb/IT1/plus/1/sort/marktwert.desc"

web_page <- .load_page(seria_a_23_link) # get 'html' code for the webpage containing tables we need 
      
web_page_char <- as.character(web_page)
 
gregexpr('yw1', web_page_char)

# [1]  83592  83686  83732  83773  83926  84087  84230  84274  84407 127240

substr(web_page_char, gregexpr('yw1', web_page_char)[[1]][1] - 50, gregexpr('yw1', web_page_char)[[1]][1] + 50)
substr(web_page_char, gregexpr('yw1', web_page_char)[[1]][2] - 50, gregexpr('yw1', web_page_char)[[1]][2] + 50)

res1 <- 
  (web_page %>% rvest::html_nodes("#yw1") %>%# so every prefix of html node name has to be prefixed with '#'
   rvest::html_nodes("table") %>% 
   rvest::html_table())[[1]]
 

View(res1)

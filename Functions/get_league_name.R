
get_league_name <- function(url){
  page <- read_html(url)
  element <- html_nodes(page, ".scorebox_meta div:nth-child(2)")
  result <- html_text(element)
  return(strsplit(result, "[(]")[[1]][1])
}

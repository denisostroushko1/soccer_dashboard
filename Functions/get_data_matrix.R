# clean function to get matrix data from html request result
get_data_matrix <- 
  function(from, element){
    res <- tryCatch(
      as.matrix((from[element] %>% rvest::html_nodes("table") %>% rvest::html_table() )[[1]] )
      )
    
    #remove that last "Total" row 
    res <- res[-c(nrow(res)), ]
    return(res)
  }


remove_promotion_relegation_playoffs <- 
  function(urls_vec){
    urls_vec[!urls_vec %in% grep("Promotion", urls_vec, value = T)]
  }
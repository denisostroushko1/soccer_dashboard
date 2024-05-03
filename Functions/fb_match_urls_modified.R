
fb_match_urls_modified <- 
  function (country, gender, season_end_year, tier = "1st", non_dom_league_url = NA, 
    time_pause = 3, seasons_df) 
  {
    main_url <- "https://fbref.com"
    country_abbr <- country
    gender_M_F <- gender
    season_end_year_num <- season_end_year
    comp_tier <- tier
    cups_url <- non_dom_league_url
    
    if (is.na(cups_url)) {
      fixtures_url <- seasons_df %>% dplyr::filter(stringr::str_detect(.data[["competition_type"]], 
        "Leagues")) %>% dplyr::filter(country %in% country_abbr, 
        gender %in% gender_M_F, season_end_year %in% season_end_year_num, 
        tier %in% comp_tier, !is.na(fixtures_url)) %>% dplyr::arrange(season_end_year) %>% 
        dplyr::pull(fixtures_url) %>% unique()
    }
    else{
      fixtures_url <- seasons_df %>% dplyr::filter(.data[["comp_url"]] %in% 
        cups_url, gender %in% gender_M_F, season_end_year %in% 
        season_end_year_num, !is.na(fixtures_url)) %>% dplyr::arrange(season_end_year) %>% 
        dplyr::pull(fixtures_url) %>% unique()
    }
    time_wait <- time_pause
    get_each_seasons_df_urls <- function(fixture_url, time_pause = time_wait) {
      Sys.sleep(time_pause)
      match_report_urls <- load_page(fixture_url) %>% rvest::html_nodes("td.left~ .left+ .left a") %>% 
        rvest::html_attr("href") %>% paste0(main_url, .) %>% 
        unique()
      return(match_report_urls)
    }
    all_seasons_df_match_urls <- fixtures_url %>% purrr::map(get_each_seasons_df_urls) %>% 
      unlist()
    history_index <- grep("-History", all_seasons_df_match_urls)
    if (length(history_index) != 0) {
      all_seasons_df_match_urls <- all_seasons_df_match_urls[-history_index]
    }
    return(all_seasons_df_match_urls)
  }

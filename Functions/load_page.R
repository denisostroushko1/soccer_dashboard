
load_page <- function(page_url) {
    agent <- getOption("worldfootballR.agent", default = "RStudio Desktop (2022.7.1.554); R (4.1.1 x86_64-w64-mingw32 x86_64 mingw32)")
    ua <- httr::user_agent(agent)
    session <- rvest::session(url = page_url, ua)
    xml2::read_html(session)
}
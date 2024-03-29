
options(repos = c(CRAN = "https://cran.rstudio.com"))

library(devtools)
library(curl)
library(tidyverse)
library(plotly)
library(rlang)
library(data.table)
library(DT)

library(rvest) # two packages for easy interface with the scraped data 
library(xml2)

library(worldfootballR) # great package for scraping from fbref and transfermarkt 
library(ggsoccer)

library(aws.s3)

library(shinythemes)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyBS)
library(shinycssloaders)
library(flexdashboard)

library(feather)
library(rsconnect)
library(stringr)

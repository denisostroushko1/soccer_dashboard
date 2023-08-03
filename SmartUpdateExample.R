
library(shinyBS)
library(shiny)
library(leaflet)


data_df <- 
  data.frame(
    Year = c("20","19","18","17","16","15"), 
    Value = c(798173, 7981732, 91230, 12903, 6758, 910293)
  )


ui <- fluidPage(

  selectInput("input1", "Select input", c("choice1", "choice2")),
  bsTooltip(id = "input1", 
              title = "Here is some text with your instructions"), 
  
  checkboxGroupInput(
    inputId = "selected_dates",
    label = "Choose the year",
    choices = c("20","19","18","17","16","15"),
    selected = c("20","19","18","17","16","15")
  ),

  actionButton("go", "Update"),
  tableOutput("tab")

)

server <- function(input, output) {

  dates <- eventReactive(input$go, {
    input$selected_dates
  })

  output$tab <- renderTable({

    data_df[data_df$Year %in% dates(), ]
    
  })
}

shinyApp(ui, server)


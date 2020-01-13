library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  
  fluidRow(
    column(3, wellPanel(
    )
    ),
    column(3, wellPanel(
    )
    ),
    column(
      width = 6
    )
  ),
  
  fluidRow(
    tabsetPanel(
      type = "tabs",
      # summary tab
      tabPanel(
        "  Select Dates and Location",
        uiOutput("loc"),
        uiOutput("dt"),
        shiny::dataTableOutput("merged")
      )
    )
  )
)

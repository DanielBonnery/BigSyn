Comparetwotables<-function(){
  library(shiny)
  library(dplyr)
  
  location = c("Hobbiton", "Hobbiton", "Rivendell", "Rivendell", "Minas Tirith", "Minas Tirith") 
  last = c("A", "B", "C", "D", "E", "F") 
  locations = data.frame(location, last)
  
  datas<-data.frame(data(package=installed.packages()[,"Package"])$results[,c("Package","Item")])
  
  # Define UI for application that draws a histogram
  ui <- fluidPage(
    # Application title
    titlePanel("Select Package and data"),
    fluidRow(
      tabsetPanel(
        type = "tabs",
        # summary tab
        tabPanel(
          "  Select Package and Data",
          uiOutput("package1"),
          uiOutput("data1"),
          uiOutput("variables")
        )
      )
    )
  )


server <- function(input, output) {
  output$package1<-renderUI({
    selectInput("package1", label = h4("Choose package"),
                choices = datas$Package ,selected = 1
    )
  })
  
  rt<-reactive({
    #dataframe creation
    datas %>%
      filter(Package == input$package1)
  })
  
  output$data1<-renderUI({
    selectInput("data1", label = h4("Choose data"), 
                choices = as.vector(as.character(rt()$Item)),
                selected = 1,
                multiple = F)
  })
  
  output$variables <- shiny::renderDataTable({
    data.frame(names(get(data(list=input$data1,package=input$package1))))
  }
}



shinyApp(ui = ui, server = server)
}


Comparetwotables()

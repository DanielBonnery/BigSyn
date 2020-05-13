#' runCompare
#' @param data1 a dataframe
#' @param data2 a dataframe
#' @description Shiny App to visualize regression trees and compare synthetic vs non synthetic data
#' @export
#' @examples
#' package1<-NULL
#' package2<-NULL
#' runCompare()
runCompare3<-function(
  data1=NULL,
  data2=NULL){
  library(shiny)
  library(dplyr)
  library(gridExtra)
  library(ggplot2)
  library(shinythemes)
  library(party)
  library(partykit)
  variable1<-data.frame(variable1=names(data1))
  variable2<-data.frame(variable2=names(data2))
  
  #datas<-datas[is.element(datas$Package,c(package1,package2)),]
  ui <- navbarPage(theme = shinytheme("slate"),
                   title="Visualize and compare datasets tool",
                   tabPanel("One table", 
                            tabsetPanel(
                              tabPanel("R Summary",
                                       verbatimTextOutput("summary1_1")),
                              tabPanel("Missing Summary",
                                       dataTableOutput("missing.summary1")),
                              tabPanel("Missing plot",
                                       plotOutput("missingplot1_1")),
                              tabPanel("Advanced Missing plot",
                                       splitLayout(uiOutput("variable1_1"),
                                                   actionButton("doadvmissing1", "Create graphics")),
                                       plotOutput("advmissingplot1_1")),
                              tabPanel("Data",dataTableOutput("table1_1")))),
                   tabPanel("One table, one variable",
                            splitLayout(uiOutput("variable2_1")),
                            tabsetPanel(
                              tabPanel("Density plot",
                                       plotOutput("densityplot2_1")),
                              tabPanel("Qplot",
                                       plotOutput("qplot2_1")),
                              tabPanel("Contingency table",
                                       dataTableOutput("contingencytable2_1")))),
                   tabPanel("Two tables",  
                            splitLayout("",h2("Original data"),h2("Transposed or synthesized data"),"",cellWidths =c("5%","45%","45%","5%")),
                            tabsetPanel(
                              tabPanel("Summaries",       
                                       splitLayout(
                                         verbatimTextOutput("summary3_1"),
                                         verbatimTextOutput("summary3_2"))),
                              tabPanel("Missing",
                                       splitLayout(
                                         plotOutput("missingplot3_1"),
                                         plotOutput("missingplot3_2"))),
                              tabPanel("Advanced missing plot",
                                       splitLayout(uiOutput("variable3_1"),
                                                   actionButton("doadvmissing3_1", "Create graphics")),
                                       plotOutput("advancedmissingplot3_1")))),
                   tabPanel("Two tables, same variable", 
                            splitLayout("",h2("Original"),h2("Transformed"),"",cellWidths =c("10%","45%","45%")),
                            splitLayout(h4("X:"),
                                        uiOutput("variable4_1"),
                                        uiOutput("variable4_2"),
                                        cellWidths =c("10%","45%","45%")),
                            tabsetPanel(
                              tabPanel("Box plot",plotOutput("boxplot4_1")),
                              tabPanel("Jitter plots",plotOutput("jitterplot4_1")),
                              tabPanel("Violin plots",plotOutput("violinplot4_1")),
                              tabPanel("Density plots",plotOutput("densityplot4_1")),
                              tabPanel("Qplot",plotOutput("qplot4_1")),
                              tabPanel("Qplot 2",plotOutput("qplot4_2")),
                              tabPanel("Histogram 2",plotOutput("hist4_2")),
                              tabPanel("Contingency table",dataTableOutput("contingencytable4_1")))),
                   tabPanel("Two tables, two variables", 
                            splitLayout("",h2("Original"),h2("Transformed"),cellWidths =c("10%","45%","45%")),
                            splitLayout(h4("X:"),uiOutput("variable5_1_1"),uiOutput("variable5_2_1"),cellWidths =c("10%","45%","45%")),
                            splitLayout(h4("Y:"),uiOutput("variable5_1_2"),uiOutput("variable5_2_2"),cellWidths =c("10%","45%","45%")),
                            tabsetPanel(
                              tabPanel("Density plots",
                                       plotOutput("densityplot5_1"),
                                       plotOutput("densityplot5_2"),
                                       plotOutput("densityplot5_3")),
                              tabPanel("Qplot 2",plotOutput("qplot5_2")),
                              tabPanel("Boxplot",plotOutput("boxplot5_1")),
                              tabPanel("Jitter plot",plotOutput("jitter5_1")),
                              tabPanel("Violin plot",plotOutput("violin5_1")),
                              tabPanel("Histogram",plotOutput("bar5_2")),
                              tabPanel("Contingency table",dataTableOutput("contingencytable5_1")),
                              tabPanel("Data",dataTableOutput("table5_1")))))
  
  server <- function(input, output) {
    theme_dark2 = function() {
      theme_grey() %+replace%
        theme(
          # Specify axis options
          axis.line = element_blank(),  
          axis.text.x = element_text(color = "white",angle=90),  
          axis.text.y = element_text(color = "white"),  
          axis.ticks = element_line(color = "white"),  
          axis.title.x = element_text(color = "white"),  
          axis.title.y = element_text(color = "white"),
          # Specify legend options
          legend.background = element_rect(color = NA, fill = " gray10"),  
          legend.key = element_rect(color = "white",  fill = " gray10"),  
          legend.text = element_text(color = "white"),  
          legend.title = element_text(color = "white"),
          # Specify panel options
          panel.background = element_rect(fill = " gray10", color  =  NA),  
          panel.border = element_rect(fill = NA, color = "white"),  
          panel.grid.major = element_line(color = "grey35"),  
          panel.grid.minor = element_line(color = "grey20"),
          # Specify facetting options
          strip.background = element_rect(fill = "grey30", color = "grey10"),  
          strip.text.x = element_text(color = "white"),  
          strip.text.y = element_text(color = "white"),  
          # Specify plot options
          plot.background = element_rect(color = " gray10", fill = " gray10"),  
          plot.title = element_text(color = "white"),  
          plot.subtitle = element_text(color = "white"),  
          plot.caption = element_text( color = "white")
        )}
    #ggplot(data=data.frame(x=1:15,z=1,y=factor(1:3)),aes(x=x,y=z,color=y))+geom_point()
    th<-theme_dark2()
    theme_set(th)
    my_palette <- c('lightblue', 'red', 'white')
    names(my_palette)<-c('Original','Transformed',NA)
    assign("scale_colour_discrete", function(..., values = my_palette) scale_colour_manual(..., values = values), globalenv())
    assign("scale_fill_discrete", function(..., values = my_palette) scale_fill_manual(..., values = values), globalenv())
    #assign("scale_fill_ordinal", function(..., values = my_palette) scale_fill_manual(..., values = values), globalenv())
    #assign("scale_colour_ordinal", function(..., values = my_palette) scale_fill_manual(..., values = values), globalenv())
    colScale <- scale_colour_manual(name = "Origin",values = c('Original'='lightblue', 'Transformed'='red','white'))
    colScale2 <- scale_fill_manual(name = "Origin",values = c('Original'='lightblue', 'Transformed'='red','white'))
    #colScale3 <- scale__manual(name = "Origin",values = c('Original'='lightblue', 'Transformed'='red','white'))
    ######################################################
    #                      tab 1
    
    toto1<-eventReactive(input$do1,{
      table1_1<-get(data(list=input$data1_1,package=input$package1_1))
      nrow1_1<-nrow(table1_1)
      if(nrow1_1>1000){sel<-sample(nrow1_1,1000)}else{sel=TRUE}
      list(table1_1=table1_1[sel,],nrow1_1=nrow1_1)
    })
    
    output$summary1_1 <- renderPrint({summary(data1)})
    
    output$variable1_1 <- renderUI({
      selectInput("variable1_1", label = h4("Choose ordering variable"),
                  choices=variable1,
                  selected=1,
                  multiple=F,
                  selectize=FALSE)})
    
    output$missingplot1_1 <- renderPlot({
      StudyDataTools::ggplot_missing(data1,reordonne=TRUE)+th
    })   
    
    output$advmissingplot1_1 <- eventReactive(input$doadvmissing1,{
      StudyDataTools::ggplot_missing2(data1,reordonne=TRUE,keep=input$variable1_1)+th
    })   
    
    output$missing.summary1 <- DT::renderDataTable(StudyDataTools::missing.summary(data1))
    
    output$table1_1<- DT::renderDataTable(data1)
    
    
    ######################################################
    #                      tab 2
    
    variable2_1<-reactive({data.frame(variable2_1=names(data1))})
    output$variable2_1 <- renderUI({
      selectInput("variable2_1", 
                  label = "Variable",
                  choices=variable2,
                  selected=1,
                  multiple=F,
                  selectize=FALSE)
    })
    
    toto2<-reactive({
      variable2_1<-data1[,input$variable2_1]
      list(variable2_1=variable2_1)
    })
    output$densityplot2_1 <- renderPlot({
      ggplot2::ggplot(data.frame(x=toto2()$variable2_1),
                      aes(x = x,color="lightblue"))+
        xlab(input$variable2_1)+
        geom_density(show.legend = TRUE,color="lightblue")
    })   
    output$qplot2_1 <- renderPlot({
      ggplot2::ggplot(data.frame(X=toto2()$variable2_1,
                                 Origin=factor("Original")),
                      aes(X,Origin,colour=Origin))+geom_count()+colScale+theme(legend.position="none")+ylab("")})   
    output$contingencytable2_1 <- DT::renderDataTable(
      as.data.frame(table(toto2()$variable2_1,useNA="ifany")))
    output$table2_1<- shiny::renderTable(toto2()$table2_1)
    
    
    ######################################################
    #                      tab 3
    
    output$variable3_1 <- renderUI({
      selectInput("variable3_1", label = "Choose ordering variable",
                  choices=variable1,
                  selected=1,
                  multiple=F,
                  selectize=FALSE)})
    
    
    
    output$summary3_1 <- renderPrint({summary(data1)})
    output$summary3_2 <- renderPrint({summary(data2)})
    output$missingplot3_1 <- renderPlot({StudyDataTools::ggplot_missing(data1,reordonne=TRUE)+th})   
    output$missingplot3_2 <- renderPlot({StudyDataTools::ggplot_missing(data2,reordonne=TRUE)+th})
    
    advmissingplot3_1 <- eventReactive(input$doadvmissing3_1, {
      graph1<- StudyDataTools::ggplot_missing2(data1,reordonne=TRUE,keep=input$variable3_1)+th
      graph2<- StudyDataTools::ggplot_missing2(data2,reordonne=TRUE,keep=input$variable3_1)+th
      grid.arrange(graph1,graph2,nrow=1)})
    
    
    output$advancedmissingplot3_1 <- renderPlot({advmissingplot3_1()})   
    
    ######################################################
    #                      tab 4
    
    output$variable4_1 <- renderUI({
      selectInput("variable4_1", label = "Choose variable",
                  choices=variable1,
                  selected=1,
                  multiple=F,
                  selectize=FALSE)})
    
    
    output$variable4_2 <- renderUI({
      selectInput("variable4_2", label = "Choose variable",
                  choices=variable2,
                  selected=1,
                  multiple=F,
                  selectize=FALSE)})
    
    
    toto4<-reactive({
      variable4_1<-data1[[input$variable4_1]]
      variable4_2<-data2[[input$variable4_2]]
      table4<-rbind(data.frame(Origin="Original",X=variable4_1),
                    data.frame(Origin="Transformed",X=variable4_2))
      table4$Origin=factor(table4$Origin,levels=c("Original","Transformed"),ordered=TRUE)
      list(variable4_1=variable4_1,
           variable4_2=variable4_2,
           table4=table4)})
    
    
    output$variable4_1 <- renderUI({
      selectInput("variable4_1", label = "",
                  choices=variable1,
                  selected=1,
                  multiple=F,
                  selectize=FALSE)})
    
    
    output$variable4_2 <- renderUI({
      selectInput("variable4_2", label = "",
                  choices=variable2,
                  selected=if(is.element(input$variable4_1,variable2)){input$variable4_1}else{1},
                  multiple=F,
                  selectize=FALSE)})
    
    output$densityplot4_1 <- renderPlot({
      ggplot2::ggplot(toto4()$table4,aes(x = X,group=Origin,colour=Origin))+geom_density()+colScale2+colScale
    }) 
    
    output$boxplot4_1 <- renderPlot({
      ggplot(toto4()$table4, aes(x = Origin,y=X)) + geom_boxplot(aes(fill = Origin)) + theme(legend.position = "none")+colScale+colScale2})
    
    output$jitterplot4_1 <- renderPlot({
      ggplot(toto4()$table4, aes(x = Origin,y=X)) + geom_jitter(alpha = I(1/4), aes(color = Origin)) +theme(legend.position = "none")+colScale+colScale2})
    
    output$violinplot4_1 <- renderPlot({
      ggplot(toto4()$table4, aes(x = X)) + 
        stat_density(aes(ymax = ..density.., ymin = -..density..,fill = Origin, color = Origin), geom = "ribbon", position = "identity") +
        facet_grid(. ~Origin) +
        coord_flip()+colScale+colScale2})
    
    
    output$qplot4_1 <- renderPlot({
      ggplot2::qplot(Origin,X,color=Origin,data=toto4()$table4)+colScale+colScale2})   
    
    output$qplot4_2 <- renderPlot({
      ggplot2::ggplot(toto4()$table4,aes(Origin,X,fill=Origin,color=Origin,group=Origin))+geom_count()+colScale+colScale2})   
    
    output$hist4_2 <- renderPlot({
      if(is.factor(data1[["X"]])|is.character(data1[["X"]])){
        ggplot2::ggplot(toto4()$table4,aes(X,fill=Origin)) + geom_bar(position = "dodge")+colScale+colScale2
      }else{
        ggplot2::ggplot(toto4()$table4,aes(X,fill=Origin)) + geom_histogram(position = "dodge")+colScale+colScale2}
    })   
    
    
    
    
    output$contingencytable4_1 <- DT::renderDataTable(
      reshape2::dcast(reshape2::melt(as.data.frame(
        ftable(X~Origin,data=toto4()$table4,na.action=na.pass, exclude = NULL)/nrow(toto4()$table4)),
        value.name="X2",variable.name="Origin2"),X~Origin,value.var = "X2")
    )
    
    ######################################################
    #                      tab 5
    
    output$variable5_1_1 <- renderUI({
      selectInput("variable5_1_1", label=NULL,
                  choices=variable1,
                  selected=1,
                  multiple=FALSE,
                  selectize=FALSE)})
    
    output$variable5_1_2 <- renderUI({
      selectInput("variable5_1_2",  label=NULL,
                  choices=variable1,
                  selected=1,
                  multiple=FALSE,
                  selectize=FALSE)})
    
    
    
    output$variable5_2_1 <- renderUI({
      selectInput("variable5_2_1",  label=NULL,
                  choices=variable2,
                  selected=if(all(sapply(input$variable5_1_1,is.element,variable2))){input$variable5_1_1}else{1},
                  multiple=FALSE,
                  selectize=FALSE)})
    
    output$variable5_2_2 <- renderUI({
      selectInput("variable5_2_2",  label=NULL,
                  choices=variable2,
                  selected=if(all(sapply(input$variable5_1_2,is.element,variable2))){input$variable5_1_2}else{2},
                  multiple=FALSE,
                  selectize=FALSE)})
    
    
    toto5<-reactive({
      variable5_1<-data1[c(input$variable5_1_1,input$variable5_1_2)]
      variable5_2<-data2[c(input$variable5_2_1,input$variable5_2_2)]
      A<-cbind(data.frame(Origin="Transformed"),variable5_2)
      B<-cbind(data.frame(Origin="Original")   ,variable5_1)
      names(A)<-c("Origin","X","Y")
      names(B)<-c("Origin","X","Y")
      table5<-rbind(A,B)
      table5<-table5[order(table5$Origin),]
      table5$Origin<-factor(table5$Origin,levels=c("Original","Transformed"),ordered=TRUE)
      xlabel=paste(unique(c(names(variable5_1[1]),names(variable5_2[1]))),collapse=" - ")
      ylabel=paste(unique(c(names(variable5_1[2]),names(variable5_2[2]))),collapse=" - ")
      list(variable5_1=names(variable5_1),
           variable5_2=names(variable5_2),
           table5=table5,
           xlabel=xlabel,
           ylabel=ylabel)})
    
    
    
    variable5_1<-reactive({data.frame(variable5_1=names(data1))})
    variable5_2<-reactive({data.frame(variable5_2=names(data2))})
    
    
    output$densityplot5_1 <- renderPlot({
      Toto5<-toto5()
      plot1<-ggplot2::ggplot(Toto5$table5,aes(x = X,y=Y,group=Origin,color=Origin))+
        geom_point()+geom_density_2d()+
        xlab(Toto5$xlabel)+
        ylab(Toto5$ylabel)+colScale
      plot2<-plot1+facet_grid(.~Origin)+theme(legend.position = 'none')
      grid.arrange(plot2,plot1)
    }) 
    
    output$densityplot5_2 <- renderPlot({
      Toto5<-toto5()
      principal<-ggplot2::ggplot(Toto5$table5,aes(x = X,y=Y,group=Origin,color=Origin))+
        geom_point()+
        xlab(Toto5$xlabel)+
        ylab(Toto5$ylabel)+colScale
      #geom_density_2d()
      # marginal density of x - plot on top
      plot_top <- ggplot(Toto5$table5,aes(x = X,group=Origin,fill=Origin)) + geom_density(alpha = 0.5) +
        theme(legend.position = "none")+colScale
      # marginal density of y - plot on the right
      plot_right <- ggplot(Toto5$table5,aes(x =Y,group=Origin,fill=Origin)) + geom_density(alpha = 0.5) +
        coord_flip() +  theme(legend.position = "none")+colScale
      # arrange the plots together, with appropriate height and width for each row
      # and column
      legendd=ggplot()+theme(legend.position = c(1, 1), legend.justification = c(1,1))
      grid.arrange(plot_top, ,principal, plot_right, ncol = 2, nrow = 2, widths = c(4,1), heights = c(1, 4))
    }) 
    
    output$densityplot5_3 <- renderPlot({
      Toto5<-toto5()
      plot1<-ggplot2::ggplot(Toto5$table5,aes(x = X,y=Y,color=Origin,group=Origin))+
        stat_density_2d(geom = "polygon", aes(alpha = ..level.., fill = Origin))+
        xlab(Toto5$xlabel)+
        ylab(Toto5$ylabel)+colScale
      plot2<-plot1+facet_grid(.~Origin)+theme(legend.position="none")
      grid.arrange(plot2,plot1)
    }) 
    
    output$densityplot5_4 <- renderPlot({
      plot1<-ggplot2::ggplot(toto5()$table5,aes(x = X,y=Y,fill=Origin))+
        xlab(Toto5$xlabel)+
        ylab(Toto5$ylabel)+colScale+
        stat_density_2d(aes(alpha = ..density.., fill = Origin), geom = "tile", contour = FALSE)+geom_point(aes(color=Origin))
      plot2<-plot1+facet_grid(.~Origin)
      grid.arrange(plot1,plot2)}) 
    
    output$qplot5_1 <- renderPlot({
      Toto5<-toto5()
      plot1<-ggplot2::qplot(X,Y,color=Origin,data=toto5()$table5)+
        xlab(Toto5$xlabel)+
        ylab(Toto5$ylabel)+theme(legend.position="none")
      plot2<-plot1+facet_grid(.~Origin)
      grid.arrange(plot2,plot1)+colScale
    })   
    
    output$qplot5_2 <- renderPlot({
      Toto5<-toto5()
      ggplot2::ggplot(toto5()$table5,aes(X,Y,colour=Origin,group=Origin))+geom_count()+facet_grid(.~Origin)+coord_flip()+
        xlab(Toto5$xlabel)+
        ylab(Toto5$ylabel)+colScale
    })   
    
    output$bar5_2 <- renderPlot({
      Toto5<-toto5()
      if(is.factor(Toto5$table5[["Y"]])|is.character(Toto5$table5[["Y"]])){
        ggplot2::ggplot(Toto5$table5,aes(x=Y,fill=Origin))+geom_bar()+
          facet_grid(X~Origin,labeller = labeller(.rows = label_both, .cols = label_both))+theme(legend.position="none") +
          coord_flip()+colScale+colScale2
      }else{
        ggplot2::ggplot(Toto5$table5,aes(x=Y,fill=Origin))+geom_histogram()+
          facet_grid(X~Origin,labeller = labeller(.rows = label_both, .cols = label_both))+theme(legend.position="none")+
          coord_flip()+colScale+colScale2
      }
    })   
    
    
    
    output$boxplot5_1 <- renderPlot({
      ggplot(toto5()$table5, aes(x = X,y=Y)) + geom_boxplot(aes(fill = Origin)) + 
        theme(legend.position = "none")+facet_grid(.~Origin)+coord_flip()+colScale+colScale2
    })
    output$jitter5_1 <- renderPlot({
      ggplot(toto5()$table5, aes(x = X,y=Y)) + geom_jitter(alpha = I(1/4), aes(color = Origin)) +
        theme(legend.position = "none")+facet_grid(~Origin)+coord_flip()+colScale
    })
    output$violin5_1 <- renderPlot({
      ggplot(toto5()$table5, aes(x = Y)) + 
        stat_density(aes(ymax = ..density.., ymin = -..density..,fill = Origin, color = Origin), geom = "ribbon", position = "identity") +
        facet_grid(X~Origin) +
        coord_flip() + 
        theme(legend.position = "none")+colScale+colScale2
    })
    output$contingencytable5_1 <- DT::renderDataTable(
      reshape2::dcast(reshape2::melt(as.data.frame(
        ftable(X+Y~Origin,data=toto5()$table5,na.action=na.pass, exclude = NULL)/nrow(toto5()$table5)),
        value.name="X2",variable.name="Origin2"),X+Y~Origin,value.var = "X2")
    )
    output$table5_1<- DT::renderDataTable(toto5()$table5)
    
  }
  shinyApp(ui = ui, server = server)
}

#runCompare(c("BigSyn","base"),c("BigSyn","ggplot2","plyr"))

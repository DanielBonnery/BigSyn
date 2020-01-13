toto1<-function(){list(table1_1=tableA)}
toto2<-function(){
  cols<-c("cont1")
  variable2_1<-tableA[[cols]]
  list(variable2_1=variable2_1)}
toto3<-function(){
  cols<-names(tableA)
  table3_1<-tableA[,cols]
  table3_2<-TSTtableA[,cols];
  list(table3_1=table3_1,
       table3_2=table3_2,variable3_1="factor1")}
toto4<-function(){table4=rbind(cbind(Origin="Original",tableA[c("cont1")]),cbind(Origin="Transformed",TSTtableA[c("cont1")]));names(table4)<-c("Origin","X");list(table4=table4)}
toto5<-function(){
  cols<-c("factor1","cont4")
  A<-tableA[,cols]
  B<-TSTtableA[,cols];
  A$Origin="Original"
  B$Origin="Transformed"
  table5<-rbind(B,A);
  names(table5)<-c("X","Y","Origin");
  table5$Origin<-factor(table5$Origin,levels=c("Original","Transformed"),ordered=TRUE)
  list(table5=table5,xlabel=cols[1],ylabel=cols[2])
}

toto6<-function(){
  report6=get(data(list="ReportfitA",package="BigSyn"))
}

report6<-function(){
  toto6()[[14]][["splits"]][[2]]
}
#;names(report6())
#report6()$fit.plot


variablesplit6<-function(){
  Toto6<-toto6()
  dd<-plyr::ddply(data.frame(Variable=names(Toto6)),"Variable",function(d){data.frame(Split=1:length(Toto6[[d$Variable]]$splits))})
  dd$VariableSplit<-paste0(dd$Variable,"-",dd$Split)
  dd
}

#' Shiny App to visualize Data
#' @export
#' @examples
#' package1<-NULL
#' package2<-NULL
#' runCompare()

runCompare<-function(listofpackage1=installed.packages()[,"Package"],
                     listofpackage2=installed.packages()[,"Package"],
                     package1=if(is.element("BigSyn",listofpackage1)){"BigSyn"}else{listofpackage1[1]},
                     package2=if(is.element("BigSyn",listofpackage2)){"BigSyn"}else{listofpackage2[1]}){
  library(shiny)
  library(dplyr)
  library(gridExtra)
  library(ggplot2)
  library(shinythemes)
  library(party)
  library(partykit)
  datas<-unique(data.frame(data(package=unique(c(listofpackage2,listofpackage1)))$results[,c("Package","Item")],stringsAsFactors = FALSE))
  #datas<-datas[is.element(datas$Package,c(package1,package2)),]
  ui <- navbarPage(theme = shinytheme("slate"),
                   title="Visualize and compare datasets tool",
                   tabPanel("One table", 
                            splitLayout(uiOutput("package1_1"),
                                        uiOutput("data1_1"),
                                        actionButton("do1", "Load data",
                                                     style="background-color: #B22222; border-color: #B22222")),
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
                              tabPanel("Data",
                                       dataTableOutput("table1_1")))),
                   tabPanel("One table, one variable", 
                            splitLayout(uiOutput("package2_1"),
                                        uiOutput("data2_1"), 
                                        uiOutput("variable2_1")),
                            actionButton("do2", "Load data"),
                            tabsetPanel(
                              tabPanel("Density plot",
                                       plotOutput("densityplot2_1")),
                              tabPanel("Qplot",
                                       plotOutput("qplot2_1")),
                              tabPanel("Contingency table",
                                       dataTableOutput("contingencytable2_1")))),
                   tabPanel("Two tables",  
                            splitLayout("",h2("Original data"),h2("Transposed or synthesized data"),"",cellWidths =c("5%","45%","45%","5%")),
                            splitLayout("Package",uiOutput("package3_1"),uiOutput("package3_2"),"",cellWidths =c("5%","45%","45%","5%")),
                            splitLayout("Data",uiOutput("data3_1"),uiOutput("data3_2"),actionButton("do3", "Load data"),cellWidths =c("5%","45%","45%","5%")),
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
                                       splitLayout(uiOutput("variable3_1"),actionButton("doadvmissing3_1", "Create graphics")),
                                       plotOutput("advancedmissingplot3_1")))),
                   tabPanel("Two tables, same variable", 
                            splitLayout("",h2("Original"),h2("Transformed"),"",cellWidths =c("5%","45%","45%","5%")),
                            splitLayout(h4("Package:"),uiOutput("package4_1"),uiOutput("package4_2"),"",cellWidths =c("5%","45%","45%","5%")),
                            splitLayout(h4("Data:"),   uiOutput("data4_1"),uiOutput("data4_2"),"",cellWidths =c("5%","45%","45%","5%")),
                            splitLayout(h4("X:"),uiOutput("variable4_1"),uiOutput("variable4_2"),actionButton("do4", "Load data"),cellWidths =c("5%","45%","45%","5%")),
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
                            splitLayout("",h2("Original"),h2("Transformed"),"",cellWidths =c("8%","45%","45%","2%")),
                            splitLayout(h4("Package:"),uiOutput("package5_1"),uiOutput("package5_2"),"",cellWidths =c("8%","45%","45%","2%")),
                            splitLayout(h4("Data:"),   uiOutput("data5_1"),uiOutput("data5_2"),"",cellWidths =c("8%","45%","45%","2%")),
                            splitLayout(h4("X:"),uiOutput("variable5_1_1"),uiOutput("variable5_2_1"),"",cellWidths =c("8%","45%","45%","2%")),
                            splitLayout(h4("Y:"),uiOutput("variable5_1_2"),uiOutput("variable5_2_2"),actionButton("do5", "Load data"),cellWidths =c("8%","45%","45%","2%")),
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
                              tabPanel("Data",dataTableOutput("table5_1"))))
                   ,
                   tabPanel("Synthetisation report",
                            splitLayout("Select report:",
                                        uiOutput("package6"),
                                        uiOutput("data6"),
                                        actionButton("do6", "Load data"),
                                        uiOutput("variable6"),
                                        uiOutput("split6"),
                                        cellWidths =c("10%","15%","15%","10%","15%","15%")),
                             tabsetPanel(
                               tabPanel("Variable Summary"),
                               tabPanel("Split Summary",
                                        splitLayout("Condition","Number synthesized","Number used","Method","Method2"),
                                        splitLayout(textOutput("report6.condition"),
                                                    textOutput("report6.numbersynthesised"),
                                                    textOutput("report6.numberused"),
                                                    textOutput("report6.method"),
                                                    textOutput("report6.method2")),
                                        splitLayout("Calculus","Number of potential predictors","Model building time","Synthetisation time"),
                                        splitLayout(textOutput("report6.calculus"),
                                                    textOutput("report6.numpred"),
                                                    textOutput("report6.modelbuildingtime"),
                                                    #plotOutput("report6.fit.model.plot"),
                                                    textOutput("report6.synthetizationtime"))),
                               tabPanel("Split Details",plotOutput("report6.fit.model.plot"))
                               )
                            ))
  
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
    output$package1_1<-renderUI({
      selectInput("package1_1", 
                  label = "Package",
                  choices = unique(datas[,"Package"]),
                  selected = package1,
                  selectize=FALSE)})
    output$data1_1<-renderUI({
      selectInput("data1_1", 
                  label = "Dataset",
                  choices = datas[datas$Package==input$package1_1,"Item"],
                  selected = 1,
                  selectize=FALSE)})
    toto1<-eventReactive(input$do1,{
      table1_1<-get(data(list=input$data1_1,package=input$package1_1))
      nrow1_1<-nrow(table1_1)
      if(nrow1_1>1000){sel<-sample(nrow1_1,1000)}else{sel=TRUE}
      list(table1_1=table1_1[sel,],nrow1_1=nrow1_1)
    })
    
    variable1_1<-reactive({
      data.frame(variable1_1=names(get(data(list=input$data1_1,package=input$package1_1))))
    })
    
    output$summary1_1 <- renderPrint({
      summary(toto1()$table1_1)
    })
    
    output$variable1_1 <- renderUI({
      selectInput("variable1_1", label = h4("Choose ordering variable"),
                  choices=variable1_1()$variable1_1,
                  selected=1,
                  multiple=F,
                  selectize=FALSE)})
    
    output$missingplot1_1 <- renderPlot({
      ggplot_missing(toto1()$table1_1,reordonne=TRUE)+th
    })   
    
    output$advmissingplot1_1 <- eventReactive(input$doadvmissing1,{
      ggplot_missing2(toto1()$table1_1,reordonne=TRUE,keep=input$variable1_1)+th
    })   
    
    output$missing.summary1 <- shiny::renderDataTable(missing.summary(toto1()$table1_1))
    
    output$table1_1<- shiny::renderDataTable(toto1()$table1_1)
    
    
    ######################################################
    #                      tab 2
    output$package2_1<-renderUI({
      selectInput("package2_1", 
                  label = "Package",
                  choices = unique(datas[,"Package"]),
                  selected = package1,
                  selectize=FALSE)})
    output$data2_1<-renderUI({
      selectInput("data2_1", 
                  label = "Dataset",
                  choices = datas[datas$Package==input$package2_1,"Item"],
                  selected = if(try(is.element("tableA",datas[datas$Package==input$package2_1,"Item"]))){"tableA"}else{1},
                  selectize=FALSE)})
    
    
    variable2_1<-reactive({
      data.frame(variable2_1=names(get(data(list=input$data2_1,package=input$package2_1))))
    })
    output$variable2_1 <- renderUI({
      selectInput("variable2_1", 
                  label = "Variable",
                  choices=variable2_1()$variable2_1,
                  selected=1,
                  multiple=F,
                  selectize=FALSE)
    })
    
    toto2<-eventReactive(input$do2,{
      table2_1<-get(data(list=input$data2_1,package=input$package2_1))
      nrow2_1<-nrow(table2_1)
      if(nrow2_1>1000){sel<-sample(nrow2_1,1000)}else{sel=TRUE}
      variable2_1<-table2_1[sel,input$variable2_1]
      list(variable2_1=variable2_1)
    })
    output$densityplot2_1 <- renderPlot({
      ggplot2::ggplot(data.frame(x=toto2()$variable2_1),aes(x = x,color="lightblue"))+
        xlab(input$variable2_1)+
        geom_density(show.legend = TRUE,color="lightblue")
    })   
    output$qplot2_1 <- renderPlot({
      ggplot2::ggplot(data.frame(X=toto2()$variable2_1,Origin=factor("Original")),aes(X,Origin,colour=Origin))+geom_count()+colScale+theme(legend.position="none")+ylab("")})   
    output$contingencytable2_1 <- renderDataTable(
      as.data.frame(table(toto2()$variable2_1,useNA="ifany")))
    output$table2_1<- shiny::renderTable(toto2()$table2_1)
    
    
    ######################################################
    #                      tab 3
    
    output$package3_1<-renderUI({
      selectInput("package3_1", 
                  label = NULL,
                  choices = listofpackage1,
                  selected = package1,
                  selectize=FALSE)})
    output$package3_2<-renderUI({
      selectInput("package3_2", 
                  label = NULL,
                  choices = listofpackage2,
                  selected = package2,
                  selectize=FALSE)})
    output$data3_1<-renderUI({
      selectInput("data3_1", 
                  label = NULL,
                  choices = datas[datas$Package==input$package3_1,"Item"],
                  selected = if(try(is.element("tableA",datas[datas$Package==input$package3_1,"Item"]))){"tableA"}else{1},
                  selectize=FALSE)})
    
    output$data3_2<-renderUI({
      selectInput("data3_2",
                  label=NULL,
                  choices = datas[datas$Package==input$package3_2,"Item"] ,
                  selected = if(try(is.element(input$data3_1,datas[datas$Package==input$package3_2,"Item"]))){input$data3_1}else{1},
                  selectize=FALSE)})
    
    variable3_1<-reactive({
      data.frame(variable3_1=names(get(data(list=input$data3_1,package=input$package3_1))))
    })
    
    
    
    output$variable3_1 <- renderUI({
      selectInput("variable3_1", label = "Choose ordering variable",
                  choices=variable3_1()$variable3_1,
                  selected=1,
                  multiple=F,
                  selectize=FALSE)})
    
    toto3<-eventReactive(input$do3,{
      table3_1<-get(data(list=input$data3_1,package=package1))
      table3_2<-get(data(list=input$data3_2,package=package2))
      summary3_1<-summary(table3_1)
      summary3_2<-summary(table3_2)
      nrow3_1<-nrow(table3_1)
      nrow3_2<-nrow(table3_2)
      sel3_1<-if(nrow3_1>1000){sample(nrow3_1,1000)}else{TRUE}
      sel3_2<-if(nrow3_2>1000){sample(nrow3_2,1000)}else{TRUE}
      list(table3_1=table3_1[sel3_1,],
           table3_2=table3_2[sel3_2,],
           summary3_1=summary3_1,
           summary3_2=summary3_2,
           variable3_1=input$variable3_1)
    })
    
    
    
    
    output$summary3_1 <- renderPrint({
      toto3()$summary3_1
    })
    
    output$summary3_2 <- renderPrint({
      toto3()$summary3_2
    })
    
    
    output$missingplot3_1 <- renderPlot({
      ggplot_missing(toto3()$table3_1,reordonne=TRUE)+th
    })   
    
    output$missingplot3_2 <- renderPlot({
      ggplot_missing(toto3()$table3_2,reordonne=TRUE)+th
    })
    
    advmissingplot3_1 <- eventReactive(input$doadvmissing3_1, {
      graph1<- ggplot_missing2(toto3()$table3_1,reordonne=TRUE,keep=toto3()$variable3_1)+th
      graph2<- ggplot_missing2(toto3()$table3_2,reordonne=TRUE,keep=toto3()$variable3_1)+th
      grid.arrange(graph1,graph2,nrow=1)})
    
    
    output$advancedmissingplot3_1 <- renderPlot({advmissingplot3_1()})   
    
    ######################################################
    #                      tab 4
    
    output$package4_1<-renderUI({
      selectInput("package4_1", 
                  label = "",
                  choices = listofpackage1,
                  selected = package1,
                  selectize=FALSE)})
    output$package4_2<-renderUI({
      selectInput("package4_2", 
                  label = NULL,
                  choices = listofpackage2,
                  selected = package2,
                  selectize=FALSE)})
    output$data4_1<-renderUI({
      selectInput("data4_1", 
                  label = "",
                  choices = datas[datas$Package==input$package4_1,"Item"],
                  selected = if(try(is.element("tableA",datas[datas$Package==input$package4_1,"Item"]))){"tableA"}else{1},
                  selectize=FALSE)})
    output$data4_2<-renderUI({
      selectInput("data4_2", 
                  label = "",
                  choices = datas[datas$Package==input$package4_2,"Item"] ,
                  selected = if(try(is.element(input$data4_1,datas[datas$Package==input$package4_2,"Item"]))){input$data4_1}else{1},
                  selectize=FALSE)})
    toto4<-eventReactive(input$do4,{
      variable4_1<-get(data(list=input$data4_1,package=input$package4_1))[[input$variable4_1]]
      variable4_2<-get(data(list=input$data4_2,package=input$package4_2))[[input$variable4_2]]
      if(length(variable4_1)>1000){variable4_1<-variable4_1[sample(length(variable4_1),1000)]}
      if(length(variable4_2)>1000){variable4_2<-variable4_2[sample(length(variable4_2),1000)]}
      
      table4<-rbind(data.frame(Origin="Original",X=variable4_1),
                    data.frame(Origin="Transformed",X=variable4_2))
      table4$Origin=factor(table4$Origin,levels=c("Original","Transformed"),ordered=TRUE)
      list(variable4_1=variable4_1,
           variable4_2=variable4_2,
           table4=table4)})
    
    variable4_1<-reactive({
      data.frame(variable4_1=names(get(data(list=input$data4_1,package=input$package4_1))))})
    
    variable4_2<-reactive({
      data.frame(variable4_2=names(get(data(list=input$data4_2,package=input$package4_2))))})
    
    output$variable4_1 <- renderUI({
      selectInput("variable4_1", label = "",
                  choices=variable4_1()$variable4_1,
                  selected=1,
                  multiple=F,
                  selectize=FALSE)})
    
    
    output$variable4_2 <- renderUI({
      selectInput("variable4_2", label = "",
                  choices=variable4_2()$variable4_2,
                  selected=if(is.element(input$variable4_1,variable4_2()$variable4_2)){input$variable4_1}else{1},
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
      if(is.factor(toto4()$table4[["X"]])|is.character(toto4()$table4[["X"]])){
        ggplot2::ggplot(toto4()$table4,aes(X,fill=Origin)) + geom_bar(position = "dodge")+colScale+colScale2
      }else{
        ggplot2::ggplot(toto4()$table4,aes(X,fill=Origin)) + geom_histogram(position = "dodge")+colScale+colScale2}
    })   
    
    
    
    
    output$contingencytable4_1 <- renderDataTable(
      reshape2::dcast(reshape2::melt(as.data.frame(
        ftable(X~Origin,data=toto4()$table4,na.action=na.pass, exclude = NULL)/nrow(toto4()$table4)),
        value.name="X2",variable.name="Origin2"),X~Origin,value.var = "X2")
    )
    
    ######################################################
    #                      tab 5
    output$package5_1<-renderUI({
      selectInput("package5_1", label=NULL,
                  choices = listofpackage1,
                  selected = package1,
                  selectize=FALSE)})
    output$package5_2<-renderUI({
      selectInput("package5_2", label=NULL,
                  choices = listofpackage2,
                  selected = package2,
                  selectize=FALSE)})
    output$data5_1<-renderUI({
      selectInput("data5_1", label=NULL,
                  choices = datas[datas$Package==input$package5_1,"Item"],
                  selected = if(try(is.element("tableA",datas[datas$Package==input$package5_1,"Item"]))){"tableA"}else{1},
                  selectize=FALSE)})
    output$data5_2<-renderUI({
      selectInput("data5_2",label=NULL,
                  choices = datas[datas$Package==input$package5_2,"Item"] ,
                  selected = if(try(is.element(input$data5_1,datas[datas$Package==input$package5_2,"Item"]))){input$data5_1}else{1},
                  selectize=FALSE)})
    
    output$variable5_1_1 <- renderUI({
      selectInput("variable5_1_1", label=NULL,
                  choices=variable5_1()$variable5_1,
                  selected=1,
                  multiple=FALSE,
                  selectize=FALSE)})
    
    output$variable5_1_2 <- renderUI({
      selectInput("variable5_1_2",  label=NULL,
                  choices=variable5_1()$variable5_1,
                  selected=1,
                  multiple=FALSE,
                  selectize=FALSE)})
    
    
    
    output$variable5_2_1 <- renderUI({
      selectInput("variable5_2_1",  label=NULL,
                  choices=variable5_2()$variable5_2,
                  selected=if(all(sapply(input$variable5_1_1,is.element,variable5_2()$variable5_2))){input$variable5_1_1}else{1},
                  multiple=FALSE,
                  selectize=FALSE)})
    
    output$variable5_2_2 <- renderUI({
      selectInput("variable5_2_2",  label=NULL,
                  choices=variable5_2()$variable5_2,
                  selected=if(all(sapply(input$variable5_1_2,is.element,variable5_2()$variable5_2))){input$variable5_1_2}else{2},
                  multiple=FALSE,
                  selectize=FALSE)})
    
    
    toto5<-eventReactive(input$do5,{
      variable5_1<-get(data(list=input$data5_1,package=input$package5_1))[c(input$variable5_1_1,input$variable5_1_2)]
      variable5_2<-get(data(list=input$data5_2,package=input$package5_2))[c(input$variable5_2_1,input$variable5_2_2)]
      if(nrow(variable5_1)>1000){variable5_1<-variable5_1[sample(nrow(variable5_1),1000),]}
      if(nrow(variable5_2)>1000){variable5_2<-variable5_2[sample(nrow(variable5_2),1000),]}
      
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
    
    
    
    variable5_1<-reactive({
      data.frame(variable5_1=names(get(data(list=input$data5_1,package=input$package5_1))))})
    
    variable5_2<-reactive({
      data.frame(variable5_2=names(get(data(list=input$data5_2,package=input$package5_2))))})
    
    
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
    output$contingencytable5_1 <- renderDataTable(
      reshape2::dcast(reshape2::melt(as.data.frame(
        ftable(X+Y~Origin,data=toto5()$table5,na.action=na.pass, exclude = NULL)/nrow(toto5()$table5)),
        value.name="X2",variable.name="Origin2"),X+Y~Origin,value.var = "X2")
    )
    output$table5_1<- shiny::renderDataTable(toto5()$table5)
    
    
    ####6
    output$package6<-renderUI({
      selectInput("package6", 
                  label = "Package",
                  choices = unique(datas[,"Package"]),
                  selected ="BigSyn",
                  selectize=FALSE)})
    output$data6<-renderUI({
      selectInput("data6", 
                  label="Report",
                  choices = datas[datas$Package==input$package6,"Item"],
                  selected = if(try(is.element("ReportFitA",datas[datas$Package==input$package6,"Item"]))){"ReportFitA"}else{1},
                  selectize=FALSE)})
    output$variable6<- renderUI({
      selectInput("variable6",  
                  label="Variable",
                  choices=variable6(),
                  selected="All",
                  multiple=FALSE,
                  selectize=FALSE)})
    
    output$split6<- renderUI({
      selectInput("split6",  
                  label="Split",
                  choices =if(!is.element(selectedvariable6(),variablesplit6()$Variable)){variablesplit6()$VariableSplit}else{variablesplit6()$VariableSplit[variablesplit6()$Variable==selectedvariable6()]},
                  selected=if(selectedvariable6()=="ALL"){1}else{paste0(selectedvariable6(),"-",1)},
                  multiple=FALSE,
                  selectize=FALSE)})
    
    
    
    toto6<-eventReactive(input$do6,{
      get(data(list=input$data6,package=input$package6))
    })
    
    variablesplit6<-reactive({
      Toto6<-toto6()
      dd<-plyr::ddply(data.frame(Variable=names(Toto6)),"Variable",function(d){data.frame(Split=1:length(Toto6[[d$Variable]]$splits))})
      dd$VariableSplit<-paste0(dd$Variable,"-",dd$Split)
      dd
    })
    
    variable6<-reactive({c("All",names(toto6()))})
    
    report6.1<-reactive({
      plyr::ldply(toto6()[[input$variable6]]$splits,function(x){
        as.data.frame(x[c("condition","method","method2","problem","calculus","modelbuildingtime","synthetizationtime","numbersynthesised","numberused")])
      })
    })
    selectedvariable6<-reactive({input$variable6})
    selectedvariablesplit6<-reactive({input$split6})
    
    report6<-reactive({
      VariableSplit<-variablesplit6()[variablesplit6()$VariableSplit==input$split6,]
      Toto<-toto6()[[VariableSplit$Variable]][["splits"]][[strtoi(VariableSplit$Split)]]
      if(is.null(Toto$fit.plot)){Toto$fit.plot<-NULL}
      Toto
      })
    
    output$report6<-renderPrint({repoort6()})  
    
    output$report6.condition<-renderPrint({report6()$condition})    
    output$report6.numpred   <-renderPrint({length(report6()$predictors)})    
    output$report6.predictors   <-shiny::renderDataTable(report6()$predictors)    
    output$report6.method   <-renderPrint({report6()$method})            
    output$report6.problem   <-renderPrint({report6()$problem})        
    output$report6.method2   <-renderPrint({report6()$method2})    
    #  output$report6.fit.model   <-renderPrint({report6()$fit.model})
    output$report6.fit.model.plot   <-renderPlot({report6()$fit.plot})#report6()$fit.plot
    output$report6.calculus <-renderPrint({report6()$calculus})    
    output$report6.modelbuildingtime <-renderPrint({report6()$modelbuildingtime})    
    output$report6.synthetizationtime <-renderPrint({report6()$synthetizationtime})    
    output$report6.numbersynthesised <-renderPrint({report6()$numbersynthesised})    
    output$report6.numberused <-renderPrint({report6()$numberused})    
    
  }
  shinyApp(ui = ui, server = server)
}

#runCompare(c("BigSyn","base"),c("BigSyn","ggplot2","plyr"))

library(base)
library(curl)
library(downloader)
library(dplyr)
library(lubridate)
library(parallel)
library(HelpersMG)
library(tidyquant)
library(data.table)

adjusted_pvt_returns <- readRDS(file= 'adjusted_pvt_returns.RData')
adjusted <- readRDS(file= 'adjusted.RData')
outliers <- unlist(fread(file="outliers.csv"))
totalReturns <- fread(file="totalReturns.csv")

adjusted_pvt <- readRDS(file="adjusted_pvt.RData")

#max_plots <- length(top_pct)

ui <- fluidPage(pageWithSidebar(
  
  headerPanel("EOD Top/Bottom Stocks Dividend/Split Adj TCR w BBands/RSI, n=400"),
  
  sidebarPanel(
    textOutput("text0"),
    sliderInput("pct", "Percent", value=.025, min=.005, step=.0025, max=.1),
    uiOutput("slider"),
    textOutput("text1"),
    textOutput("text2"),
    textOutput("text3"),
    textOutput("text4"),
    textOutput("text5"),
    textOutput("text6"),
    textOutput("text7"),
    textOutput("text8"),
    plotOutput("hist")
    
  ),
  
  mainPanel(
    # This is the dynamic UI for the plots
    
    fluidRow(
      splitLayout(cellWidths = c("50%", "50%"), plotOutput("cumulativePlot1",height = 400, width = 600), plotOutput("cumulativePlot2",height = 400, width = 600))
    ),
    
    fluidRow(
      splitLayout(cellWidths = c("50%", "50%"), uiOutput("plots1"), uiOutput("plots2"))
    )
    
  )
))

server <- function(input, output, session) {
  
  sorted <- data.frame(t(t(totalReturns)[order(-t(totalReturns)), , drop = FALSE]))
  totalReturns_noIndex <- dplyr::select(sorted, -c("X.SP500TR"))
  
  top_pct_1 <- reactive({
    unlist(quantile(totalReturns_noIndex,probs=c((1-input$pct))))
  })
  
  top_pct_names <- reactive({
    #colnames(totalReturns_noIndex)[c(totalReturns_noIndex[,which(totalReturns_noIndex>=top_pct_1())])]
    colnames(totalReturns_noIndex[,which(totalReturns_noIndex>=top_pct_1())])
  })
  
  #have to unlist it
  bottom_pct_1 <- reactive({
    unlist(quantile(totalReturns_noIndex,probs=c(input$pct)))
  })
  
  bottom_pct_names <- reactive({
    #colnames(totalReturns_noIndex)[c(totalReturns_noIndex[,which(totalReturns_noIndex<=bottom_pct_1())])]
    colnames(totalReturns_noIndex[,which(totalReturns_noIndex<=bottom_pct_1())])
  })
  
  numberStocks <- reactive({
    length(totalReturns_noIndex[,which(totalReturns_noIndex<=bottom_pct_1())])
  })
  
  output$text0 <- renderText({
    
    format(Sys.Date())
    
  })
  
  
  output$text1 <- renderText({
    
    paste("Median:",median(unlist(totalReturns_noIndex)))
    
  })
  
  output$text2 <- renderText({
    
    paste(c("Summary:",round(summary(unlist(totalReturns_noIndex)),2)))})
  
  
  output$text3 <- renderText({
    
    "Outlier's"
    
  })
  
  output$text4 <- renderText({
    
    names(outliers)
    
  })
  
  output$text5 <- renderText({
    
    outliers
    
  })
  
  output$text6 <- renderText({
    
    "Top/Bottom Thresholds"
    
  })
  
  output$text7 <- renderText({
    
    unlist(bottom_pct_1())
    
  })
  
  output$text8 <- renderText({
    
    unlist(top_pct_1())
    
  })
  
  
  output$hist <- renderPlot({
    
    hist(unlist(totalReturns_noIndex),main="Histogram of Total Cumulative Returns")
    
  })
  
  output$slider <- ({
    if(is.null(numberStocks))
    {return(NULL)}
    renderUI({
      sliderInput("n", "Number of plots", value=4, min=1, max=numberStocks(), round=TRUE)
    })
  })
  
  #https://deanattali.com/blog/building-shiny-apps-tutorial/#11-using-uioutput-to-create-ui-elements-dynamically
  observe({
    updateNumericInput(session, "n", max = numberStocks())
  })
  
  output$cumulativePlot1 <- renderPlot({
    #if (is.null(filtered())) 
    {
      #return()
    }
    chart.CumReturns(adjusted_pvt_returns[,c(top_pct_names(),"X.SP500TR")], legend.loc = 'topleft',ylim=c(-1,10))
  })
  
  output$cumulativePlot2 <- renderPlot({
    #if (is.null(filtered())) 
    {
      #return()
    }
    chart.CumReturns(adjusted_pvt_returns[,c(bottom_pct_names(),"X.SP500TR")], legend.loc = 'topleft',ylim=c(-1,10))
  })
  
  # Insert the right number of plot output objects into the web page
  output$plots1 <- renderUI({
    if (is.null(input$n)) {
      return(NULL)
    }    
    plot_output_list <- lapply(1:input$n, function(i) {
      plotname1 <- paste("plot1", i, sep="")
      
      plotOutput(plotname1, height = 400, width = 600)
    })
    
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, plot_output_list)
  })
  
  output$plots2 <- renderUI({
    if (is.null(input$n)) {
      return(NULL)
    }
    plot_output_list <- lapply(1:input$n, function(i) {
      plotname2 <- paste("plot2", i, sep="")
      
      plotOutput(plotname2, height = 400, width = 600)
    })
    
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, plot_output_list)
  })
  
  # Call renderPlot for each one. Plots are only actually generated when they
  # are visible on the web page.
  #lapply(adjusted,function (x) {
  
  #plotSet_test <- reactive({adjusted[c(top_pct_names())]})
  
  #observe({print(plotSet_test())})
  
  #plotSet1 <- adjusted[c(top_pct)]
  plotSet1 <- reactive({adjusted[c(top_pct_names())]})
  #top_pct[1]
  observe({
    if(is.null(numberStocks()))
    {return(NULL)}
    max_plots=numberStocks()
    #print(numberStocks())
    for (i in 1:max_plots) {
      # Need local so that each item gets its own number. Without it, the value
      # of i in the renderPlot() will be the same across all instances, because
      # of when the expression is evaluated.
      
      local({
        my_i <- i
        plotname1 <- paste("plot1", my_i, sep="")
        
        chartName <- top_pct_names()[[i]]
        
        #x <- adjusted[[i]][,]
        
        x <- plotSet1()[[i]][,c("Date","Open","High","Low","Close","Volume","Adjusted")]
        
        output[[plotname1]] <- renderPlot({
          #print(names(plotSet[i]))
          #x <- plotSet1[[1]][,c("Date","Open","High","Low","Close","Volume","Adjusted")]
          
          temp <- as.data.table(x)
          rownames(temp) <- temp$Date
          temp <- as.xts(temp)
          
          quantmod::chartSeries(temp, type="candlestick",subset=NULL,up.col = "black", dn.col = "red", name=chartName, theme = "white",TA=c(addRSI(n=14,maType="EMA"),addMACD(fast=12,slow=26,signal=9,type='EMA',histogram = TRUE),addBBands(n = 20, sd = 2, maType = "SMA", draw = 'bands', on = -1)))
          
        })
        
      })
    }
  })
  plotSet2 <- reactive({adjusted[c(bottom_pct_names())]})
  #plotSet2 <- adjusted[c(bottom_pct)]
  observe({
    if(is.null(numberStocks()))
    {return(NULL)}
    max_plots=numberStocks()
    for (i in 1:max_plots) {
      # Need local so that each item gets its own number. Without it, the value
      # of i in the renderPlot() will be the same across all instances, because
      # of when the expression is evaluated.
      local({
        my_i <- i
        plotname2 <- paste("plot2", my_i, sep="")
        
        chartName <- bottom_pct_names()[[i]]
        
        #x <- adjusted[[i]][,]
        x <- plotSet2()[[i]][,c("Date","Open","High","Low","Close","Volume","Adjusted")]
        
        output[[plotname2]] <- renderPlot({
          
          temp <- as.data.table(x)
          rownames(temp) <- temp$Date
          temp <- as.xts(temp)
          
          quantmod::chartSeries(temp, type="candlestick",subset=NULL,up.col = "black", dn.col = "red", name= chartName, theme = "white",TA=c(addRSI(n=14,maType="EMA"),addMACD(fast=12,slow=26,signal=9,type='EMA',histogram = TRUE),addBBands(n = 20, sd = 2, maType = "SMA", draw = 'bands', on = -1), addEMA(n = 5, wilder = FALSE, ratio=NULL, on = 1, with.col = Cl, overlay = TRUE, col = "blue"),addEMA(n = 20, wilder = FALSE, ratio=NULL, on = 1, with.col = Cl, overlay = TRUE, col = "green"),addEMA(n = 50, on = 1, with.col = Cl, overlay = TRUE, col = "brown"),addEMA(n = 200, on = 1, with.col = Cl, overlay = TRUE, col = "pink")))
          
        })
        
      })
    }
  })
}

shinyApp(ui = ui, server = server)
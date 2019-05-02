# ============================================================
# Assignment 4 : Time Series Prediction for Cryptocurriences
# ============================================================

# ---------------------------------------------------------
# Install and load essential packages

# install.packages("shiny")
# install.packages("tseries")
# install.packages("forecast")
# install.packages("dplyr")
# install.packages("zoo")
# install.packages("ggplot2")
# install.packages("fma")
# install.packages("gridExtra")
library(shiny)
library(tseries)
library(forecast)
library(dplyr)
library(zoo)
library(ggplot2)
library(fma)
library(gridExtra)

# ---------------------------------------------------------
# Load the dataset to be used and simple explorations
cryptoData1 <- read.csv("crypto-markets.csv", header = TRUE)[, c("symbol", "date",  "open", "close")]
str(cryptoData1)
cryptoData1$date <- as.Date(cryptoData1$date) # convert the data from a factor to date variable type
str(cryptoData1)
class(cryptoData1) # it is in the data.frame format

# -------------------------------------------------------
# Treating the data as a true Time Series dataset

# View as Time Series
bitcoin <- subset(cryptoData1, symbol =="BTC")
ethereum <- subset(cryptoData1, symbol =="ETH")
xrp <- subset(cryptoData1, symbol =="XRP")
bitcoin_cash <- subset(cryptoData1, symbol =="BCH")
litecoin <- subset(cryptoData1, symbol =="LTC")


shinyServer(function(input,output,session){
  MyData <- reactive({
    if(input$dataset=="BTC"){
      DData <- bitcoin
    }
    else if(input$dataset=="ETH"){
      DData <- ethereum
    }
    else if(input$dataset=="XRP"){
      DData <- xrp
    }
    else if(input$dataset=="BCH"){
      DData <- bitcoin_cash
    }
    else if(input$dataset=="LTC"){
      DData <- litecoin
    }
    StartDate <- as.Date(as.yearmon(Start()))
    EndDate <- as.Date(as.yearmon(End())+ 11/12, frac = 1)
    DData <- DData[DData$date >= StartDate & DData$date<=EndDate,]
  })
  # Record the input forecast horizon
  getHorizon <- reactive({ input$horizon })
  #choose column to analyse
  Col<-reactive({
    if (input$Col=="Open"){return(3)}
    else if (input$Col=="Close"){return(4)}
  })
  # Get the dataset chosen by the user
  getDataset<- reactive({
    crypto_ts_zoo <- zoo(MyData()[,Col()], seq(from = as.Date(as.yearmon(Start())), to = as.Date(as.yearmon(End()) + 11/12, frac = 1), by = 1))
    crypto_ts_ <-  zooreg(crypto_ts_zoo)
    crypto_ts <- ts(crypto_ts_)
    return(crypto_ts)
  })
  
  Start<-reactive({input$Start})
  End<-reactive({input$End})
  Fre<-reactive({input$freq})
  output$summary<-renderPrint({
    summary(MyData()[,Col()])
  })
  output$table<-renderDataTable({
    MyData()
  })
  output$PlotG<-renderPlot({
    if(is.null(MyData())!=T){
      plot(MyData()[,Col()],ylab="Observations")+lines(MyData()[,Col()])
    }
  })

  # Plot the forecast plot for Mean
  output$meanForecastPlot <- renderPlot({
    fitModel <- meanf(getDataset(), h = getHorizon())
    autoplot(fitModel, main="", xlab="Days", ylab="Prices in USD")
  })
  
  # Plot the forecast plot for Naive
  output$naiveForecastPlot <- renderPlot({
    fitModel <- naive(getDataset(), h = getHorizon())
    autoplot(fitModel, main="", xlab="Days", ylab="Prices in USD")
  })
  
  # Plot the forecast plot for Drift
  output$driftForecastPlot <- renderPlot({
    fitModel <- rwf(getDataset(), h = getHorizon(), drift = TRUE)
    autoplot(fitModel, main="", xlab="Days", ylab="Prices in USD")
  })
  # Plot the ses ETS plot
  output$sesForecastPlot <- renderPlot({
    etsFit <- ets(getDataset(), model = 'ANN', damped = FALSE)
    fitModel <- forecast(etsFit, h = getHorizon(),level=input$CI)
    autoplot(fitModel, main="", xlab="Days", ylab="Prices in USD")
  })
  # Plot the hlm ETS plot
  output$hlmForecastPlot <- renderPlot({
    etsFit <- ets(getDataset(), model = 'AAN', damped = FALSE)
    fitModel <- forecast(etsFit, h = getHorizon(),level=input$CI2)
    autoplot(fitModel, main="", xlab="Days", ylab="Prices in USD")
  })
  # Plot the auto ETS plot
  output$AutoETS <- renderPlot({
    etsFit <- ets(getDataset(), model = 'ZZZ', damped = FALSE)
    fitModel <- forecast(etsFit, h = getHorizon(),level=input$CI)
    autoplot(fitModel, main="", xlab="Days", ylab="Prices in USD")
  })
  output$ArimaPlot <- renderPlot({
    arima_auto<- auto.arima(getDataset())
    # autoplot(arima_auto)
    arimaOptFC <- forecast(getDataset(), model = arima_auto, h = getHorizon())
    plot(arimaOptFC,xlab = "Days", ylab="Prices in USD")
  })
  
  # Cross validation
  output$CrossValid <- renderPlot({
    sesf <- function(y,h) forecast(ets(getDataset(), model = 'ANN', damped = FALSE),h=getHorizon())
    e <- tsCV(getDataset(), sesf, getHorizon())
    #summary(e)
    #sqrt(mean(e^2, na.rm=TRUE))
    
    # Compute the MSE values and remove missing values
    rmse <- sqrt(colMeans(e^2, na.rm = T))
    mape <- 100*colMeans(e^2/getDataset()^2, na.rm = T)
    # Plot the MSE values against the forecast horizon
    ses_df <- data.frame(h = 1:getHorizon(), RMSE = rmse, MAPE=mape)
    g1<-ggplot(data=ses_df,aes(x = h)) +
      geom_line(aes(y=RMSE),colour="black") + 
      geom_line(aes(y=MAPE),colour="blue") + 
      xlab("h for SES Method") +
      ylab('cross-validated values')
    hlmf <- function(y,h) forecast(ets(getDataset(), model = 'AAN', damped = FALSE),h=getHorizon())
    e <- tsCV(getDataset(), hlmf, getHorizon())
    #summary(e)
    #sqrt(mean(e^2, na.rm=TRUE))
    
    # Compute the MSE values and remove missing values
    rmse <- colMeans(e^2, na.rm = T)
    mape <- 100*colMeans(e^2/getDataset()^2, na.rm = T)
    # Plot the MSE values against the forecast horizon
    hlm_df <- data.frame(h = 1:getHorizon(), RMSE = rmse,MAPE=mape)
    g2 <- ggplot(data=hlm_df,aes(x = h)) +
      geom_line(aes(y=RMSE),colour="black") + 
      geom_line(aes(y=MAPE),colour="blue") + 
      xlab("h for Holt's Linear Method") +
      ylab('cross-validated values')
    autof <- function(y,h) forecast(ets(getDataset(), model = 'ZZZ', damped = FALSE),h=getHorizon())
    e <- tsCV(getDataset(), autof, getHorizon())
    #summary(e)
    #sqrt(mean(e^2, na.rm=TRUE))
    
    # Compute the MSE values and remove missing values
    rmse <- colMeans(e^2, na.rm = T)
    mape <- 100*colMeans(e^2/getDataset()^2, na.rm = T)

    # Plot the MSE values against the forecast horizon
    auto_df <- data.frame(h = 1:getHorizon(), RMSE = rmse,MAPE=mape)
    g3 <- ggplot(data=auto_df,aes(x = h)) +
      geom_line(aes(y=RMSE),colour="black") + 
      geom_line(aes(y=MAPE),colour="blue") + 
      xlab("h for Automatic Smoothing Method") +
      ylab('cross-validated values')
    
    #ggarrange(g1, g2,g3, ncol=3)
    grid.arrange(g1,g2,g3, ncol=3)
    
  })

  
})
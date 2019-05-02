# Run app from here
library(shiny)
library(shinythemes)


shinyUI(navbarPage(
  theme = shinytheme("united"),
  strong("Time Series Forecasting App"),
  tabPanel("Data Summary",
           sidebarLayout(
             sidebarPanel(
               h4(strong("Note: The plots take a while to render please wait for the loading of the plots in each tab")),
               radioButtons("dataset",label=h3(strong("Cryptocurreny Type")),choices = list("BTC"="BTC","ETH"="ETH","XRP"="XRP","BCH"="BCH","LTC"),selected = "BTC"),
               selectInput("Col", "Column to Analysis",
                           list("Open" = "Open", 
                                "Close" = "Close"),selected="Open"),
               numericInput("horizon", "Forecast Horizon (Days)", 60),
               h4(strong("Time Series Setting")),
               numericInput("Start",strong("Start Period"),value = 2017),
               numericInput("End",strong("End Period"),value = 2018),
               numericInput("freq",strong("Frequency (Days)"),value=365)

               
             ),
             
             
             
             mainPanel(fluidRow(
               
               column(10,h3("Summary"),verbatimTextOutput("summary")),
               column(10,h3("Table"),dataTableOutput("table")),
               column(10,h3("Plot"),plotOutput("PlotG"))
               
             )
             
             )
             
           )),
  

  #############################################################################
  navbarMenu("Simple Method",       
             tabPanel("Mean Forecast",
                      sidebarLayout(
                        sidebarPanel(
                          h4(strong("Note: The plots take a while to render please wait for the loading of the plots in each tab")),
                          'Mean Forecasting Method has Fixed Confidence intervals as shown in the graphs.'
                          
                        ),
                        mainPanel(
                          fluidRow(
                            h3('Model Introduction'),
                            p('Simple Mean Forecasting',align='justify'),
                            
                            column(10,h4(strong('Forecasting Plot')),plotOutput("meanForecastPlot"))
                          )
                        )
                      )),
             tabPanel("Naive Forecast",
                      sidebarLayout(
                        (sidebarPanel(
                          h4(strong("Note: The plots take a while to render please wait for the loading of the plots in each tab")),
                          'Naive Method has Fixed Confidence intervals as shown in the graphs.'
                        )),
                        mainPanel(
                          fluidRow(
                            h3('Model Introduction'),
                            p('Naive forecasts are the most cost effective forecasting model. ',align='justify'),
                            p('Generally, it just uses the values of past to predict the near future.',align='justify'),
                            
                            column(10,h4(strong('Forecasting Plot')),plotOutput("naiveForecastPlot"))
                          )
                        )
                      )),
             tabPanel("Drift Forecast",
                      sidebarLayout(
                        sidebarPanel(
                          h4(strong("Note: The plots take a while to render please wait for the loading of the plots in each tab")),
                          'Drift Method has Fixed Confidence intervals as shown in the graphs.'
                        ),
                        mainPanel(
                          fluidRow(
                            h3('Model Introduction'),
                            p('Forecasting with Drift',align='Justify'),
                            column(10,h4(strong('Forecasting Plot')),plotOutput("driftForecastPlot"))
                          )
                        )
                      ))),
  
  
  #############################################################################
  navbarMenu("Smoothing Method",       
             tabPanel("Simple Exponential Smoothing",
                      sidebarLayout(
                        sidebarPanel(
                          h4(strong("Note: The plots take a while to render please wait for the loading of the plots in each tab")),
                          sliderInput("CI",label="Confience Interval",min=0.1,max=0.9,value=0.9)
                          
                        ),
                        mainPanel(
                          fluidRow(
                            h3('Model Introduction'),
                            p('Simple exponential smoothing.' ,align='justify'),
                            p('The simplest of the exponentially smoothing methods is naturally called simple exponential smoothing (SES). ',align='justify'),
                            p('(In some books, it is called single exponential smoothing.)',align='justify'),
                            p('This method is suitable for forecasting data with no trend or seasonal pattern.',align='justify'),
                            
                            column(10,h4(strong('Forecasting Plot')),plotOutput("sesForecastPlot"))
                          )
                        )
                      )),
             
             tabPanel("Holt's Linear Method",
                      sidebarLayout(
                        sidebarPanel(
                          h4(strong("Note: The plots take a while to render please wait for the loading of the plots in each tab")),
                          sliderInput("CI2",label="Confience Interval",min=0.1,max=0.9,value=0.9)
                          
                          
                        ),
                        mainPanel(
                          fluidRow(
                            h3('Model Introduction'),
                            p("Holt's Linear is an extended simple exponential smoothing that allows the forecasting of data with a trend.",align='Justify'),
                            column(10,h4(strong('Forecasting Plot')),plotOutput("hlmForecastPlot"))
                          )
                        )
                      )),
             tabPanel("Auto Smoothing Method",
                      sidebarLayout(
                        sidebarPanel(
                          h4(strong("Note: The plots take a while to render please wait for the loading of the plots in each tab")),
                          'Auto Smoothing Method has Fixed Confidence intervals as shown in the graphs.'
                          
                          
                          
                          
                        ),
                        mainPanel(
                          fluidRow(
                            h3('Model Introduction'),
                            p('Automatically selected Exponential Smoothing Model',align='Justify'),
                            column(10,h4(strong('Forecasting Plot')),plotOutput("AutoETS"))
                            
                          )
                        )
                      )
             ),
             
             tabPanel("Cross Validation",
                      sidebarLayout(
                        sidebarPanel(
                          h4(strong("Note: The plots take more than 10 minutes to render please wait for the loading of the plots in each tab")),
                          h5(strong("Color Legend:")),
                          h5("RMSE- black"),
                          h5("MAPE- blue"),
                          p("An Example of our cross validation analysis. "),
                          p("An explanation of the errors variation observed for BTC open data:"),
                          p("We have used Root Mean Squared Error(RMSE) and Mean Absolute Percentage Error (MAPE) to do the cross-validation."), 
                          p("All the plots have negligible MAPE values and as a result it is not a good measurement for the cross validation."),
                          p("Looking at the plots, it can be seen that RMSE values for the Holt's Linear method is the highest among the 3 models. Since Holt's Linear model takes into account both error and trend, the RMSE values are high as our plots do not constitute of a fixed trend."), 
                          p("Therefore, our predictions are based on the Simple Exponential Smoothing (SES) method and the Automatic Model. The gradient for the RMSE values of the Automatic Model is very similar when comparing with the RMSE values of SES method. Hence, we conclude that the SES method is the best model for BTC Open.")
                        ),
                        mainPanel(
                          fluidRow(
                            h3('Model Introduction'),
                            p("Cross Validation for all Exponential Time Series Methods: ",align='Justify'),
                            p("Simple Exponential Smoothing(SES), Holt's Linear Method(HLM), Automatic Smoothing Method.",align='Justify'),
                            column(10,h4(strong('Forecasting Plot')),plotOutput("CrossValid"))
                          
                          )
                        )
                      ))
             ),

  

  tabPanel("ARIMA Method",
           sidebarLayout(
             sidebarPanel(
               h4(strong("Note: The plots take a while to render please wait for the loading of the plots in each tab")),
               
               'ARIMA Method has Fixed Confidence intervals as shown in the graphs.'
               
             ),
             
             mainPanel(
               fluidRow(
                 h3('Model Introduction'),
                 p('In statistics and econometrics, and in particular in time series analysis, an autoregressive integrated moving average (ARIMA) model is a generalization of an autoregressive moving average (ARMA) model. These models are fitted to time series data either to better understand the data or to predict future points in the series (forecasting). They are applied in some cases where data show evidence of non-stationarity, where an initial differencing step (corresponding to the "integrated" part of the model) can be applied to reduce the non-stationarity.',align='Justify'),
                 
                 column(10,h4(strong('Forecasting Plot')),plotOutput("ArimaPlot"))
               )
             )
           )),
  tabPanel("Read Me",
           h3(strong("Assignment 4 : Forecasting")),
           br(),
           p('Download the data on Cryptocurrency Historical Prices from https://www.kaggle.com/jessevent/all-crypto-currencies/. The goal of this assignment is to create an R Shiny dashboard to analyze top 10 cryptocurrencies, and forecast their price.',align="Justify"),
           p( 'We have extracted the four columns "symbol", "date", "open", "close" from the dataset, and we are working with at least one year of latest data (as available in the dataset). R Shiny application have a dropdown menu to let the user choose one of the following cryptocurrencies -- Bitcoin, Ethereum, XRP, Bitcoin Cash, Litecoin -- the top 5 according to https://coinmarketcap.com/. For each such choice, R Shiny dashboard predict the price of the cryptocurrency for 2 months(60 days) in the future, using the best ETS model. It also displays the cross-validated accuracy of the best ETS model that is being used, and the parameters of the best ETS model. The forecast must contain the confidence intervals.',align="Justify")
           

  )
  
)
)

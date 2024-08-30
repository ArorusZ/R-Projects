library(shiny)
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(zoo)
library(forecast)
library(corrplot)
library(tseries)

# Load the data
data <- read_csv("ForEx Rates 2006.csv")

# Convert the date column to Date format
data$Date <- as.Date(data$Date, format = "%d-%m-%Y")

# Define UI
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom_styles.css")
  ),
  
  titlePanel("Currency Forecasting with Various Models"),
  
  sidebarLayout(
    sidebarPanel(
      div(style = "overflow-y: scroll; max-height: 600px;",  # Adjust the max-height as needed
          selectInput("currency", "Select Currency:", 
                      choices = c("USD", "AUS","EUR", "GBP","SGD","AED","BRL","CNY","RUB","SAR")),
          hr(),
          h4("Plot Selection"),
          radioButtons("plot_choice", "Choose Plot:",
                       choices = list("Time Series Plot" = "time_series",  # New option added
                                      "Holt-Winters Model" = "holtwinters", 
                                      "ACF and PACF" = "acf_pacf",
                                      "ARIMA/SARIMA Model" = "arima_sarima",
                                      "Residual Analysis" = "residual_analysis"),
                       selected = "time_series"),
          conditionalPanel(
            condition = "input.plot_choice == 'acf_pacf'",
            radioButtons("acf_pacf_type", "Choose Data Type:",
                         choices = list("Original Data" = "original", 
                                        "Differenced Data" = "differenced"),
                         selected = "original")
          ),
          hr(),
          verbatimTextOutput("modelSummary"),
          verbatimTextOutput("forecastSummary")
      )
    ),
    
    mainPanel(
      conditionalPanel(
        condition = "input.plot_choice == 'time_series'",
        plotOutput("timeSeriesPlot")  # New plot output added
      ),
      conditionalPanel(
        condition = "input.plot_choice == 'holtwinters'",
        plotOutput("plotHoltWinters"),
        plotOutput("plotForecast")
      ),
      conditionalPanel(
        condition = "input.plot_choice == 'acf_pacf'",
        plotOutput("acfPlot"),
        plotOutput("pacfPlot")
      ),
      conditionalPanel(
        condition = "input.plot_choice == 'arima_sarima'",
        plotOutput("arimaPlot"),
        plotOutput("sarimaForecastPlot")
      ),
      conditionalPanel(
        condition = "input.plot_choice == 'residual_analysis'",
        plotOutput("residualHistogram"),
        plotOutput("residualQQPlot"),
        verbatimTextOutput("ljungBoxTest")
      )
    )
  )
)

server <- function(input, output) {
  
  # Reactive expression to generate the monthly data
  monthly_data <- reactive({
    data %>%
      mutate(YearMonth = floor_date(Date, "month")) %>%
      group_by(YearMonth) %>%
      summarize(across(where(is.numeric), mean, na.rm = TRUE))
  })
  
  # Reactive expression to generate the differenced data
  differenced_data <- reactive({
    diff_data <- diffinv(monthly_data()[[input$currency]], differences = 1)
    ts(diff_data[-1], start = c(2006, 2), frequency = 12)  # Remove first element due to differencing
  })
  
  # Reactive expression to fit the Holt-Winters model
  holt_winters_model <- reactive({
    selected_currency <- monthly_data()[[input$currency]]
    exchange_rate_ts <- ts(selected_currency, start = c(2006, 1), frequency = 12)
    HoltWinters(exchange_rate_ts)
  })
  
  # Reactive expression to fit the ARIMA model
  arima_model <- reactive({
    selected_currency <- monthly_data()[[input$currency]]
    exchange_rate_ts <- ts(selected_currency, start = c(2006, 1), frequency = 12)
    auto.arima(exchange_rate_ts, seasonal = F)
  })
  
  # Reactive expression to fit the SARIMA model
  sarima_model <- reactive({
    selected_currency <- monthly_data()[[input$currency]]
    exchange_rate_ts <- ts(selected_currency, start = c(2006, 1), frequency = 12)
    auto.arima(exchange_rate_ts, seasonal = T)
  })
  
  # Reactive expression to calculate ARIMA residuals
  arima_residuals <- reactive({
    residuals(arima_model())
  })
  
  # Summary of the models
  output$modelSummary <- renderPrint({
    if (input$plot_choice == "holtwinters") {
      summary(holt_winters_model())
    } else if (input$plot_choice == "arima_sarima") {
      summary(arima_model())
    } else if (input$plot_choice == "acf_pacf") {
      if (input$acf_pacf_type == "original") {
        selected_currency <- monthly_data()[[input$currency]]
        exchange_rate_ts <- ts(selected_currency, start = c(2006, 1), frequency = 12)
        adf.test(exchange_rate_ts)
      } else if (input$acf_pacf_type == "differenced") {
        adf.test(differenced_data())
      }
    }else if (input$plot_choice == "residual_analysis") {
      ljung_box_test <- Box.test(arima_residuals(), lag = 12, type = "Ljung-Box")
      print(ljung_box_test)
    }
  })
  
  # Plot the time series data
  output$timeSeriesPlot <- renderPlot({
    if (input$plot_choice == "time_series") {
      selected_currency <- monthly_data()[[input$currency]]
      exchange_rate_ts <- ts(selected_currency, start = c(2006, 1), frequency = 12)
      plot(exchange_rate_ts, main = paste("Time Series Plot for", input$currency, "to INR"), 
           ylab = paste(input$currency, "to INR"), xlab = "Year", col = "#3a855d")
      legend("topleft", legend = c(paste(input$currency, "to INR")), col = "#3a855d", lty = 1)
    }
  })
  
  # Plot the fitted Holt-Winters model
  output$plotHoltWinters <- renderPlot({
    if (input$plot_choice == "holtwinters") {
      plot(holt_winters_model(), main = paste("Holt-Winters Model for", input$currency, "to INR"))
      legend("topleft", legend = c("Observed", "Fitted"), 
             col = c("black", "red"), lty = c(1,1))
    }
  })
  
  # Forecasting the next 12 periods (months) with Holt-Winters
  holt_winters_forecast <- reactive({
    forecast(holt_winters_model(), h = 12)
  })
  
  # Summary of the Holt-Winters forecast
  output$forecastSummary <- renderPrint({
    if (input$plot_choice == "holtwinters") {
      summary(holt_winters_forecast())
    } else if (input$plot_choice == "arima_sarima") {
      summary(sarima_model())
    }
  })
  
  # Plot the Holt-Winters forecast
  output$plotForecast <- renderPlot({
    if (input$plot_choice == "holtwinters") {
      plot(holt_winters_forecast(), main = paste("Holt-Winters Forecast for", input$currency, "to INR"),col = c("black","#3a855d"))
      # Add the forecasted line with a specific color
      legend("topleft", legend = c("Observed", "Forecast"), 
             col = c("black", "blue"), lty = c(1, 1))
    }
  })
  
  # Plot ACF
  output$acfPlot <- renderPlot({
    if (input$plot_choice == "acf_pacf") {
      if (input$acf_pacf_type == "original") {
        selected_currency <- monthly_data()[[input$currency]]
        exchange_rate_ts <- ts(selected_currency, start = c(2006, 1), frequency = 12)
        Acf(exchange_rate_ts, main = paste("ACF for Original Data:", input$currency, "to INR"))
      } else if (input$acf_pacf_type == "differenced") {
        Acf(differenced_data(), main = paste("ACF for Differenced Data:", input$currency, "to INR"))
      }
    }
  })
  
  # Plot PACF
  output$pacfPlot <- renderPlot({
    if (input$plot_choice == "acf_pacf") {
      if (input$acf_pacf_type == "original") {
        selected_currency <- monthly_data()[[input$currency]]
        exchange_rate_ts <- ts(selected_currency, start = c(2006, 1), frequency = 12)
        Pacf(exchange_rate_ts, main = paste("PACF for Original Data:", input$currency, "to INR"))
      } else if (input$acf_pacf_type == "differenced") {
        Pacf(differenced_data(), main = paste("PACF for Differenced Data:", input$currency, "to INR"))
      }
    }
  })
  
  # Plot the ARIMA model forecast
  output$arimaPlot <- renderPlot({
    if (input$plot_choice == "arima_sarima") {
      forecast_arima <- forecast(arima_model(), h = 12)
      plot(forecast_arima, main = paste("ARIMA Forecast for", input$currency, "to INR"))
      legend("topleft", legend = c("Observed", "Forecast"), 
             col = c("black", "blue"), lty = c(1, 1))
    }
  })
  
  # Plot the SARIMA model forecast
  output$sarimaForecastPlot <- renderPlot({
    if (input$plot_choice == "arima_sarima") {
      forecast_sarima <- forecast(sarima_model(), h = 12)
      plot(forecast_sarima, main = paste("SARIMA Forecast for", input$currency, "to INR"))
      legend("topleft", legend = c("Observed", "Forecast"), 
             col = c("black", "blue"), lty = c(1, 1))
    }
  })
  
  # Residual Analysis: Histogram of ARIMA residuals with Normal Distribution Curve
  output$residualHistogram <- renderPlot({
    if (input$plot_choice == "residual_analysis") {
      hist(arima_residuals(), 
           main = "Histogram of ARIMA Residuals with Normal Curve", 
           xlab = "Residuals", 
           col = "#3a855d", 
           border = "white", 
           prob = TRUE)
      curve(dnorm(x, mean = mean(arima_residuals()), sd = sd(arima_residuals())), 
            col = "red", 
            lwd = 2, 
            add = TRUE)
      legend("topleft", legend = c("Normal Curve"), col = c("red"), lty = c(1))
    }
  })
  
  # Residual Analysis: QQ plot of the residuals
  output$residualQQPlot <- renderPlot({
    if (input$plot_choice == "residual_analysis") {
      qqnorm(arima_residuals(), main = "QQ Plot of ARIMA Residuals")
      qqline(arima_residuals(), col = "red")
      legend("topleft", legend = c("Theoretical Quantiles"), col = c("red"), lty = c(1))
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

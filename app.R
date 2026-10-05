# BDA400 Assignment 6
# Technical Analysis using R - Visualization Phase
# Student: Parvaneh Rezaei

library(shiny)
library(ggplot2)
library(quantmod)

# User Interface

ui <- fluidPage(
  
  titlePanel("BDA400 Portfolio Dashboard"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      textInput("symbol", "Stock Symbol:", value = "AAPL"),
      
      dateRangeInput(
        "date_range",
        "Select Date Range:",
        start = "2023-01-01",
        end = "2023-07-01"
      ),
      selectInput(
        "time_frame",
        "Select Time Frame:",
        choices = c("Daily", "Weekly", "Monthly")
      ),
      
      checkboxGroupInput(
        "technical_indicators",
        "Technical Indicators:",
        choices = c(
          "Moving Averages",
          "RSI",
          "MACD"
        )
      )
    ),   
    mainPanel(
      plotOutput("stock_chart")
    )
  )
)
# Server

server <- function(input, output) {
  stock_data <- reactive({
    getSymbols(
      input$symbol,
      src = "yahoo",
      from = input$date_range[1],
      to = input$date_range[2],
      auto.assign = FALSE
    )
  })
  
  output$stock_chart <- renderPlot({
    
    filtered_data <- stock_data()
    # Apply selected time frame
    if (input$time_frame == "Weekly") {
      filtered_data <- to.weekly(filtered_data, indexAt = "lastof", drop.time = TRUE)
    } else if (input$time_frame == "Monthly") {
      filtered_data <- to.monthly(filtered_data, indexAt = "lastof", drop.time = TRUE)
    }
    
    plot_data <- data.frame(
      Date = index(filtered_data),
      Close = as.numeric(Cl(filtered_data))
    )# Calculate Moving Averages
    plot_data$MA20 <- SMA(plot_data$Close, n = 20)
    plot_data$MA50 <- SMA(plot_data$Close, n = 50)
    # Generate Buy and Sell Signals
    plot_data$Signal <- NA
    
    plot_data$Signal[
      plot_data$MA20 > plot_data$MA50
    ] <- "Buy"
    
    plot_data$Signal[
      plot_data$MA20 < plot_data$MA50
    ] <- "Sell"
    # Calculate RSI
    plot_data$RSI <- RSI(plot_data$Close, n = 14)
    # Calculate MACD
    macd_values <- MACD(
      plot_data$Close,
      nFast = 12,
      nSlow = 26,
      nSig = 9
    )
    
    plot_data$MACD <- as.numeric(macd_values[, "macd"])
    plot_data$MACD_Signal <- as.numeric(macd_values[, "signal"])
    p <- ggplot(plot_data, aes(x = Date, y = Close)) +
      geom_line() +
      labs(
        title = paste(input$symbol, "Stock Price"),
        x = "Date",
        y = "Closing Price"
      )
    
    if ("Moving Averages" %in% input$technical_indicators) {
      p <- p +
        geom_line(aes(y = MA20), linewidth = 0.8) +
        geom_line(aes(y = MA50), linewidth = 0.8)
    }
    p <- p +
      geom_point(
        data = subset(plot_data, Signal == "Buy"),
        aes(x = Date, y = Close),
        shape = 24,
        size = 3
      ) +
      geom_point(
        data = subset(plot_data, Signal == "Sell"),
        aes(x = Date, y = Close),
        shape = 25,
        size = 3
      )
    if ("RSI" %in% input$technical_indicators) {
      p <- p +
        geom_line(aes(y = RSI), linewidth = 0.8)
    } 
    if ("MACD" %in% input$technical_indicators) {
      p <- p +
        geom_line(aes(y = MACD), linewidth = 0.8) +
        geom_line(aes(y = MACD_Signal), linewidth = 0.8)
    }
    print(p)
  })
}
shinyApp(ui = ui, server = server)
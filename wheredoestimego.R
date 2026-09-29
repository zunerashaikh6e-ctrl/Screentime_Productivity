## =============================================================
##  WHERE DOES MY TIME GO? - Interactive R Shiny App
##  SML Micro-Project
## =============================================================
##  HOW TO RUN THIS APP:
##  1. Install Shiny once (only needed the first time):
##       install.packages("shiny")
##  2. Put this file (app.R) and "time_log_data_simple.csv" in the
##     SAME folder.
##  3. Open app.R in RStudio.
##  4. Click the green "Run App" button at the top-right of the
##     script editor (or run:  shiny::runApp()  in the console).
##  5. A window/browser tab will open with the live app.
## =============================================================

library(shiny)

## -------------------------------------------------------------
## LOAD DATA
## -------------------------------------------------------------

data <- read.csv("time_log_data_simple.csv")
categories <- c("Sleep", "Study", "ScreenTime", "Leisure", "Other")
colors5 <- c("skyblue", "orange", "tomato", "lightgreen", "gray")

## =================================================================
## UI - what the user sees
## =================================================================

ui <- fluidPage(
  
  titlePanel("Where Does My Time Go? - Interactive Time Use Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("person", "Select Group Member:",
                  choices = c("All Members", unique(data$Person))),
      hr(),
      helpText("This app explores how our group spends a typical week."),
      helpText("Use the tabs on the right to see:"),
      tags$ul(
        tags$li("Average time split (bar + pie chart)"),
        tags$li("Weekday vs Weekend comparison"),
        tags$li("Hypothesis test on screen time"),
        tags$li("Screen time vs sleep relationship")
      )
    ),
    
    mainPanel(
      tabsetPanel(
        
        tabPanel("Time Distribution",
                 br(),
                 plotOutput("barPlot"),
                 plotOutput("piePlot")),
        
        tabPanel("Weekday vs Weekend",
                 br(),
                 plotOutput("boxPlot"),
                 h4("Hypothesis Test Result"),
                 verbatimTextOutput("tTestResult")),
        
        tabPanel("Screen Time vs Sleep",
                 br(),
                 plotOutput("scatterPlot"),
                 verbatimTextOutput("corrResult")),
        
        tabPanel("Raw Data",
                 br(),
                 tableOutput("dataTable"))
      )
    )
  )
)

## =================================================================
## SERVER - the logic behind the app
## =================================================================

server <- function(input, output) {
  
  ## Filters the dataset based on the dropdown selection
  filteredData <- reactive({
    if (input$person == "All Members") {
      data
    } else {
      subset(data, Person == input$person)
    }
  })
  
  ## ---- Bar chart: average hours per activity ----
  output$barPlot <- renderPlot({
    averages <- colMeans(filteredData()[categories])
    barplot(averages,
            main = "Average Time Spent on Each Activity",
            ylab = "Hours per Day",
            col = colors5)
  })
  
  ## ---- Pie chart: same data, different view ----
  output$piePlot <- renderPlot({
    averages <- colMeans(filteredData()[categories])
    pie(averages,
        main = "Where Does My Time Go?",
        col = colors5)
  })
  
  ## ---- Boxplot: Weekday vs Weekend screen time ----
  output$boxPlot <- renderPlot({
    d <- filteredData()
    weekday <- d$ScreenTime[d$DayType == "Weekday"]
    weekend <- d$ScreenTime[d$DayType == "Weekend"]
    boxplot(weekday, weekend,
            names = c("Weekday", "Weekend"),
            main = "Screen Time: Weekday vs Weekend",
            ylab = "Hours",
            col = c("skyblue", "orange"))
  })
  
  ## ---- Hypothesis Test: is screen time really different on weekends? ----
  ## H0 (Null Hypothesis): average screen time is the same on weekdays and weekends
  ## H1 (Alternative): average screen time is different on weekends
  output$tTestResult <- renderPrint({
    d <- filteredData()
    weekday <- d$ScreenTime[d$DayType == "Weekday"]
    weekend <- d$ScreenTime[d$DayType == "Weekend"]
    
    cat("H0: No difference in average screen time (weekday = weekend)\n")
    cat("H1: Screen time differs between weekday and weekend\n\n")
    
    if (length(weekday) < 2 || length(weekend) < 2) {
      cat("Not enough data points in this selection to run a t-test.\n")
      cat("Try selecting 'All Members' for a bigger sample.\n")
    } else {
      result <- t.test(weekend, weekday)
      print(result)
      
      if (result$p.value < 0.05) {
        cat("\nConclusion: p-value < 0.05 -> Reject H0.\n")
        cat("Screen time is significantly different on weekends.\n")
      } else {
        cat("\nConclusion: p-value >= 0.05 -> Fail to reject H0.\n")
        cat("No significant difference in screen time.\n")
      }
    }
  })
  
  ## ---- Scatter plot: Screen Time vs Sleep, with regression line ----
  output$scatterPlot <- renderPlot({
    d <- filteredData()
    plot(d$ScreenTime, d$Sleep,
         main = "Screen Time vs Sleep",
         xlab = "Screen Time (hours)",
         ylab = "Sleep (hours)",
         pch = 19, col = "blue")
    if (nrow(d) >= 2) {
      abline(lm(Sleep ~ ScreenTime, data = d), col = "red", lwd = 2)
    }
  })
  
  ## ---- Correlation value ----
  output$corrResult <- renderPrint({
    d <- filteredData()
    if (nrow(d) >= 2) {
      correlation <- cor(d$ScreenTime, d$Sleep)
      cat("Correlation between Screen Time and Sleep:", round(correlation, 2), "\n")
      if (correlation < 0) {
        cat("This is a negative correlation: more screen time tends to go with less sleep.\n")
      } else {
        cat("This is a positive correlation: more screen time tends to go with more sleep.\n")
      }
    } else {
      cat("Not enough data to calculate correlation.\n")
    }
  })
  
  ## ---- Raw data table ----
  output$dataTable <- renderTable({
    filteredData()
  })
}

## =================================================================
## RUN THE APP
## =================================================================

shinyApp(ui = ui, server = server)

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinyalert)
library(readr)
library(dplyr)
library(ggplot2)
library(DT)

bike_raw <-  read_csv(
  "./Data/SeoulBikeData.csv",
  col_names = TRUE,
  locale = locale(encoding = 'latin1')
)

ui <- fluidPage(
  titlePanel("ST558 Project 2: Exploring Seoul Bike Rental Data"),
  
  tabsetPanel(
    tabPanel(
      "About",
      h3(
        "Welcome! This site explores data collected from a bike rental service."
      ),
      p(
        "On the \"Data Exploration\" tab, you'll find a sidebar that will allow subsetting of the data. That data will then be graphed and summarized on the subtabs. On the summary tabs, you can also select variables to summarize. The full data is available at:"
      ),
      a("this link", href = "https://www.kaggle.com/datasets/saurabhshahane/seoul-bike-sharing-demand-prediction"),
      uiOutput("img")
    ),
    tabPanel(
      "Data Download",
      textOutput("row_count"),
      downloadButton("button_download"),
      dataTableOutput("download_out")
    ),
    tabPanel("Data Exploration", sidebarLayout(
      sidebarPanel(
        checkboxGroupInput(
          "season_setting",
          "Seasons:",
          c("Winter", "Spring", "Summer", "Autumn"),
          selected = c("Winter", "Spring", "Summer", "Autumn")
        ),
        checkboxGroupInput(
          "holiday_setting",
          "Type of day:",
          c("Holiday", "No Holiday"),
          selected = c("Holiday", "No Holiday")
        ),
        sliderInput(
          "temperature_setting",
          "Temperature (°C):",
          min = 0,
          max = 30,
          value = c(0, 20),
          step = .01
        ),
        sliderInput(
          "humidity_setting",
          "Humidity (Relative %)",
          min = 0,
          max = 100,
          value = c(30, 70),
          step = .01
        ),
        actionButton("button_submit", "Submit")
      ),
      mainPanel(tabsetPanel(
        tabPanel(
          "Graphs",
          fluidRow(column(width = 6, plotOutput("basic_hist")), column(width = 6, plotOutput("complex_hist"))),
          fluidRow(column(width = 6, plotOutput("basic_scatter")), column(width = 6, plotOutput("complex_scatter"))),
          fluidRow(column(width = 4), column(width = 4))
        ),
        tabPanel("Numeric Summaries", fluidRow(
          column(
            width = 12,
            checkboxGroupInput(
              "numeric_variables_setting",
              "Choose variables (confirm with submit button):",
              names(bike_raw |> select(where(is.numeric))),
              selected = names(bike_raw |> select(where(is.numeric)))
            )
          ),
          fluidRow(column(
            width = 12, tableOutput("num_sum_by_holiday")
          )),
          fluidRow(column(
            width = 12, tableOutput("num_sum_by_season")
          ))
        )),
        tabPanel(
          "Categorical Summaries",
          fluidRow(column(
            width = 12,
            checkboxGroupInput(
              "categorical_variables_setting",
              "Choose variables (confirm with submit button):",
              c("Holiday", "Seasons", "Functioning Day"),
              selected = c("Holiday", "Seasons", "Functioning Day")
            )
          )),
          fluidRow(
            column(width = 6, tableOutput("cat_tbl_holiday")),
            column(width = 6, tableOutput("cat_tbl_fd")),
            column(width = 6, tableOutput("cat_tbl_season"))
          )
        )
      ))
    ))
  )
)





server <- function(input, output, session) {
  subset_data <- bike_raw
  
  output$img <- renderUI({
    tags$img(src = "https://english.seoul.go.kr/wp-content/uploads/2022/06/ddareng1-2.png")
  })
  
  alert_if_empty_subset <- function(subset) {
    if (!(dim(subset)[1] > 0)) {
      shinyalert(
        "No Data",
        "Your subset choices exclude all available data. Try some other settings.",
        type = "error"
      )
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  
  bring_value_into_range <- function(values,
                                     lower = 0,
                                     upper = 10) {
    new_values <- c(lower, upper)
    
    if (!is.null(values) & values[1] >= lower) {
      new_values[1] <- values[1]
    }
    
    if (!is.null(values) & values[2] <= upper) {
      new_values[2] <- values[2]
    }
    
    return(new_values)
  }
  
  #our primary update function
  observeEvent(input$button_submit,
               {
                 subset_data <- bike_raw |>
                   filter(Seasons %in% input$season_setting) |>
                   filter(Holiday %in% input$holiday_setting)
                 
                 if (alert_if_empty_subset(subset_data)) {
                   return()
                 }
                 
                 temperature_upper <- max(subset_data$`Temperature(°C)`)
                 temperature_lower <- min(subset_data$`Temperature(°C)`)
                 temperature_value <- bring_value_into_range(input$temperature_setting,
                                                             temperature_lower,
                                                             temperature_upper)
                 
                 humidity_upper <- max(subset_data$`Humidity(%)`)
                 humidity_lower <- min(subset_data$`Humidity(%)`)
                 humidity_value <- bring_value_into_range(input$humidity_setting, humidity_lower, humidity_upper)
                 
                 updateSliderInput(
                   session,
                   "temperature_setting",
                   value = temperature_value,
                   min = temperature_lower,
                   max = temperature_upper
                 )
                 
                 subset_data <- subset_data |>
                   filter(
                     `Temperature(°C)` >= temperature_value[1] &
                       `Temperature(°C)` <= temperature_value[2]
                   )
                 
                 if (alert_if_empty_subset(subset_data)) {
                   return()
                 }
                 
                 updateSliderInput(
                   session,
                   "humidity_setting",
                   value = humidity_value,
                   min = humidity_lower,
                   max = humidity_upper
                 )
                 
                 subset_data <- subset_data |>
                   filter(`Humidity(%)` >= humidity_value[1] &
                            `Humidity(%)` <= humidity_value[2])
                 
                 if (alert_if_empty_subset(subset_data)) {
                   return()
                 }
                 
                 #begin rendering plots and summaries
                 output$basic_hist <- renderPlot(
                   subset_data |>
                     ggplot(aes(x = `Rented Bike Count`)) +
                     geom_histogram() +
                     ggtitle("Histogram of Rented Bike Count") +
                     xlab("Number of bikes rented") +
                     ylab("Count of days with n bikes rented")
                 )
                 
                 output$complex_hist <- renderPlot(
                   subset_data |>
                     ggplot(aes(x = `Rented Bike Count`)) +
                     geom_histogram(alpha = .5, aes(fill = Seasons)) +
                     ggtitle("Histogram of Rented Bike Count by Season") +
                     xlab("Number of bikes rented") +
                     ylab("Count of days with n bikes rented")
                 )
                 
                 output$basic_scatter <- renderPlot(
                   subset_data |>
                     ggplot(aes(x = `Temperature(°C)`, y = `Humidity(%)`)) +
                     geom_point() +
                     geom_smooth(method = "lm") +
                     ggtitle("Humidity vs. Temperature") +
                     xlab("Temperature (°C)") +
                     ylab("Humidity (Relative %)")
                 )
                 
                 output$complex_scatter <- renderPlot(
                   subset_data |>
                     ggplot(aes(x = `Temperature(°C)`, y = `Humidity(%)`)) +
                     geom_point() +
                     geom_smooth(method = "lm") +
                     facet_wrap("Seasons") +
                     ggtitle("Humidity vs. Temperature by Season") +
                     xlab("Temperature (°C)") +
                     ylab("Humidity (Relative %)")
                 )
                 
                 
                 if (is.null(input$numeric_variables_setting)) {
                   shinyalert(
                     "No Variables",
                     "There are no variables for numeric summaries. Select some and try again.",
                     type =  "error"
                   )
                   return()
                 }
                 
                 
                 num_sum_by_holiday <- subset_data |>
                   group_by(Holiday) |>
                   summarise(across(
                     input$numeric_variables_setting,
                     list(
                       "mean" = mean,
                       "median" = median,
                       "sd" = sd
                     )
                   ))
                 
                 output$num_sum_by_holiday <- renderTable(num_sum_by_holiday)
                 
                 num_sum_by_season <- subset_data |>
                   group_by(Seasons) |>
                   summarise(across(
                     input$numeric_variables_setting,
                     list(
                       "mean" = mean,
                       "median" = median,
                       "sd" = sd
                     )
                   ))
                 
                 output$num_sum_by_season <- renderTable(num_sum_by_season)
                 
                 
                 if (is.null(input$categorical_variables_setting)) {
                   shinyalert(
                     "No Variables",
                     "There are no variables for categorical summaries. Select some and try again.",
                     type =  "error"
                   )
                   return()
                 }
                 
                 if ("Holiday" %in% input$categorical_variables_setting) {
                   output$cat_tbl_holiday <-  renderTable(table(subset_data$Holiday))
                 } else {
                   output$cat_tbl_holiday <-  NULL
                 }
                 
                 if ("Seasons" %in% input$categorical_variables_setting) {
                   output$cat_tbl_season <-  renderTable(table(subset_data$Seasons))
                 } else {
                   output$cat_tbl_season <-  NULL
                 }
                 
                 if ("Functioning Day" %in% input$categorical_variables_setting) {
                   output$cat_tbl_fd <-  renderTable(table(subset_data$`Functioning Day`))
                 } else {
                   output$cat_tbl_fd <-  NULL
                 }
                 
                 output$download_out <- renderDataTable(subset_data)
                 
                 output$button_download <- downloadHandler(
                   filename = function() {
                     paste('data-', Sys.Date(), '.csv', sep = '')
                   },
                   content = function(con) {
                     write.csv(subset_data, con)
                   }
                 )
                 
                 output$row_count = renderText(paste(c("Total rows selected: ", dim(subset_data[1])), collapse = ""))
                 
               },
               ignoreInit = FALSE,
               ignoreNULL = FALSE)
  
}

shinyApp(ui = ui, server = server)
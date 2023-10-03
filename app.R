library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(rhandsontable)
library(shinythemes)
library(ggplot2)

# Load the dataset
data <- read.csv("dataset.csv", header = TRUE)

# Sorting the dataset
data <- data %>%
  arrange(age_5yr_70_id, province_id, area_council_id)

ui <- dashboardPage(
  dashboardHeader(title = "Vanuatu 2020 Population Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Datatable", tabName = "home"),
      menuItem("Visualization", tabName = "population")
    )
  ),
  dashboardBody(
    theme = shinytheme("cerulean"), 
    tags$head(
      tags$style(HTML(".total-population {text-align: center; font-size: 20px; font-weight: bold;}")),
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css") 
    ),
    tabItems(
      tabItem(
        tabName = "home",
        h2("Explore Vanuatu Population Data"),
        p("Explore demographic data for Vanuatu's provinces and area councils. Use the filters below to refine your selection based on province, area council, sex, and age group. The table presents a summarized view of the population data, showcasing the total population count for each combination of province and area council. By default, the table displays 10 rows, but you can paginate through the data to view additional records. Gain valuable insights into the population distribution across different regions of Vanuatu."),
        fluidRow(
          column(3,
                 selectInput("province", "Select a Province", 
                             choices = c("All", unique(data$province)), 
                             multiple = FALSE, selected = "All"),
                 selectInput("area_council", "Select an Area Council", 
                             choices = "All", multiple = FALSE, selected = "All"),
                 selectInput("sex", "Select Sex", 
                             choices = c("All", unique(data$sex)), 
                             multiple = FALSE, selected = "All"),
                 selectInput("age_5yr_70", "Select a Grouped Age",
                             choices = c("All", unique(data$age_5yr_70)),
                             multiple = FALSE, selected = "All")
          ),
          column(9,
                 DTOutput("filtered_data_table"),
                 tags$div(class = "total-population", textOutput("total_population"))
          )
        )
      ),
      tabItem(
        tabName = "population",
        h2("Population Data"),
        p("This is the population page content."),
        fluidRow(
          column(6,
                 style = "margin-right: 5px;",
                 box(
                   width = NULL,
                   solidHeader = TRUE,
                   title = "Population by Province",
                   plotlyOutput("bar_chart")
                 )
          ),
          column(6,
                 style = "margin-left: 5px;",
                 box(
                   width = NULL,
                   solidHeader = TRUE,
                   title = "Population Pyramid",
                   plotOutput("population_pyramid")
                 )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  filtered_data <- reactive({
    filtered <- data
    if (!"All" %in% input$province) {
      filtered <- filtered[filtered$province %in% input$province, ]
    }
    if (!"All" %in% input$area_council) {
      filtered <- filtered[filtered$area_council %in% input$area_council, ]
    }
    if (!"All" %in% input$sex) {
      filtered <- filtered[filtered$sex %in% input$sex, ]
    }
    if (!"All" %in% input$age_5yr_70) {
      filtered <- filtered[filtered$age_5yr_70 %in% input$age_5yr_70, ]
    }
    return(filtered)
  })
  
  observe({
    if (!"All" %in% input$province) {
      updateSelectInput(session, "area_council",
                        choices = c("All", unique(data$area_council[data$province %in% input$province])),
                        selected = "All")
    } else {
      updateSelectInput(session, "area_council",
                        choices = "All",
                        selected = "All")
    }
  })
  
  output$filtered_data_table <- renderDT({
    if (nrow(filtered_data()) > 0) {
      aggregated_data <- filtered_data() %>%
        group_by(Province = province, `Area Council` = area_council) %>%
        summarize(Population = round(sum(ac_factor), 0))
      datatable(aggregated_data, options = list(pageLength = 10)) 
    }
  })
  
  output$total_population <- renderText({
    total_population <- round(sum(filtered_data()$ac_factor), 0)
    paste("Total Population:", total_population)
  })
  
  output$bar_chart <- renderPlotly({
    bar_data <- filtered_data() %>%
      group_by(province) %>%
      summarize(Population = round(sum(ac_factor), 0))
    
    plot_ly(data = bar_data, x = ~province, y = ~Population, type = 'bar', 
            text = ~paste('Population:', Population)) %>%
      layout(title = "Population by Province", xaxis = list(title = "Province"))
  })
  
  output$population_pyramid <- renderPlot({
    pyramid_data <- filtered_data() %>%
      group_by(age_5yr_70, sex) %>%
      summarize(Population = sum(ac_factor))
    
    pyramid_data <- pyramid_data %>%
      mutate(Population = ifelse(sex == "Male", -Population, Population))
    
    pyramid_data <- pyramid_data %>%
      arrange(age_5yr_70_id)
    
    ggplot(pyramid_data, aes(x = age_5yr_70, y = Population, fill = sex)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = "Population Pyramid", x = "Population", y = "Age Group") +
      theme(legend.position="none")
  })
}

shinyApp(ui, server)

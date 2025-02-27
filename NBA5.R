# Load the necessary libraries
library(shiny)
library(ggplot2)
library(DT)  # For interactive tables
library(readxl)  # To read Excel files

# Load dataset
basketball_data <- read.csv("https://raw.githubusercontent.com/MarkCombs3115/First-Shiny-Project/refs/heads/main/Grouped_BBall_data.csv")

# Define UI for the application
ui <- fluidPage(
  
  # Application title
  titlePanel("Basketball Player Yearly Averages"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      
      # Input: Select stat to plot
      selectInput(inputId = "stat",
                  label = "Select Statistic:",
                  choices = c("PTS", "TRB", "AST"),
                  selected = "PTS"),
      
      # Input: Select players to compare
      checkboxGroupInput(inputId = "players",
                         label = "Select Players:",
                         choices = unique(basketball_data$Player),
                         selected = unique(basketball_data$Player))
    ),
    
    # Main panel with tabs
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput(outputId = "linePlot")),  # Tab for the plot
        tabPanel("Data Table", DTOutput("dataTable"))  # Tab for the filtered table
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive expression to filter data based on user input
  filtered_data <- reactive({
    basketball_data %>%
      filter(Player %in% input$players) %>%
      select(Player, Season, PTS, TRB, AST)  # Keep only required columns
  })
  
  # Render the line plot
  output$linePlot <- renderPlot({
    ggplot(filtered_data(), aes(x = Season, y = .data[[input$stat]], color = Player, group = Player)) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      labs(title = paste("Yearly", input$stat, "Comparison"),
           x = "Year",
           y = input$stat) +
      theme_minimal() +
      theme(legend.position = "bottom",
            axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Render the data table with selected columns
  output$dataTable <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 10))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

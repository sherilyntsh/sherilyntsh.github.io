library(tidyverse)
library(shiny)
library(shinythemes)
library(gridExtra)

dogs <- read.csv("akc dogs copy 2.csv")

dogs_filtered <- dogs %>%
  filter(!is.na(popularity) & popularity !="" & popularity !="of")

data_mutated_arranged <- dogs_filtered %>%
  mutate(mean_height = (min_height + max_height)/2) %>%
  mutate(mean_weight = (min_weight + max_weight)/2) %>%
  mutate(mean_expectancy = (min_expectancy + max_expectancy)/2) %>%
  mutate(popularity=as.numeric(popularity)) %>%
  arrange(popularity)

data <- data_mutated_arranged %>%
  select(breeds,group,mean_height,mean_weight,mean_expectancy) %>%
  slice(1:102)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinythemes::shinytheme("journal"),
  # Application title
  titlePanel("Comparison Visualization: Violin Chart"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      selectInput(inputId = "mvariables",
                  label = "Mean Variables:",
                  choices = c("mean_height","mean_weight","mean_expectancy"),
                  selected = "Mean Height")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("violinplot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$violinplot <- renderPlot({
      ggplot(data, aes(x = group, y = .data[[input$mvariables]], fill = group)) +
        geom_violin(alpha = 0.7) +
        labs(x = "Breed Groups", y = input$mvariables, fill = "Breed Groups") +
        ggtitle("Violin Chart Comparison")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
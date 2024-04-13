#BAR CHART

library(tidyverse)
library(shiny)
library(shinythemes)
library(gridExtra)

dogs <- read.csv("akc dogs copy.csv")

dogs_filtered <- dogs %>%
  filter(!is.na(popularity) & popularity !="" & popularity !="of")

data_mutated_arranged <- dogs_filtered %>%
  mutate(mean_height = (min_height + max_height)/2) %>%
  mutate(mean_weight = (min_weight + max_weight)/2) %>%
  mutate(mean_expectancy = (min_expectancy + max_expectancy)/2) %>%
  mutate(popularity=as.numeric(popularity)) %>%
  arrange(popularity)

data <- data_mutated_arranged %>%
  select(breeds,group,popularity,mean_height,mean_weight,mean_expectancy,shedding_value,
         energy_level_value,trainability_value) %>%
  slice(1:102)

data_plot <- data %>%
  mutate(shedding_value=as.factor(shedding_value)) %>%
  mutate(energy_level_value=as.factor(energy_level_value)) %>%
  mutate(trainability_value=as.factor(trainability_value)) 


# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinythemes::shinytheme("journal"),
    # Application title
    titlePanel("Summary Visualization: Bar Chart"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          textOutput("text"),
            selectInput(inputId = "variable",
                        label = "Variable:",
                        choices = c("shedding_value","energy_level_value","trainability_value"),
                        selected = "shedding_value")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("barplot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$text <- renderText("Change the variables to see which is the majority value for the shedding_value, energy_level_value and trainability_value.")

    output$barplot <- renderPlot({
       ggplot(data_plot, aes(x= group, fill = .data[[input$variable]])) +
        geom_bar() +
        labs(x = "Breed Group",y = "Count") +
        ggtitle("Count of Breed Groups")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

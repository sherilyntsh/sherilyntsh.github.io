library(tidyverse)
library(shiny)
library(shinythemes)
library(gridExtra)

dogs <- read.csv("akc dogs.csv")

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

data_wg <- data %>%
  filter(group == "Working Group")

# Define UI ----
ui <- fluidPage(
  theme = shinythemes::shinytheme("journal"),
  
  titlePanel("Table of Data Set"),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Descriptions", textOutput("description"),
                           h4("This tab shows the descriptions of the different filters."),
                           br(),
                           p(strong("Breed Group: "),"These are the different breed groups of the dogs."),
                           br(),
                           p(strong("Mean Height Range (cm):"), "This is calculated by adding the miniumum
                             height and the maximum height of each dog and divide it by 2."),
                           br(),
                           p(strong("Mean Weight Range (kg):"), "This is calculated by adding the minimum
                             weight and maximum weight of each dog and divide it by 2."),
                           br(),
                           p(strong("Mean Expectancy Range (years):"), "This is calculated by adding the minimum
                             life expectancy and maximum life expectancy of each dog and divide it by 2."),
                           br(),
                           p(strong("Shedding Value:"), "This number represents the shedding level. The higher
                             the value, the more frequent the shedding. "),em("0.2 = Infrequent, 0.4 = Occasional, 0.6 = Seasonal, 0.8 = Regular, 1.0 = Frequent"),
                           br(),
                           br(),
                           p(strong("Energy Level:"), "This number represents the energy level. The higher 
                             the value, the more energy the breed has."), em("0.2 = Couch Potato, 0.4 = Calm, 0.6 = Regular Exercise, 0.8 = Energetic, 1.0 = Needs Lots of Activity"),
                           br(),
                           br(),
                           p(strong("Trainability Value:"), "This number represents the trainability. The higher 
                             the value, the easier to train"), em("0.2 = May be Stubborn, 0.4 = Independent, 0.6 = Agreeable, 0.8 = Easy Training, 1.0 = Eager to Please")
                           ),
                  tabPanel("Table", 
                           sidebarLayout(
                             sidebarPanel(
                               textOutput("text"),
                               
                               selectInput(inputId = "breeds",
                                           label="Breed Group:",
                                           choices = c("All", unique(data$group)),
                                           selected = "All",
                                           multiple = TRUE),
                               
                               sliderInput(inputId = "mHeight",
                                           label = "Mean Height Range (cm)",
                                           min=min(data$mean_height, na.rm = TRUE),
                                           max=max(data$mean_height, na.rm = TRUE),
                                           value=c(min(data$mean_height, na.rm = TRUE), max(data$mean_height, na.rm = TRUE))),
                               
                               sliderInput(inputId="mWeight",
                                           label = "Mean Weight Range (kg)",
                                           min = min(data$mean_weight, na.rm = TRUE),
                                           max = max(data$mean_weight, na.rm = TRUE),
                                           value = c(min(data$mean_weight, na.rm = TRUE), max(data$mean_weight, na.rm = TRUE))),
                               
                               sliderInput(inputId="mExpectancy",
                                           label = "Mean Expectancy Range (years)",
                                           min = min(data$mean_expectancy, na.rm = TRUE),
                                           max = max(data$mean_expectancy, na.rm = TRUE),
                                           step = 0.5,
                                           value = c(min(data$mean_expectancy, na.rm = TRUE), max(data$mean_expectancy, na.rm = TRUE))),
                               
                               sliderInput(inputId="shedV",
                                           label="Shedding Value:",
                                           min = 0.2,
                                           max = 1.0,
                                           step = 0.2,
                                           value = c(0.2,1.0)),
                               
                               sliderInput(inputId = "energyV",
                                           label = "Energy Level:",
                                           min = 0.2,
                                           max = 1.0,
                                           step = 0.2,
                                           value = c(0.2,1.0)),
                               
                               sliderInput(inputId = "trainV",
                                           label = "Trainability Value:",
                                           min = 0.2,
                                           max = 1.0,
                                           step = 0.2,
                                           value = c(0.2,1.0))
                             ), 
                             mainPanel(tableOutput("summary"))
                           ))
                  ))
  )


# Define server logic ----
server <- function(input, output) {  
  filtered_data <- reactive({
    filtered<-data
    
    if (input$breeds !="All") {
      filtered <- filtered %>% filter(group %in% input$breeds)
    }
    
    filtered <- filtered %>%
      filter(mean_height >= input$mHeight[1] & mean_height <= input$mHeight[2],
             mean_weight >= input$mWeight[1] & mean_weight <= input$mWeight[2],
             mean_expectancy >= input$mExpectancy[1] & mean_expectancy <= input$mExpectancy[2],
             shedding_value >= input$shedV[1] & shedding_value <= input$shedV[2],
             energy_level_value >= input$energyV[1] & energy_level_value <= input$energyV[2],
             trainability_value >= input$trainV[1] & trainability_value <= input$trainV[2])
    
    return(filtered)
  })

  output$text <- renderText("*this sidebar panel is used for the 'Table' panel")
  
  output$summary <- renderTable ({filtered_data()})
  
  output$description <- renderText({
  })

  }
    
# Run the app ----
shinyApp(ui = ui, server = server)
library(tidyverse)
dogs <- read.csv("akc dogs.csv")

glimpse(dogs)

dogs2 <- dogs %>%
  mutate(mean_height = (min_height + max_height)/2) %>%
  mutate(mean_weight = (min_weight + max_weight)/2) %>%
  mutate(mean_expectancy = (min_expectancy + max_expectancy)/2)
glimpse(dogs2)

data <- dogs2 %>%
  select(breeds,popularity,mean_height,mean_weight,mean_expectancy,group,grooming_frequency_value,
          shedding_value,shedding_category,energy_level_value,energy_level_category,
          trainability_value,trainability_category,demeanor_value,demeanor_category)

# Define UI ----
ui <- fluidPage(
  titlePanel("What dog breed is best suited for me?"),
  
  sidebarLayout(
    sidebarPanel( 
      selectInput(inputId = "breeds",
                  label="Breed Group:",
                  choices = c("All", unique(data$group)),
                  multiple = TRUE),
    
    selectInput(inputId = "shed",
                label = "Shedding Category:",
                choices = c("All",unique(data$shedding_category)),
                multiple = TRUE
    ),
    
    selectInput(inputId = "energy",
                label = "Energy Level Category:",
                choices = c("All",unique(data$energy_level_category)),
                multiple = TRUE
    ),
    
    selectInput(inputId = "train",
                label = "Trainability Category:",
                choices = c("All",unique(data$trainability_category)),
                multiple = TRUE
    ),
    
    selectInput(inputId = "demeanor",
                label = "Demeanor Category:",
                choices = c("All",unique(data$demeanor_category)),
                multiple = TRUE
    ),
    
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
                value = c(min(data$mean_expectancy, na.rm = TRUE), max(data$mean_expectancy, na.rm = TRUE))),
    
    sliderInput(inputId="grv",
                label= "Grooming Frequency Value:",
                min = min(data$grooming_frequency_value, na.rm = TRUE),
                max = max(data$grooming_frequency_value, na.rm = TRUE),
                value = c(min(data$grooming_frequency_value, na.rm = TRUE),max(data$grooming_frequency_value, na.rm = TRUE)),
                step = 0.2),
    
    sliderInput(inputId="shedv",
                label="Shedding Value:",
                min = min(data$shedding_value, na.rm = TRUE),
                max = max(data$shedding_value, na.rm = TRUE),
                value = c(min(data$shedding_value, na.rm = TRUE),max(data$shedding_value, na.rm = TRUE)),
                step = 0.2),
    
    sliderInput(inputId="elv",
                label = "Energy Level Value:",
                min = min(data$energy_level_value, na.rm = TRUE),
                max = max(data$energy_level_value, na.rm = TRUE),
                value = c(min(data$energy_level_value, na.rm = TRUE),max(data$energy_level_value, na.rm = TRUE)),
                step = 0.2),
    
    sliderInput(inputId = "tv",
                label = "Trainability Value:",
                min = min(data$trainability_value, na.rm = TRUE),
                max = max(data$trainability_value, na.rm = TRUE),
                value = c(min(data$trainability_value, na.rm = TRUE), max(data$trainability_value, na.rm = TRUE)),
                step = 0.2),
    
    sliderInput(inputId = "dv",
                label = "Demeanor Value:",
                min = min(data$demeanor_value, na.rm = TRUE),
                max = max(data$demeanor_value, na.rm = TRUE),
                value = c(min(data$demeanor_value, na.rm = TRUE), max(data$demeanor_value, na.rm = TRUE)),
                step = 0.2)
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Descriptions", textOutput("description")),
                  tabPanel("Table", tableOutput("summary")),
                  tabPanel("Plot", plotOutput("plot"))))
  )
)


# Define server logic ----
server <- function(input, output) {  
  filtered_data <- reactive({
    filtered<-data
    
    if (input$breeds !="All") {
      filtered <- filtered %>% filter(group %in% input$breeds)
    }
    
    if (input$shed !="All") {
      filtered <- filtered %>% filter(shedding_category %in% input$shed)
    }
    
    if (input$train !="All") {
      filtered <- filtered %>% filter(trainability_category %in% input$train)
    }
    
    if (input$demeanor !="All") {
      filtered <- filtered %>% filter(demeanor_category %in% input$demeanor)
    }
    
    filtered <- filtered %>%
      filter(mean_height >= input$mHeight[1] & mean_height <= input$mHeight[2],
             mean_weight >= input$mWeight[1] & mean_weight <= input$mWeight[2],
             mean_expectancy >= input$mExpectancy[1] & mean_expectancy <= input$mExpectancy[2],
             grooming_frequency_value >= input$gfv[1] & grooming_frequency_value <= input$gfv[2],
             energy_level_value >= input$elv[1] & energy_level_value <= input$elv[2],
             trainability_value >= input$tv[1] & trainability_value <= input$tv[2],
             demeanor_level >= input$dv[1] & demeanor_level <=input$dv[2])
    
    return(filtered)
  })

  output$summary <- renderTable ({filtered_data()})
  
  output$description <- renderText({
   paste(h1("This panel shows the descriptions of the different filters and categories!"),
         br(),
         "Breeds: These are the different breeds of dogs available in the data set. It may not have every single dog however it does include the more common breeds."
         )
  })
  
  output$plot <- renderPlot({
    data %>% 
      select(trainability_value,energy_level_value) %>%
      ggplot(aes(x=trainability_value, y=energy_level_value)) + geom_density(na.rm = TRUE)
  })
  }
    
# Run the app ----
shinyApp(ui = ui, server = server)
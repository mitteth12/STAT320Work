library(shiny)
library(tidyverse)

games <- read_excel("Video Games.xlsx") %>%
    mutate(Evlump=fct_lump_n(Publisher, 30))

# Define the UI for the application
ui <- fluidPage(
    titlePanel("Analysis of Video Game Publishers 1980-2016"),
        mainPanel(
            plotOutput("q2plot")),
        sidebarPanel(sliderInput(inputId = "range1",
                                 label = "Year Range",
                                 min = 1980,
                                 max = 2016,
                                 value = c(1980, 2016),
                                 format = "####",
                                 sep = ""),
                     h2("Video Game Publishers"),
                                 uiOutput("evcheck"))
            
        )

    
# Define Server Logic
server <- function(input, output) {
    
    games.st <- reactive({games %>% mutate(Evlump=fct_lump_n(Publisher, 30))})
    
    
    output$q2plot <- renderPlot({
        games.st() %>% filter(Evlump %in% input$events) %>% 
            filter(Year >= input$range1[1] & 
                    Year <= input$range1[2]) %>%
        ggplot() +
            geom_line(aes(Year, Global_Sales, color=Publisher)) +
            ylim(0,85) +
            xlim(1980, 2016) +
            labs(y="Global Sales (in millions)")})
    
    output$evcheck <- renderUI(
        checkboxGroupInput(
            "events", "Choose Publisher(s)",
            choices = unique(games.st()$Publisher),
            selected = FALSE
        )
    )
   
}
# Run the application 
shinyApp(ui = ui, server = server)

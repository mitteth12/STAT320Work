library(shiny)
library(tidyverse)

games <- read_excel("Video Games.xlsx") %>%
    mutate(Evlump=fct_lump_n(Publisher, 30))

# Define the UI for the application
ui <- fluidPage(
    titlePanel("Analysis of Video Game Publishers 1980-2016"),
        mainPanel(
            plotOutput("q2plot")),
        sidebarPanel(sliderInput("year", "Year:", 1980, 2016, format = "####",
                                  value = c(1980,2016), sep=""), 
                    h2("Video Game Publishers"),
                    uiOutput("evcheck"),
)
            
        )

    
# Define Server Logic
server <- function(input, output, session) {
    
    movies <- reactive({
        minyear <- input$year[1]
        maxyear <- input$year[2]
        
    m <- games %>% filter(games$Year >= minyear, 
                             games$Year<= maxyear)})

    
    games.st <- reactive({games %>% mutate(Evlump=fct_lump_n(Publisher, 30))})
        
    
    
    
    
    output$q2plot <- renderPlot({
        games.st() %>% filter(Evlump %in% input$events) %>%
        ggplot() +
            geom_line(aes(input$year, Global_Sales, color=Publisher)) +
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

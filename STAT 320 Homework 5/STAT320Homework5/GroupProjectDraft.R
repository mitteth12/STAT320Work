library(shiny)
library(tidyverse)

games <- read_excel("Video Games.xlsx") 

# Define the UI for the application
ui <- fluidPage(
    titlePanel("Analysis of Video Game Publishers 1980-2016"),
        mainPanel(
            plotOutput("q2plot")),
        sidebarPanel( h2("Video Game Publishers"),
                                 selectInput("Publisher", "Select Publishers",
                                             choices=sort(unique(games$Publisher))))
            
        )

    
# Define Server Logic
server <- function(input, output) {
    
    games_st <- reactive(games %>% filter(Publisher==input$Publisher))
    
    
    output$q2plot <- renderPlot({
        games_st() %>%
        ggplot() +
            geom_line(aes(Year, Global_Sales)) +
            ylim(0,85) +
            xlim(1980, 2016) +
            labs(y="Global Sales (in millions)")})
   
}
# Run the application 
shinyApp(ui = ui, server = server)

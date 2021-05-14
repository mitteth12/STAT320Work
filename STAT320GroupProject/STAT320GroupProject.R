library(shiny)
library(tidyverse)

games <- read_csv("Video Games.xlsx - Data.csv")

# Define the UI for the application
ui <- fluidPage(
    titlePanel("Analysis of Publisher Sales"),
    mainPanel(
        h1("Difference Between Publishers"),
        plotOutput("q1plot"),
        selectInput("Publisher", "Select Publisher", choices = sort(unique(input$Publisher)))
        ,)
    )

# Define Server Logic
server <- function(input, output) {
    
    games_st <- reactive(games %>% filter(X==input$Publisher))
    
    output$q1plot <- renderPlot({
        games_st() %>%
            ggplot() +
            geom_line(aes(Year, Global_Sales)) +
            ylim(0,85) +
            xlim(1980, 2016) +
            labs(y="Amount of Sales (in Millions)")})
    
}


# Run the application 
shinyApp(ui = ui, server = server)

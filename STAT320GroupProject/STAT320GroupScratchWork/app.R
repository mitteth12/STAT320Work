library(shiny)
library(tidyverse)
library(forcats)


games <- read_csv("Video Games.xlsx - Data.csv")
myChoices <- unique(games$Publisher)
# Define the UI for the application
ui <- fluidPage(
    titlePanel("Map of Weather Events."),
    sidebarPanel(
    checkboxInput('all', 'Select All/None', value = TRUE),
    checkboxGroupInput("games", "Publishers", myChoices)),
    mainPanel(
        h1("Location of Weather Events"),
        plotOutput("q1plot")
        )
    )


# Define Server Logic
server <- function(input, output, session) {
    
    games_st <- reactive(games)
    
    observe({
        updateCheckboxGroupInput(
            session, "games", choices = myChoices, 
            selected = if(input$all) myChoices
        )
    })
    
    
    output$q1plot <- renderPlot({
        games_st() %>%
        ggplot() +
            geom_line(aes(Year, Global_Sales)) +
            ylim(0,85) +
            xlim(1980, 2016) +
            labs(y="Amount of Global Sales (in Millions)")})
    
    
}


# Run the application 
shinyApp(ui = ui, server = server)

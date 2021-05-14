library(shiny)
library(tidyverse)
library(readxl)

games <- read_excel("Video Games.xlsx") %>%
    mutate(Evlump=fct_lump_n(Publisher, 30))

# Define the UI for the application
ui <- fluidPage(navbarPage("Video Game App", 
    tabPanel("Plot",
    titlePanel("Analysis of Video Game Publishers 1980-2016"),
        mainPanel(tabsetPanel(tabPanel("Global Sales", plotOutput("q2plot")),
                               tabPanel("NA Sales", plotOutput("q3plot")),
                               tabPanel("EU Sales", plotOutput("q4plot")),
                               tabPanel("JP Sales", plotOutput("q5plot")))),
        sidebarPanel(sliderInput(inputId = "range1",
                                 label = "Year Range",
                                 min = 1980,
                                 max = 2016,
                                 value = c(1980, 2016),
                                 format = "####",
                                 sep = ""),
                     h2("Video Game Publishers"),
                                 uiOutput("evcheck"))
            
        ),
    tabPanel("Documentation",
             uiOutput("tab"))))

    
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
    
    output$q3plot <- renderPlot({
        games.st() %>% filter(Evlump %in% input$events) %>% 
            filter(Year >= input$range1[1] & 
                       Year <= input$range1[2]) %>%
            ggplot() +
            geom_line(aes(Year, NA_Sales, color=Publisher)) +
            ylim(0,50) +
            xlim(1980, 2016) +
            labs(y="North America Sales (in millions)")})
    
    output$q4plot <- renderPlot({
        games.st() %>% filter(Evlump %in% input$events) %>% 
            filter(Year >= input$range1[1] & 
                       Year <= input$range1[2]) %>%
            ggplot() +
            geom_line(aes(Year, EU_Sales, color=Publisher)) +
            ylim(0,50) +
            xlim(1980, 2016) +
            labs(y="Europe Sales (in millions)")})
    
    output$q5plot <- renderPlot({
        games.st() %>% filter(Evlump %in% input$events) %>% 
            filter(Year >= input$range1[1] & 
                       Year <= input$range1[2]) %>%
            ggplot() +
            geom_line(aes(Year, JP_Sales, color=Publisher)) +
            ylim(0,50) +
            xlim(1980, 2016) +
            labs(y="Japan Sales (in millions)")})
    
    output$evcheck <- renderUI(
        checkboxGroupInput(
            "events", "Choose Publisher(s)",
            choices = unique(games.st()$Publisher),
            selected = FALSE
        )
    )
    
    url <- a("App Documentation", href="https://docs.google.com/document/d/1gLuO9LY37WwCcqoyDynR6eWacihkfcc9wbWkTIyZ4pI/edit")
    output$tab <- renderUI({
        tagList("URL Link:", url)})
   
}
# Run the application 
shinyApp(ui = ui, server = server)

library(shiny)
library(tidyverse)

act <- read_csv("MO_ACT_06-14.csv")

# Define the UI for the application
ui <- fluidPage(
    titlePanel("Analysis of Video Game Publisher Sales"),
        mainPanel(
            h1("Difference Between Public and Private School"),
            plotOutput("q1plot"),
            actionButton("button", "Click"),
            h2("ACT Scores Over the Years"),
            plotOutput("q2plot"),
            h2("Select School"),
            selectInput("School", "Select School (Button Applies to Graph Above and Below)",
                        choices=sort(unique(act$X))),
            h3("Students with 33 or Above ACT Score Over the Years"),
            plotOutput("q3plot")
            
        )

    )
    
# Define Server Logic
server <- function(input, output) {
    
    act_st <- reactive(act %>% filter(X==input$School))
    
    output$q3plot <- renderPlot({
        act_st() %>%
            ggplot() +
            geom_line(aes(Year, Pct.33.36)) +
            ylim(0,1) +
            xlim(2006, 2014) +
            labs(y="Percentage of Students")})
    
    output$q2plot <- renderPlot({
        act_st() %>%
        ggplot() +
            geom_line(aes(Year, Ave.Act)) +
            ylim(0,36) +
            xlim(2006, 2014) +
            labs(y="Average ACT Score")})
   
    whichplot <- reactiveVal(TRUE)
    
    plot1 <-
                ggplot(act) +
                    geom_point(aes(Year, Ave.Act, color=Pub.Priv)) +
                    scale_color_manual(name="Type of University", values =alpha(c("black","grey60"),c(1,.3))) +
                    labs(y="Average ACT Score")
    
    plot2 <-
                ggplot(act) +
                    geom_point(aes(Year, Ave.Act, color=Pub.Priv)) +
                    scale_color_manual(name="Type of University", values =alpha(c("grey60","black"), c(.3,1))) +
                    labs(y="Average ACT Score")
    
    observeEvent(input$button, {
        whichplot(!whichplot())
    })
    
    which_graph <- reactive({
        if (whichplot()) {
            plot1
        } else {
            plot2
        }
    })
    
    output$q1plot <- renderPlot ({
        which_graph()
    })
    

    

 }
        

# Run the application 
shinyApp(ui = ui, server = server)

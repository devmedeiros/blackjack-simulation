library(shiny)
library(shinythemes)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    theme = shinytheme("sandstone"),
    
    # Application title
    titlePanel("Blackjack Simulation"),
    
    # Sidebar with a slider input for number of bins
    
    fluidRow(
        column(2,
               h4("Variable list"),
               checkboxGroupInput("archtype",
                                  "Choose which players archetypes to simulate",
                                  choiceNames = list("Newbie", "Cautious", "Strategist"),
                                  choiceValues = list("nb", "ct", "st"),
                                  selected = c("nb", "ct", "st")),
               sliderInput("decks",
                           "Number of decks used",
                           min = 2,
                           max = 8,
                           value = 8),
               numericInput("rounds", "How many rounds", value = 100),
               submitButton("run", "Run Simulation")
        ),
        column(10,
               tabsetPanel(
                   tabPanel("Plot", plotOutput("loseRate")),
                   tabPanel("Game Setup", dataTableOutput("text")),
                   tabPanel("Lose Rate", dataTableOutput("losRt"))
               )
        )
    )
))

#Define server logic
shinyServer(function(input, output) {
    
    source(file = "https://raw.githubusercontent.com/devmedeiros/blackjack-simulation/main/blackjack_functions.R", local = T)
    
    library(ggplot2)
    library(dplyr)
    library(reshape2)
    
    theme_set(theme_minimal())
    
    simulation <- reactive({
        play_multiple_rounds(input$decks, length(input$archtype), input$rounds, input$archtype)
    })
    
    output$text <- renderDataTable({
        simulation()[[2]]
    })
    
    output$losRt <- renderDataTable({
        round(data.frame('round' = simulation()$lost$round,
                   sapply(simulation()$lost[,-1],
                          function(x) cummean(x))), 3)
    })
    
    output$loseRate <- renderPlot({
        ggplot(data = melt(round(data.frame('round' = simulation()$lost$round,
                                            sapply(simulation()$lost[,-1],
                                                   function(x) cummean(x))), 3),
                           id = c('round')),
               aes(x = round,
                   y = value,
                   colour = variable)) + 
            geom_line(size = 1) + 
            ylim(0,1) +
            labs(x = "Round", y = "Lose Rate")
    })
    
})

library(shiny)

# Define server logic
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

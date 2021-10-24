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

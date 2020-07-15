#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(Cairo)
genre_list <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film.Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci.Fi", "Thriller", "War", "Western")

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("DSRC Movie Recommendation System"),
    fluidRow(
        
        column(4, h3("Please select the Genres You are interested today (order matters):"),
               wellPanel(
                   selectInput("input_genre", "I like this the most!",
                               genre_list),
                   selectInput("input_genre2", "I may like something like this too!",
                               genre_list),
                   selectInput("input_genre3", "If nothing else!",
                               genre_list)
               )),
        
        column(4, h3("Please select corresponding movies of the select Movies genres:"),
               wellPanel(
                   # This outputs the dynamic UI component
                   uiOutput("ui"),
                   uiOutput("ui2"),
                   uiOutput("ui3")
                   #submitButton("Get Recommendations")
               )),
        
        column(4,
               h3("Try the following movies:"),
               tableOutput("table")
        ))
)
)

#server.R
library(shiny)
library(proxy)
library(recommenderlab)
library(reshape2)
source("C:/Users/Dhruba/Desktop/GitHub/R - GitHub/helper.R")

search <- read.csv("C:/Users/Dhruba/Desktop/GitHub/R - GitHub/data/search.csv", stringsAsFactors=FALSE)
ratings <- read.csv("C:/Users/Dhruba/Desktop/GitHub/R - GitHub/data/ratings.csv", header = TRUE)
search <- search[-which((search$movieId %in% ratings$movieId) == FALSE),]

formatInput <- function(v,a,d){
    ## This function formats the user's input of Valence-Arousal-Dominance
    ## and outputs them as a vector
    c(v,a,d)
}





# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$ui <- renderUI({
        if (is.null(input$input_genre))
            return()
        
        switch(input$input_genre,
               "Action" = selectInput("select", "Movie of Genre you like the most",
                                      choices = sort(subset(search, Action == 1)$title),
                                      selected = sort(subset(search, Action == 1)$title)[1]),
               "Adventure" = selectInput("select", "Movie of Genre you like the most",
                                         choices = sort(subset(search, Adventure == 1)$title),
                                         selected = sort(subset(search, Adventure == 1)$title)[1]),
               "Animation" =  selectInput("select", "Movie of Genre you like the most",
                                          choices = sort(subset(search, Animation == 1)$title),
                                          selected = sort(subset(search, Animation == 1)$title)[1]),
               "Children" =  selectInput("select", "Movie of Genre you like the most",
                                         choices = sort(subset(search, Children == 1)$title),
                                         selected = sort(subset(search, Children == 1)$title)[1]),
               "Comedy" =  selectInput("select", "Movie of Genre you like the most",
                                       choices = sort(subset(search, Comedy == 1)$title),
                                       selected = sort(subset(search, Comedy == 1)$title)[1]),
               "Crime" =  selectInput("select", "Movie of Genre you like the most",
                                      choices = sort(subset(search, Crime == 1)$title),
                                      selected = sort(subset(search, Crime == 1)$title)[1]),
               "Documentary" =  selectInput("select", "Movie of Genre you like the most",
                                            choices = sort(subset(search, Documentary == 1)$title),
                                            selected = sort(subset(search, Documentary == 1)$title)[1]),
               "Drama" =  selectInput("select", "Movie of Genre you like the most",
                                      choices = sort(subset(search, Drama == 1)$title),
                                      selected = sort(subset(search, Drama == 1)$title)[1]),
               "Fantasy" =  selectInput("select", "Movie of Genre you like the most",
                                        choices = sort(subset(search, Fantasy == 1)$title),
                                        selected = sort(subset(search, Fantasy == 1)$title)[1]),
               "Film.Noir" =  selectInput("select", "Movie of Genre you like the most",
                                          choices = sort(subset(search, Film.Noir == 1)$title),
                                          selected = sort(subset(search, Film.Noir == 1)$title)[1]),
               "Horror" =  selectInput("select", "Movie of Genre you like the most",
                                       choices = sort(subset(search, Horror == 1)$title),
                                       selected = sort(subset(search, Horror == 1)$title)[1]),
               "Musical" =  selectInput("select", "Movie of Genre you like the most",
                                        choices = sort(subset(search, Musical == 1)$title),
                                        selected = sort(subset(search, Musical == 1)$title)[1]),
               "Mystery" =  selectInput("select", "Movie of Genre you like the most",
                                        choices = sort(subset(search, Mystery == 1)$title),
                                        selected = sort(subset(search, Mystery == 1)$title)[1]),
               "Romance" =  selectInput("select", "Movie of Genre you like the most",
                                        choices = sort(subset(search, Romance == 1)$title),
                                        selected = sort(subset(search, Romance == 1)$title)[1]),
               "Sci.Fi" =  selectInput("select", "Movie of Genre you like the most",
                                       choices = sort(subset(search, Sci.Fi == 1)$title),
                                       selected = sort(subset(search, Sci.Fi == 1)$title)[1]),
               "Thriller" =  selectInput("select", "Movie of Genre you like the most",
                                         choices = sort(subset(search, Thriller == 1)$title),
                                         selected = sort(subset(search, Thriller == 1)$title)[1]),
               "War" =  selectInput("select", "Movie of Genre you like the most",
                                    choices = sort(subset(search, War == 1)$title),
                                    selected = sort(subset(search, War == 1)$title)[1]),
               "Western" = selectInput("select", "Movie of Genre you like the most",
                                       choices = sort(subset(search, Western == 1)$title),
                                       selected = sort(subset(search, Western == 1)$title)[1])
        )
    })
    
    output$ui2 <- renderUI({
        if (is.null(input$input_genre2))
            return()
        
        switch(input$input_genre2,
               "Action" = selectInput("select2", "Movie of Genre you may like too",
                                      choices = sort(subset(search, Action == 1)$title),
                                      selected = sort(subset(search, Action == 1)$title)[1]),
               "Adventure" = selectInput("select2", "Movie of Genre  you may like too",
                                         choices = sort(subset(search, Adventure == 1)$title),
                                         selected = sort(subset(search, Adventure == 1)$title)[1]),
               "Animation" =  selectInput("select2", "Movie of Genre you may like too",
                                          choices = sort(subset(search, Animation == 1)$title),
                                          selected = sort(subset(search, Animation == 1)$title)[1]),
               "Children" =  selectInput("select2", "Movie of Genre you may like too",
                                         choices = sort(subset(search, Children == 1)$title),
                                         selected = sort(subset(search, Children == 1)$title)[1]),
               "Comedy" =  selectInput("select2", "Movie of Genre you may like too",
                                       choices = sort(subset(search, Comedy == 1)$title),
                                       selected = sort(subset(search, Comedy == 1)$title)[1]),
               "Crime" =  selectInput("select2", "Movie of Genre you may like too",
                                      choices = sort(subset(search, Crime == 1)$title),
                                      selected = sort(subset(search, Crime == 1)$title)[1]),
               "Documentary" =  selectInput("select2", "Movie of Genre you may like too",
                                            choices = sort(subset(search, Documentary == 1)$title),
                                            selected = sort(subset(search, Documentary == 1)$title)[1]),
               "Drama" =  selectInput("select2", "Movie of Genre you may like too",
                                      choices = sort(subset(search, Drama == 1)$title),
                                      selected = sort(subset(search, Drama == 1)$title)[1]),
               "Fantasy" =  selectInput("select2", "Movie of Genre you may like too",
                                        choices = sort(subset(search, Fantasy == 1)$title),
                                        selected = sort(subset(search, Fantasy == 1)$title)[1]),
               "Film.Noir" =  selectInput("select2", "Movie of Genre you may like too",
                                          choices = sort(subset(search, Film.Noir == 1)$title),
                                          selected = sort(subset(search, Film.Noir == 1)$title)[1]),
               "Horror" =  selectInput("select2", "Movie of Genre you may like too",
                                       choices = sort(subset(search, Horror == 1)$title),
                                       selected = sort(subset(search, Horror == 1)$title)[1]),
               "Musical" =  selectInput("select2", "Movie of Genre you may like too",
                                        choices = sort(subset(search, Musical == 1)$title),
                                        selected = sort(subset(search, Musical == 1)$title)[1]),
               "Mystery" =  selectInput("select2", "Movie of Genre you may like too",
                                        choices = sort(subset(search, Mystery == 1)$title),
                                        selected = sort(subset(search, Mystery == 1)$title)[1]),
               "Romance" =  selectInput("select2", "Movie of Genre you may like too",
                                        choices = sort(subset(search, Romance == 1)$title),
                                        selected = sort(subset(search, Romance == 1)$title)[1]),
               "Sci.Fi" =  selectInput("select2", "Movie of Genre you may like too",
                                       choices = sort(subset(search, Sci.Fi == 1)$title),
                                       selected = sort(subset(search, Sci.Fi == 1)$title)[1]),
               "Thriller" =  selectInput("select2", "Movie of Genre you may like too",
                                         choices = sort(subset(search, Thriller == 1)$title),
                                         selected = sort(subset(search, Thriller == 1)$title)[1]),
               "War" =  selectInput("select2", "Movie of Genre you may like too",
                                    choices = sort(subset(search, War == 1)$title),
                                    selected = sort(subset(search, War == 1)$title)[1]),
               "Western" = selectInput("select2", "Movie of Genre you may like too",
                                       choices = sort(subset(search, Western == 1)$title),
                                       selected = sort(subset(search, Western == 1)$title)[1])
        )
    })
    
    output$ui3 <- renderUI({
        if (is.null(input$input_genre3))
            return()
        
        switch(input$input_genre3,
               "Action" = selectInput("select3", "Movie of Genre if nothing else",
                                      choices = sort(subset(search, Action == 1)$title),
                                      selected = sort(subset(search, Action == 1)$title)[1]),
               "Adventure" = selectInput("select3", "Movie of Genre if nothing else",
                                         choices = sort(subset(search, Adventure == 1)$title),
                                         selected = sort(subset(search, Adventure == 1)$title)[1]),
               "Animation" =  selectInput("select3", "Movie of Genre if nothing else",
                                          choices = sort(subset(search, Animation == 1)$title),
                                          selected = sort(subset(search, Animation == 1)$title)[1]),
               "Children" =  selectInput("select3", "Movie of Genre if nothing else",
                                         choices = sort(subset(search, Children == 1)$title),
                                         selected = sort(subset(search, Children == 1)$title)[1]),
               "Comedy" =  selectInput("select3", "Movie of Genre if nothing else",
                                       choices = sort(subset(search, Comedy == 1)$title),
                                       selected = sort(subset(search, Comedy == 1)$title)[1]),
               "Crime" =  selectInput("select3", "Movie of Genre if nothing else",
                                      choices = sort(subset(search, Crime == 1)$title),
                                      selected = sort(subset(search, Crime == 1)$title)[1]),
               "Documentary" =  selectInput("select3", "Movie of Genre if nothing else",
                                            choices = sort(subset(search, Documentary == 1)$title),
                                            selected = sort(subset(search, Documentary == 1)$title)[1]),
               "Drama" =  selectInput("select3", "Movie of Genre if nothing else",
                                      choices = sort(subset(search, Drama == 1)$title),
                                      selected = sort(subset(search, Drama == 1)$title)[1]),
               "Fantasy" =  selectInput("select3", "Movie of Genre if nothing else",
                                        choices = sort(subset(search, Fantasy == 1)$title),
                                        selected = sort(subset(search, Fantasy == 1)$title)[1]),
               "Film.Noir" =  selectInput("select3", "Movie of Genre if nothing else",
                                          choices = sort(subset(search, Film.Noir == 1)$title),
                                          selected = sort(subset(search, Film.Noir == 1)$title)[1]),
               "Horror" =  selectInput("select3", "Movie of Genre if nothing else",
                                       choices = sort(subset(search, Horror == 1)$title),
                                       selected = sort(subset(search, Horror == 1)$title)[1]),
               "Musical" =  selectInput("select3", "Movie of Genre if nothing else",
                                        choices = sort(subset(search, Musical == 1)$title),
                                        selected = sort(subset(search, Musical == 1)$title)[1]),
               "Mystery" =  selectInput("select3", "Movie of Genre if nothing else",
                                        choices = sort(subset(search, Mystery == 1)$title),
                                        selected = sort(subset(search, Mystery == 1)$title)[1]),
               "Romance" =  selectInput("select3", "Movie of Genre if nothing else",
                                        choices = sort(subset(search, Romance == 1)$title),
                                        selected = sort(subset(search, Romance == 1)$title)[1]),
               "Sci.Fi" =  selectInput("select3", "Movie of Genre if nothing else",
                                       choices = sort(subset(search, Sci.Fi == 1)$title),
                                       selected = sort(subset(search, Sci.Fi == 1)$title)[1]),
               "Thriller" =  selectInput("select3", "Movie of Genre if nothing else",
                                         choices = sort(subset(search, Thriller == 1)$title),
                                         selected = sort(subset(search, Thriller == 1)$title)[1]),
               "War" =  selectInput("select3", "Movie of Genre if nothing else",
                                    choices = sort(subset(search, War == 1)$title),
                                    selected = sort(subset(search, War == 1)$title)[1]),
               "Western" = selectInput("select3", "Movie of Genre if nothing else",
                                       choices = sort(subset(search, Western == 1)$title),
                                       selected = sort(subset(search, Western == 1)$title)[1])
        )
    })
    
    output$table <- renderTable({
        movie_recommendation(input$select, input$select2, input$select3)
    })
    
    output$dynamic_value <- renderPrint({
        c(input$select,input$select2,input$select3)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

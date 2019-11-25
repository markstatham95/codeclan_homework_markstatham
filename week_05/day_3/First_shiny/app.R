#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(CodeClanData)
library(ggthemes)

all_teams <- unique(olympics_overall_medals$team)

source("function.R")
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("Title"), 
  
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = "season", 
        label   = "Summer or Winter Olympics?", 
        choices = c("Summer", "Winter")
      ), 
      selectInput(
        inputId = "team", 
        label   = "Choose your team", 
        choices = all_teams
      )
    ), 
    
    mainPanel(
      plotOutput("top_5_teams_plot")
    )
  )
)
   


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  
  output$top_5_teams_plot <- renderPlot({
  
  
    
    olympics_overall_medals %>%
      filter(team  == input$team) %>%
      filter(season == input$season) %>%
      ggplot() +
      aes(x = medal, y =  count, fill = medal) + 
      geom_col() +  
      scale_fill_manual(values = c("Gold", "Grey69","Peru")) +  
      theme_few()
    
    #my_function(input$team, input$season)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


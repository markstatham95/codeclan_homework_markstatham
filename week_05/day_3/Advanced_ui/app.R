library(shiny)
library(dplyr)
library(ggplot2)
library(CodeClanData)
all_teams <- unique(olympics_overall_medals$team)
ui <- fluidPage(
  titlePanel(tags$i("Olympic Medals")),
  sidebarLayout(
    sidebarPanel(
      radioButtons("season",
                   "Summer or Winter Olympics?",
                   choices = c("Summer", "Winter")
      ),
      selectInput("team",
                  "Which Team?",
                  choices = all_teams,
                  selected = "Great Britain"
      )
    ),
    mainPanel(
      plotOutput("medal_plot")
    )
  )
)
server <- function(input, output) {
  output$medal_plot <- renderPlot({
    olympics_overall_medals %>%
      filter(team == input$team) %>%
      filter(season == input$season) %>%
      ggplot() +
      aes(x = medal, y = count, fill = medal) +
      geom_col() + 
      
  })
}
shinyApp(ui = ui, server = server)
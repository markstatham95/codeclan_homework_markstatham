

library(shiny)
library(tidyverse)
library(CodeClanData)
library(ggthemes)



ui <- fluidPage(
   
   titlePanel("Five Country Medal Comparison"),
   
   sidebarLayout(
        sidebarPanel(
          radioButtons(
            inputId = "season", 
            label   = "Summer or Winter Olympics?", 
            choices = c("Summer", "Winter")
          ), 
          radioButtons(
            inputId = "medal", 
            label   = "Choose your medal", 
            choices = c("Bronze", "Silver", "Gold")
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
      filter(team %in% c("United States",
                         "Soviet Union",
                         "Germany",
                         "Italy",
                         "Great Britain")) %>%
      filter(medal == input$medal) %>%
      filter(season == input$season) %>%
      ggplot() +
      aes(x = team, y = count) +
      geom_col(fill = case_when(input$medal == "Gold"   ~ "Gold", 
                                input$medal == "Silver" ~ "Grey69", 
                                input$medal == "Bronze" ~ "Peru")) +
      theme_few()
  })
}

shinyApp(ui = ui, server = server)




library(shiny)
library(tidyverse)
library(ggthemes)
library(CodeClanData)


ui <- fluidPage(
   

   titlePanel("Stacked Bar Chart of Olympics Medals"),
   
   sidebarLayout(
      sidebarPanel(
        radioButtons(
          inputId = "season", 
          label   = "Summer or Winter Olympics?", 
          choices = c("Summer", "Winter")
        )
      ),
      
      mainPanel(
         plotOutput("medal_stack_plot")
      )
   )
)

server <- function(input, output){
  
  output$medal_stack_plot <- renderPlot({

    olympics_overall_medals %>%
    filter(team %in% c("United States",
                       "Soviet Union",
                       "Germany",
                       "Italy",
                       "Great Britain")) %>%
    filter(season == input$season) %>%
    ggplot() + 
    aes(x = team, y = count, fill = medal) + 
    geom_col(position = "stack") +
    scale_fill_manual(values = c("Gold", "Grey69", "Peru")) +
    theme_few()

  })
}

# Run the application 
shinyApp(ui = ui, server = server)


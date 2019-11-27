library(CodeClanData)
library(tidyverse)
library(shiny)
library(shinythemes)

source("data_wrangling_and_plots.R")


ui <- fluidPage(
  theme = shinytheme("cerulean"), 
  titlePanel("The Big Three Gaming Contenders: 1988 - 2016"),
  


  fluidRow(column(4, 
    
      selectInput("year",
        "Which year are you interested in?",
        choices = unique(game_sales$year_of_release)
                  )
      ),
          column(4, 
        
      selectInput("genre",
        "Choose the genre you are interested in",
        choices = unique(game_sales$genre)
      )
      ), 
          column(2, 
      radioButtons("big_three_rating",
        "Choose console producer",
        choices = unique(game_sales$big_three)
      )
      ),
      column(2, 
             actionButton("add_line", 
                          "Add a regression line?")), 

  
  column(4, 
    plotOutput("yearPlot")
    ),
  column(4, 
    plotOutput("genrePlot")
    ),
  column(4, 
    plotOutput("big_three_ratingPlot")
  )


)
)



server <- function(input, output) {
  
 #this plot compares sales of games in each year from the big three "microsoft, ninetndo and sony" 
  output$yearPlot <- renderPlot({
    game_sales %>%
      filter(year_of_release == input$year) %>%
      ggplot() +
      aes(x = big_three, y = sales, fill = big_three) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("limegreen", "Grey78", "darkslateblue")) +
      theme_clean() +
      labs(
        title = "Total Sales of games for big three consoles",
        subtitle = input$year
      )
  })
  
#this plot shows the sales for each of the big three across the years in the data(excluding the earliest years, as there was v little data for them)
# gives an insight into how each of the big three gets sales in different genres across the years covered in the data 
  output$genrePlot <- renderPlot({
    game_sales %>%
      filter(year_of_release > 1998) %>%
      filter(genre == input$genre) %>%
      ggplot() +
      aes(x = year_of_release, y = sales, fill = big_three) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("limegreen", "Grey78", "darkslateblue")) +
      theme_clean()
  })

#this plot shows the relationship between user ratings and critic ratings of games, for each of the three console providers - generally a positive relationship 
  # hopefully users would be able to see how sony games had fewer bad ratings 

  output$big_three_ratingPlot <- renderPlot({
    game_sales %>%
      filter(year_of_release > 1998) %>%
      filter(big_three == input$big_three_rating) %>%
      ggplot() +
      aes(x = user_score, y = critic_score) +
      geom_point(shape = "circle", alpha = 0.95, colour = "limegreen") +
      theme_clean()
  })
}

# Run the application
shinyApp(ui = ui, server = server)

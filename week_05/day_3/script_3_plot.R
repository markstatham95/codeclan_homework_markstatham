library(shiny)
library(tidyverse)
library(CodeClanData)

head(olympics_overall_medals)

output$medal_plot <-  renderPlot ({olympics_overall_medals %>%
  filter(team  == "Great Britain") %>%
  filter(season == "Summer") %>% 
  ggplot()+  
  aes(x = medal, y =  count, fill = medal) + 
  geom_col() +  
    scale_fill_manual(values = c("Gold", "Grey69","Peru"))
})


olympics_overall_medals %>%
  filter(medal == "Gold") %>%
  filter(season == "Summer") %>%
  arrange(desc(count)) %>%
  slice(1:5) %>%
  ggplot() +
  aes(x= team, y = count) +
  geom_col() + 
  coord_flip() + 
  labs(
    title =  "Top  5 teams for Gold Medals")
    
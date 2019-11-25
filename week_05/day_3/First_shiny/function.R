
my_function <- function(teamin, seasonin) {
  browser()
olympics_overall_medals %>%
    filter(team  == teamin) %>%
    filter(season == seasonin) %>%
    ggplot() +
    aes(x = medal, y =  count, fill = medal) + 
    geom_col() +  
    scale_fill_manual(values = c("Gold", "Grey69","Peru")) +  
    theme_few()

}

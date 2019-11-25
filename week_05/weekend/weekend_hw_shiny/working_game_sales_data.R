library(tidyverse)
library(CodeClanData)


view(game_sales) 

dim(game_sales)

names(game_sales)

game_sales %>%
  group_by(developer) %>%
  drop_na() %>%
  summarise(mean(sales, na.rm = TRUE)) 

str(game_sales)

min(game_sales$year_of_release)
hist(game_sales$year_of_release)

#comparing sales of different ps consoles over time 
## -- would be good to make a geom_area  

game_sales %>%
  filter(publisher == )
  filter(str_detect(platform, "PS+")) %>%
  ggplot + 
  aes(x = year_of_release, y = sales, fill = platform) + 
  geom_col()

game_sales %>%
  filter(str_detect(platform, "PS+")) %>%
  ggplot + 
  aes(x = year_of_release, y = sales, colour = platform, group = platform) + 
  geom_line()

game_sales %>%
  filter(str_detect(platform, "X+")) %>%
  ggplot + 
  aes(x = year_of_release, y = sales, fill = platform) + 
  geom_col()

game_sales %>%
  filter(platform %in% c("Wii", "WiiU",  "DS",  "3DS",  "GBA")) %>%
  ggplot + 
  aes(x = year_of_release, y = sales, fill = platform) + 
  geom_col() + 
  ggtitle("Nintendo Based Games Sales over time")


game_sales %>%
  filter(platform %in% c("PS", "PSP",  "PS2",  "PS3",  "PS4", "PSV")) %>%
  ggplot + 
  aes(x = year_of_release, y = sales, fill = platform) + 
  geom_col() + 
  ggtitle("Sony Based Games Sales over time")

game_sales %>%
  ggplot + 
  aes(x = year_of_release, y = sales, fill = developer) + 
  geom_col() + 
  ggtitle("Sales by developer")

game_sales %>%
  filter(genre == "Sports") %>%
  ggplot + 
  aes(x = year_of_release, y = sales, fill = developer) + 
  geom_col() + 
  ggtitle("Sales of Sports games by developer")

game_sales %>%
  filter(genre == "Racing") %>%
  ggplot + 
  aes(x = year_of_release, y = sales, fill = developer) + 
  geom_col() + 
  ggtitle("Sales of Racing games by developer")

game_sales %>%
  filter(genre == "Misc") %>%
  ggplot + 
  aes(x = year_of_release, y = sales, fill = developer) + 
  geom_col() + 
  ggtitle("Sales of Misc games by developer")

game_sales %>%
  filter(genre == "Shooter") %>%
  ggplot + 
  aes(x = year_of_release, y = sales, fill = platform) + 
  geom_col() + 
  ggtitle("Sales of Shooter games by developer")

game_sales %>%
  group_by(platform) %>%
  summarise(n())

game_sales %>%
  group_by(genre) %>%
  summarise(n())

game_sales %>%
  group_by(publisher) %>%
  summarise(n())

game_sales %>%
  ggplot + 
  aes(x = year_of_release, y = sales, fill = genre) + 
  geom_col()


game_sales %>%
  ggplot + 
  aes(x = year_of_release, y = sales, fill = developer ) + 
  geom_col()

game_sales %>%
  filter(str_detect(platform, "PS+")) %>%
  ggplot() + 
  aes(x = year_of_release, y = sales, fill = platform) + 
  geom_col()

#relationship between user ratings and critics ratings for playsation games 

game_sales %>%
  filter(str_detect(platform, "PS+")) %>%
  ggplot + 
  aes(x = user_score, y = critic_score) + 
  geom_point(shape = "square", alpha = 0.75, colour = "coral") + 
  geom_smooth(method = "lm")

#same thing for wii games 

game_sales %>%
  filter(str_detect(platform, "W")) %>%
  ggplot + 
  aes(x = user_score, y = critic_score) + 
  geom_point(shape = "square", alpha = 0.75, colour = "coral") + 
  geom_smooth(method = "lm")



game_sales %>%
  filter(str_detect(platform, "X")) %>%
  filter(year_of_release == 2006) %>%
  ggplot + 
  aes(x = user_score, y = critic_score, size = sales) + 
  geom_point(shape = "circle", alpha = 0.75, colour = "coral") + 
  scale_size(range = c(1, 30), name="Sales (M)")

game_sales %>%
  ggplot + 
  aes(x = user_score, y = critic_score) + 
  geom_point(shape = "square", alpha = 0.75, colour = "coral") + 
  geom_smooth(method = "lm")


game_sales %>%
  filter(str_detect(platform, "PS+")) %>%
  ggplot + 
  aes(x = critic_score, y = sales) + 
  geom_point(shape = "square", alpha = 0.75, colour = "coral") 


game_sales %>%
  filter(year_of_release == 2010) %>%
  ggplot() + 
  aes(x = publisher, y = sales) + 
  geom_col()

game_sales_console <- game_sales %>%
  mutate(console = str_replace_all(platform, "PS+", "Sony"),
         console = str_replace_all(platform, "Wi+", "Nintendo"), 
         console = str_replace_all(platform, "X+", "Microsoft"), 
         console = str_replace_all(platform, "G+", "Nintendo"), 
         console = if_else(platform == "3DS", "Nintendo", console) 
         )

table(game_sales$platform)



table(game_sales$developer)

## I want to group the platform column into Sony, Microsoft, Nintendo 


games_sales <- game_sales %>%
  mutate(big_three = case_when(platform == "3DS"   ~ "Nintendo", 
                                      platform == "DS"    ~ "Nintendo",
                                      platform == "GBA"   ~ "Nintendo",
                                      platform == "GC"    ~ "Nintendo", 
                                      platform == "PC"    ~ "Microsoft", 
                                      platform == "PS"    ~ "Sony",
                                      platform == "PS2"   ~ "Sony", 
                                      platform == "PS3"   ~ "Sony", 
                                      platform == "PS4"   ~ "Sony", 
                                      platform == "PSP"   ~ "Sony", 
                                      platform == "PSV"   ~ "Sony", 
                                      platform == "Wii"   ~ "Nintendo", 
                                      platform == "WiiU"  ~ "Nintendo", 
                                      platform == "X360"  ~ "Microsoft", 
                                      platform == "XB"    ~ "Microsoft", 
                                      platform == "XOne"  ~ "Microsoft"))


table(games_sales$big_three)


view(games_sales)
games_sales_test %>%
  filter(year_of_release > 1995) %>%
ggplot() + 
  aes(x = year_of_release, y = sales, fill = platform_grouped) + 
  geom_col()


games_sales_test %>%
  filter(year_of_release > 1995) %>%
  ggplot() + 
  aes(x = year_of_release, y = sales, fill = platform_grouped) + 
  geom_col()


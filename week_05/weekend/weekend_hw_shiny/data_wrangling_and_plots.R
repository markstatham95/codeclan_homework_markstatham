library(CodeClanData)
library(tidyverse)
library(ggthemes)




#A bit of data wrangling - I want a grouping column for the big three console makers

game_sales <- game_sales %>%
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

game_sales %>%
  filter(year_of_release == 2007) %>%
  ggplot + 
  aes(x = big_three, y = sales, fill = big_three) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("limegreen", "Grey78", "darkslateblue")) + 
  theme_clean() + 
  labs(title = "Total Sales of games for big three consoles", 
       subtitle = "2007")


game_sales %>%
  filter(year_of_release > 1998) %>%
  filter(genre == "Sports") %>%
  ggplot + 
  aes(x = year_of_release, y = sales, fill = big_three) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("limegreen", "Grey78", "darkslateblue")) + 
  theme_clean()

game_sales %>%
  filter(year_of_release > 2001) %>%
  ggplot + 
  aes(x = user_score, y = critic_score, colour = big_three) + 
  geom_point(shape = "circle", alpha = 0.95) + 
  scale_colour_manual(values = c("limegreen", "Grey78", "darkslateblue")) + 
  theme_clean()

# users vs critics for microsoft console games

game_sales %>%
  filter(year_of_release > 2001) %>%
  filter(big_three == "Microsoft") %>%
  ggplot + 
  aes(x = user_score, y = critic_score) + 
  geom_point(shape = "circle", alpha = 0.95, colour = "limegreen") + 
  theme_clean()


# users vs critics for nintendo console games


game_sales %>%
  filter(year_of_release > 2001) %>%
  filter(big_three == "Nintendo") %>%
  ggplot + 
  aes(x = user_score, y = critic_score) + 
  geom_point(shape = "circle", alpha = 0.95, colour = "grey78") + 
  theme_clean()


# users vs critics for sony console games


game_sales %>%
  filter(year_of_release > 2001) %>%
  filter(big_three == "Sony") %>%
  ggplot + 
  aes(x = user_score, y = critic_score) + 
  geom_point(shape = "circle", alpha = 0.95, colour = "darkslateblue") + 
  theme_clean()








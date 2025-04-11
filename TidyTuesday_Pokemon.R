#Tidy Tuesday R Users Group

#load packages
library(tidyverse)
library(ggimage)
library(patchwork)

#load the data
pokemon_df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-04-01/pokemon_df.csv')



#do different primary types have different hp, attack, and defense levels

ggplot(data = pokemon_df, aes(x = type_1, y = hp)) +
  geom_jitter(alpha = 0.75) +
  geom_violin(alpha = 0.5)

#do they get stronger on average as you go up in generation?
ggplot(data = pokemon_df, aes(x = as.factor(generation_id), y = hp)) +
  geom_jitter(alpha = 0.75) +
  geom_violin(alpha = 0.5)

summary(aov(hp ~ as.factor(generation_id), data = pokemon_df))

#which pikachu is the best?
pikachu <- pokemon_df %>% 
  filter(grepl('pikachu', pokemon)) # there are all the same...


#of the OG three... what are the stats
OG3 <- pokemon_df[1:9,]
OG3$evolution <- rep(1:3, 3)

OG3 <- OG3 %>% select(pokemon, height, weight, base_experience, 
               hp, attack, defense, special_attack, special_defense, speed,evolution,
               color_1, url_image) 

OG3[,2:10] <- scale(OG3[,2:10])

OG3_long <- OG3 %>% pivot_longer(cols = height:speed)

OG3_long$name <- factor(OG3_long$name, 
                           levels = c("height", "weight", "speed",
                                      "base_experience", "hp", "attack", "defense",
                                      "special_attack", "special_defense"),
                           labels = c("Height", "Weight", "Speed",
                                      "Base Experience", "Hit Points", "Attack", "Defense",
                                      "Special Attack", "Special Defense")
                           ) 
OG3_long$pokemon <- factor(OG3_long$pokemon, 
                           levels = c("bulbasaur", "charmander", "squirtle",
                                      "ivysaur", "charmeleon", "wartortle",
                                      "venusaur", "charizard", "blastoise"),
                           labels = c("Bulbasaur", "Charmander", "Squirtle",
                                      "Ivysaur", "Charmeleon", "Wartortle",
                                      "Venusaur", "Charizard", "Blastoise"))


g1 <- ggplot(filter(OG3_long, evolution == 1) , aes(x = name, y = pokemon,
                                              size = value,
                                              color = color_1)) +
  geom_image(aes(image = url_image)) +
  geom_vline(xintercept = c(3.5, 7.5)) +
  scale_color_identity() +
  scale_size_continuous(limits = range(OG3_long$value),
                        range = c(0.05,0.3)) +
  labs(x = "", y = expression(bold("First Evolution"))) +
  scale_x_discrete(position = "top") +
  theme_bw() +
  theme(legend.position = "none")

g2 <- ggplot(filter(OG3_long, evolution == 2) , aes(x = name, y = pokemon,
                                                    size = value,
                                                    color = color_1)) +
  geom_image(aes(image = url_image)) +
  geom_vline(xintercept = c(3.5, 7.5)) +
  scale_size_continuous(limits = range(OG3_long$value),
                        range = c(0.05,0.3)) +
  scale_color_identity() +
  labs(x = "", y = expression(bold("Second Evolution"))) +
  scale_x_discrete(position = "top") +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

g3 <- ggplot(filter(OG3_long, evolution == 3) , aes(x = name, y = pokemon,
                                                    size = value,
                                                    color = color_1)) +
  geom_image(aes(image = url_image)) +
  geom_vline(xintercept = c(3.5, 7.5)) +
  scale_size_continuous(limits = range(OG3_long$value),
                        range = c(0.05,0.3)) +
  scale_color_identity() +
  labs(x = "", y = expression(bold("Third Evolution"))) +
  scale_x_discrete(position = "top") +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


combo <- g1/g2/g3 + plot_annotation(
  title = expression(bold("Stats of the OG Three")),
  theme = theme(plot.title = element_text(size = 24,
                                          hjust = 0.5))
)



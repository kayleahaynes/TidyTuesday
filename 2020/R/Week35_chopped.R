# description of script --------------------------------------------------------
# TidyTuesday 25th August 2020 - week 35
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-08-25/readme.md
#
# inputs - : chopped.tsv
#
# outputs - a menu of items sampled from the food options on Chopped

# set up -----------------------------------------------------------------------
message("Set up the script")

# dependencies
library(tidyverse)
library(tidytext)
library(ggtext)
library(ggforce)
library(stringi)


# load data --------------------------------------------------------------------
message("Load data")

df_chopped <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-25/chopped.tsv')

nrow(df_chopped) # there are 569 episodes (thus combinations of food)

# transformations --------------------------------------------------------------
message("Transform data")

# split out the ingredients in the appetizers, entrees and desserts
appetizers <- df_chopped %>%
  select(season, season_episode, series_episode, episode_rating, appetizer) %>%
  separate_rows(appetizer, sep=", ")

entrees <- df_chopped %>%
  select(season, season_episode, series_episode, episode_rating, entree) %>%
  separate_rows(entree, sep=", ")

desserts <- df_chopped %>%
  select(season, season_episode, series_episode, episode_rating, dessert) %>%
  separate_rows(dessert, sep=", ")

# EDA --------------------------------------------------------------------------
message("EDA")

# count how many times a food item appears in the list of appetizers

appetizers_count <- appetizers %>%
  count(appetizer, sort = TRUE)

nrow(appetizers_count) #1701 unique items in appetizers, 313 used more than once
nrow(appetizers_count %>% filter(n > 1))

appetizers %>%
  group_by(appetizer) %>%
  mutate(count_item = n()) %>%
  group_by(season, season_episode) %>%
  mutate(min_count = min(count_item))

# average number of food items found in the appetizers
appetizers %>%
  group_by(season, season_episode) %>%
  count() %>%
  ungroup() %>%
  summarise(mean(n), median(n))


entrees_count <- entrees %>%
  count(entree, sort = TRUE)

entrees %>%
  group_by(season, season_episode) %>%
  count() %>%
  ungroup() %>%
  summarise(mean(n), median(n))

nrow(entrees_count) #1666 unique items in entrees, 312 used more than once
nrow(entrees_count %>% filter(n > 1))

dessert_count <- desserts %>%
  count(dessert, sort = TRUE)

desserts %>%
  group_by(season, season_episode) %>%
  count() %>%
  ungroup() %>%
  summarise(mean(n), median(n))

nrow(dessert_count) #1652 unique items in appetizers, 319 used more than once
nrow(dessert_count %>% filter(n > 1))

# function + plot --------------------------------------------------------------

# This function samples 4 food items for each course randomly and then outputs a menu with these choices.
create_menu <- function(seed = 12){

  set.seed(seed)

  # sample 4 food items for each course. The weight is given as the number of episodes a food items has been used to try and avoid having 4 completely obscure items together.
  # With more time ideally you'd want some more information about the different food items to try and make more sensible choices. For example 1 meat, 2 veg, 1 sauce etc.
  choose_appetizers <- sample_n(appetizers_count, 4, FALSE, weight = appetizers_count$n)
  choose_entrees <- sample_n(entrees_count, 4, FALSE, weight = entrees_count$n)
  choose_desserts <- sample_n(dessert_count, 4, FALSE, weight = dessert_count$n)

  menu_plot <- ggplot() +
    coord_fixed(xlim = c(-35, 35), ylim = c(0,100), clip = "off") +
    theme_void() +
    # side lines
    geom_line(data = data.frame(x = c(-34,-34), y = c(5,95)), aes(x = x, y = y), color = "brown4", size = 1) +
    geom_line(data = data.frame(x = c(34,34), y = c(5,95)), aes(x = x, y = y), color = "brown4", size = 1) +
    # top decoration
    geom_line(data = data.frame(x = c(-34,-28), y = c(95,95)), aes(x = x, y = y), color = "brown4", size = 1) +
    geom_line(data = data.frame(x = c(34,28), y = c(95,95)), aes(x = x, y = y), color = "brown4", size = 1) +
    geom_line(data = data.frame(x = c(-28,-28), y = c(95,97)), aes(x = x, y = y), color = "brown4", size = 1) +
    geom_line(data = data.frame(x = c(28,28), y = c(95,97)), aes(x = x, y = y), color = "brown4", size = 1) +
    geom_line(data = data.frame(x = c(-28,-15), y = c(97,97)), aes(x = x, y = y), color = "brown4", size = 1) +
    geom_line(data = data.frame(x = c(28,15), y = c(97,97)), aes(x = x, y = y), color = "brown4", size = 1) +
    geom_curve(aes(x = 15, y = 97, xend = -15, yend = 97), color = "brown4", size = 1, curvature = 0.3) +
    # bottom decoration
    geom_line(data = data.frame(x = c(-34,-28), y = c(5,5)), aes(x = x, y = y), color = "brown4", size = 1) +
    geom_line(data = data.frame(x = c(34,28), y = c(5,5)), aes(x = x, y = y), color = "brown4", size = 1) +
    geom_line(data = data.frame(x = c(-28,-28), y = c(5,3)), aes(x = x, y = y), color = "brown4", size = 1) +
    geom_line(data = data.frame(x = c(28,28), y = c(5,3)), aes(x = x, y = y), color = "brown4", size = 1) +
    geom_line(data = data.frame(x = c(-28,-15), y = c(3,3)), aes(x = x, y = y), color = "brown4", size = 1) +
    geom_line(data = data.frame(x = c(28,15), y = c(3,3)), aes(x = x, y = y), color = "brown4", size = 1) +
    geom_curve(aes(x = -15, y = 3, xend = 15, yend = 3), color = "brown4", size = 1, curvature = 0.3) +
    geom_text(aes(x= 0, y = 90, label = "Menu"),
              size = 10,
              hjust = 0.5,
              family = "Palatino",
              fontface = "italic",
              col = "brown4") +
    geom_text(aes(x= 0, y = 70, label = "Appetizers"),
              size = 5,
              hjust = 0.5,
              col = "darkolivegreen4",
              family = "serif",
              fontface = "bold.italic") +
    geom_textbox(aes(x= 0, y = 60, label = paste(choose_appetizers$appetizer, collapse = " * ")),
                 size = 4,
                 hjust = 0.5,
                 family = "serif",
                 fontface = "italic",
                 fill = NA,
                 box.colour = NA,
                 width = unit(3, "inch")) +
    geom_text(aes(x= 0, y = 50, label = "Entrees"),
              size = 5,
              hjust = 0.5,
              col = "darkolivegreen4",
              family = "serif",
              fontface = "bold.italic") +
    geom_textbox(aes(x= 0, y = 40, label = paste(choose_entrees$entree, collapse = " * ")),
                 size = 4,
                 hjust = 0.5,
                 family = "serif",
                 fontface = "italic",
                 fill = NA,
                 box.colour = NA,
                 width = unit(3, "inch")) +
    geom_text(aes(x= 0, y = 30, label = "Desserts"),
              size = 5,
              hjust = 0.5,
              col = "darkolivegreen4",
              family = "serif",
              fontface = "bold.italic") +
    geom_textbox(aes(x= 0, y = 20, label = paste(choose_desserts$dessert, collapse = " * ")),
                 size = 4,
                 hjust = 0.5,
                 family = "serif",
                 fontface = "italic",
                 fill = NA,
                 box.colour = NA,
                 width = unit(3, "inch")) +
    theme(plot.background = element_rect(fill="bisque", color = "bisque"),
          plot.margin = margin(0, 0, 0, 0),
          plot.caption = element_markdown(size = 7, hjust = .5)) +
    labs(
      caption = "Data from 'Chopped: 10+ Years of Episode Data' <br> (kaggle.com/jeffreybraun/chopped-10-years-of-episode-data)<br>"
    )

  print(menu_plot)

}

create_menu(250820)
ggsave('Week35_chopped.png')

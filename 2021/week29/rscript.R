# description -------------------------------------------------------------

# TidyTuesday week 29 - ScoobyDoo Data
# 1. Take a sad plot and make it better (bar, line or scatter)
#   - using a bar plot I theme it to be like Scooby Doo
#   - use Scooby Doo font
#   - use a colour pallette that represents The Mystery Machine and the different characters
#   - Add images of the flowers found on the mystery machine

# set up  -----------------------------------------------------------------

if(!require(pacman)) install.package("pacman")

pacman::p_load(tidyverse,
               extrafont,
               cowplot,
               ggtext)

font_import()
loadfonts()

# load data  --------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2021, week = 29)
df_scooby_raw <- tuesdata$scoobydoo

# transform data  ---------------------------------------------------------
df_scooby_tidy1 <- df_scooby_raw %>%
  # filter out the columns caught_other and caught_not
  select(-c(caught_other, caught_not)) %>%
  # combine the caught columns
  pivot_longer(starts_with("caught"),
               names_to = c("caught", "character"),
               names_sep = "_",
               values_to = "value") %>%
  # filter to only keep the rows to show who caught the monsters
  filter(value == TRUE) %>%
  # separate the monster type column where there are multiple monsters
  separate_rows(monster_type, sep = ",", convert = TRUE) %>%
  # trim any white space from the monster types
  mutate(monster_type = str_trim(monster_type)) %>%
  # filter out missing monster type
  filter(monster_type != "" & monster_type != "NULL") %>%
  # lump together the monster types with 1 or 2 occurences
  mutate(monster_type = fct_lump(monster_type, 4)) %>%
  # count the occurace of each monster type and order in descending order
  count(monster_type, character, sort = TRUE) %>%
  # reorder the monster type to plot in size order
  mutate(monster_type = fct_reorder(monster_type, n))

# plot a bar chart to show how many monsters each of the characters have caught and what type

# default plot
gg_default <- df_scooby_tidy1 %>%
  ggplot(aes(x = n, y = reorder(character, n, sum), fill = monster_type)) +
  geom_col() +
  labs(title = "Jeepers! Who caught the most monsters?",
       subtitle = "Scooby Dooby Doo (caught the most monsters)",
       y = "",
       x = "Number of monsters")

# theme the plot to be more "Scooby Doo"
# - Use Scooby Doo font
# - Use a colour paellette inspired by Scooby Doo
# - Add some flowers which are on the mystery machine

scooby_colours <-
  c('#76a2ca', # blue
    '#cd7e05', # orange
    '#966a00', #brown
    '#b2bb1b', # green
    '#7c68ae') # purple

gg_creative <- gg_default +
  geom_col(col = "black") +
  scale_fill_manual(values = scooby_colours, name = "Monster Type") +
  theme(text = element_text(family = "Scooby Doo", size = 16),
        plot.title = element_markdown(size = 20, colour = "#ff8d00", fill = "#00e304"),
        plot.background = element_rect(fill = "#00cfd4"),
        panel.background = element_rect(fill = "#00e304", colour = "black"),
        panel.grid = element_blank(),
        legend.background = element_rect(fill = "#00e304", colour = "black"))

ggdraw() +
  draw_plot(gg_creative) +
  draw_image("TidyTuesday_git/2021/week29/flower.png", x = 1, y = 1, hjust = 1, vjust = 1, halign = 0.85, valign = 0.01, scale = 0.25) +
  draw_image("TidyTuesday_git/2021/week29/flower.png", x = 1, y = 1, hjust = 1, vjust = 1, halign = 0.92, valign = 0.25, scale = 0.15)

ggsave(plot = gg_default, "default_week29.png", width = 10, height = 8)
ggsave("week29.png", width = 10, height = 8)


# set up  -----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(extrafont)

loadfonts()

# load data  --------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2021, week = 27)
df_data_raw <- tuesdata$animal_rescues

glimpse(df_data_raw)
View(df_data_raw)

# transform data  ---------------------------------------------------------

df_data <- df_data_raw %>%
  filter(cal_year < 2021) %>%
  mutate(animal_group_parent = ifelse(animal_group_parent == "cat", "Cat", animal_group_parent),
         animal_group_parent = fct_lump(animal_group_parent, 4)) %>%
  mutate(date = dmy_hm(date_time_of_call),
         month = month(date, label = TRUE)) %>%
  mutate(year2020 = ifelse(cal_year == 2020, "2020", "<2020"))

df_data %>%
  group_by(animal_group_parent, month, cal_year, year2020) %>%
  count() %>%
  group_by(animal_group_parent, month, year2020) %>%
  summarise(average = median(n)) %>%
  ungroup() %>%
  group_by(month, year2020) %>%
  mutate(total = sum(average)) %>%
  ggplot(aes(x = month, y = average, group = fct_reorder(animal_group_parent, average), fill = animal_group_parent)) +
  geom_col(width = 0.98, col = "black", size = 0.1) +
  geom_text(aes(y = total + 8, label = month), size  = 2) +
  coord_polar() +
  facet_wrap(~year2020, strip.position="bottom") +
  theme_void() +
  labs(title = "Average number of animals rescued",
       subtitle = "Pre 2020 vs 2020",
       y = "",
       x = "",
       caption = "#TidyTuesday week 27 | source: London Fire Brigade | datavis: @kayleahaynes") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5,
                                     margin = margin(10,0,30,0)),
        plot.caption = element_text(size = 8, margin = margin(30,0,0,0)),
        panel.border = element_blank(),
        legend.position = "top",
        panel.grid = element_line(size = 0.05),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 8),
        text = element_text(family = "Catamaran", size = 14),
        plot.background = element_rect(fill = "white", color = "white")) +
  scale_fill_manual(values = c("#d1b490", "#8aa8a1", "#885a89", "#ee7b30", "#cbcbd4"), name = "")

ggsave("week27.png", height = 5, width = 10)

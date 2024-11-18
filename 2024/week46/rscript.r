# Load packages -----------------------------------------------------------------------------------------------------------------------------------------------

library(ggplot2)
library(tidyverse)
library(showtext)
library(forcats)
library(ggtext)
library(geofacet)

source("2024/theme_kaylea.r")

# Load fonts --------------------------------------------------------------------------------------------------------------------------------------------------

font_add_google("Carter One", "carter")

# Load data ---------------------------------------------------------------------------------------------------------------------------------------------------

df_countries <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-11-12/countries.csv')
df_country_subdivisions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-11-12/country_subdivisions.csv')
df_former_countries <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-11-12/former_countries.csv')

# EDA/transformations ---------------------------------------------------------------------------------------------------------------------------------------------------

df_country_codes_format <- df_countries %>%
  separate(alpha_3, into = c("a", "b", "c"), sep = c(1,2), remove = FALSE) %>% 
  pivot_longer(
    cols = c(a, b, c),
    names_to = "letter_pos",
    values_to = "value"
  ) %>% 
  mutate(
    x = rep(c(1:3), times = nrow(df_countries))
  ) 

# Plot ------------------------------------------------------------------------------------------------------------------------------------------------------------------

ggplot(df_country_codes_format) + 
  geom_raster(
    mapping = aes(
      x = x, y = 1, fill = value
    )
  ) + 
  geom_text(
    mapping = aes(
      x = x, y = 1, label = value
    ),
    hjust = 0.5,
    size = 6, 
    col = "white", 
    family = "carter"
  ) + 
  theme_void(base_size = 20, 
             base_family = "carter") + 
  facet_geo(~alpha_3, grid = "world_countries_grid1", label = "code_alpha3") + 
  theme(plot.margin = margin(10, 10, 10, 10),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    legend.position = "none",
    strip.text = element_blank(),
    panel.spacing = unit(0.2, "lines")) + 
  labs(
    title = "ISO 3166-1 Country Codes",
    caption = "#TidyTuesday 2024 week 46 | dataviz by @kayleahaynes | Source: iso_codes",
    x = "Year",
    y = "Country")

ggsave(file.path("2024", "week46", "plot.png"),
    width = 7, height = 7)

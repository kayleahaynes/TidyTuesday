# Load packages -----------------------------------------------------------------------------------------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(showtext)
library(forcats)
library(ggtext)

source("2024/theme_kaylea.r")

# Load fonts --------------------------------------------------------------------------------------------------------------------------------------------------

font_add_google("Lato", "lato")
showtext_auto()

# Colours -----------------------------------------------------------------------------------------------------------------------------------------------------

democratic_color <- "#006400"    # Dark shade
non_democratic_color <- "#90EE90"   # Lighter shade

# Load data ---------------------------------------------------------------------------------------------------------------------------------------------------

df_democracy_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-11-05/democracy_data.csv')

# EDA/transformations ---------------------------------------------------------------------------------------------------------------------------------------------------

head(df_democracy_data)

unique(df_democracy_data$country_name)
unique(df_democracy_data$is_democracy)

# Get the countries where the demographic status has changed
df_democracy_change_countries <- df_democracy_data %>% 
    group_by(country_name) %>% 
    summarise(democracy_type = n_distinct(is_democracy)) %>% 
    filter(democracy_type == 2) 

df_democracy_data_changed_countries <- df_democracy_data %>% merge(df_democracy_change_countries, how = "inner", by = "country_name")

ggplot(df_democracy_data_changed_countries, 
    aes(x = year, 
        y = reorder(country_name, is_democracy), 
        fill = as.factor(is_democracy)), 
        color = country_name, 
        group = country_name) +
  geom_tile(color = "white") + 
  scale_fill_manual(values = c("TRUE" = democratic_color, "FALSE" = non_democratic_color)) +
  labs(
    title = "Democracy Status by Country and Year",
    subtitle = "<span style='color:blue;'>Democratic</span> and <span style='color:red;'>Non-Democratic</span> status.",
    caption = "#TidyTuesday 2024 week 45 | dataviz by @kayleahaynes | Source: demography_data",
    x = "Year",
    y = "Country",
    fill = "Democracy Status"
  ) + 
  theme_kaylea(family = "lato", base_size = 50) + 
  theme(
    legend.position = "none",  
    plot.subtitle = element_markdown() 
  )

  ggsave(file.path("2024", "week45", "plot.png"),
    width = 12, height = 20)

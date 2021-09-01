# description -------------------------------------------------------------

# TidyTuesday week 36 Bird baths

# set up  -----------------------------------------------------------------

if(!require(pacman)) install.package("pacman")

pacman::p_load(tidyverse,
               showtext)

font_add_google("Julius Sans One")
showtext_auto()

# load data  --------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2021, week = 36)

df_birdbaths <- tuesdata$bird_baths

glimpse(df_birdbaths)

# count the number of birds of each bird type and find the top 10 most sighted birds. Calculate the % of these bird sightings compared to the total number of bird sightings

sum_bird_count <- df_birdbaths %>%
  filter(bird_count == 1) %>%
  count(bird_type, sort = TRUE) %>%
  ## add percentage label with `sprintf()` (% of the total number of sightings)
  mutate(perc = paste0(sprintf("%4.1f", n / sum(n) * 100), "%")) %>%
  top_n(10) %>%
  mutate(bird_type = fct_reorder(bird_type, n),
         perc = if_else(row_number() == 1, paste(perc, "of all bird sightings"), perc))

# create colour palette

pal <- c(
  rep("gray70", length(sum_bird_count$bird_type) - 3),
  "#B9D2B1", "#A18276", "#FBACBE"
)

# plot the bar chart

sum_bird_count %>%
  ggplot(aes(x = n, y = bird_type)) +
  geom_col(aes(fill = bird_type)) +
  geom_text(aes(label = perc),
            hjust = 1,
            nudge_x = -.5,
            family = "Julius Sans One",
            size = 6) +
  scale_fill_manual(values = pal, guide = "none") +
  labs(x = "Number of sightings",
       y = "Bird Type",
       title = "Noisy Miners are the bullies of the bird baths",
       subtitle = str_wrap("The total number of sightings of the top 10 dominating birds found at bird baths across Austrailia.", 70),
       caption = paste("\n\n #TidyTuesday week 36 | dataviz by @kayleahaynes | Source: Cleary et al, 2016  \n\n", str_wrap("Data on bird occurrence at bird baths were collected during “The Bathing Birds Study” that ran for a four week period in each of two seasons: austral winter (June 24th to July 26th 2014) and summer (January 27th to February 29th 2015).", 110))) +
  theme(plot.title = element_text(hjust = 0.5, size = 32),
        plot.subtitle = element_text(hjust = 0.5, size = 20, margin = margin(0,0,10,0)),
        plot.caption = element_text(hjust = 0),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "#F1D6B8", size = 0.1),
        panel.grid.minor = element_line(colour = "#F1D6B8", size = 0.2),
        axis.ticks = element_blank(),
        text = element_text(family = "Julius Sans One", lineheight = 0.4, size = 18))

ggsave("week36.png", width = 5, height = 4, units = "in", dpi = 300)



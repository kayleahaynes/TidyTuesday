# description -------------------------------------------------------------

# TidyTuesday week 33 - Olympics
# 1.

# set up  -----------------------------------------------------------------

if(!require(pacman)) install.package("pacman")

devtools::install_github("hrbrmstr/streamgraph")

pacman::p_load(tidyverse,
               ggthemes,
               scales,
               streamgraph,
               ggstream,
               ragg)

# load data  --------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2021, week = 33)

df_chaininvestment_raw <- tuesdata$chain_investment

# Replicate this plot and try other plot types for evolution
# https://twitter.com/DonFSchneider/status/1367181077316575236/photo/1

# select the relevant investment types

df_chaininvestment <- df_chaininvestment_raw %>%
  filter(category %in% c("Water",
                         "Sewer",
                         "Power",
                         "Conservation and development",
                         "Highways and streets",
                         "Air transportation",
                         "Water transportation",
                         "Rail transportation",
                         "Transit",
                         "Other federal",
                         "Other private")) %>%
  group_by(category, year) %>%
  summarise(total_gross_inv_chain = sum(gross_inv_chain))

# line plot

agg_png("week33_lineplot.png",
        width = 14, height = 10, units = "in", res = 300)

df_chaininvestment %>%
  ggplot(aes(x=year, y=total_gross_inv_chain, fill=category, color = category)) +
  geom_line(size = 1) +
  labs(title = "Basic Infrastructure: gross investment",
       subtitle = "Chained 2021 dollars",
       y = "Gross Investment ($ x million)",
       x = "Year") +
  ggthemes::theme_pander(base_size = 18) +
  scale_color_ptol() +
  scale_y_continuous(labels = dollar)

dev.off()

# area plot

agg_png("week33_area.png",
        width = 14, height = 10, units = "in", res = 300)

df_chaininvestment %>%
  ggplot(aes(x=year, y=total_gross_inv_chain, fill=category, group = category)) +
  geom_area() +
  labs(title = "Basic Infrastructure: gross investment",
       subtitle = "Chained 2012 dollars",
       y = "Gross Investment ($ x million)",
       x = "Year",
       caption = str_wrap("#TidyTuesday week 33 |
       dataviz by @kayleahaynes | Source: BEA")) +
  ggthemes::theme_pander(base_size = 14) +
  scale_fill_ptol() +
  facet_wrap(~category, scales = "free_y") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = dollar)

dev.off()

# stacked area plot

agg_png("week33_stackedarea.png",
        width = 14, height = 10, units = "in", res = 300)

df_chaininvestment %>%
  ggplot(aes(x=year, y=total_gross_inv_chain, fill=category, group = category)) +
  geom_area() +
  labs(title = "Basic Infrastructure: gross investment",
       subtitle = "Chained 2012 dollars",
       y = "Gross Investment ($ x million)",
       x = "Year",
       caption = str_wrap("#TidyTuesday week 33 |
       dataviz by @kayleahaynes | Source: BEA")) +
  ggthemes::theme_pander(base_size = 18) +
  scale_fill_ptol() +
  scale_y_continuous(labels = dollar)

dev.off()

# stream plot
## using geom_steam

agg_png("week33_ggstream.png",
        width = 14, height = 10, units = "in", res = 300)

df_chaininvestment %>%
  ggplot(aes(x=year, y=total_gross_inv_chain, fill=category, group = category)) +
  geom_stream() +
  labs(title = "Basic Infrastructure: gross investment",
       subtitle = "Chained 2012 dollars",
       y = "Gross Investment ($ x million)",
       x = "Year",
       caption = str_wrap("#TidyTuesday week 33 |
       dataviz by @kayleahaynes | Source: BEA")) +
  ggthemes::theme_pander(base_size = 18) +
  scale_fill_ptol() +
  scale_y_continuous(labels = dollar)

dev.off()

## using streamgraph

agg_png("week33_stream_streamgraph.png",
        width = 14, height = 10, units = "in", res = 300)

df_chaininvestment %>%
  streamgraph(key = category,
              value = total_gross_inv_chain,
              date = year)

dev.off()

# stacked bar chart

agg_png("week33_stacked_bar.png",
        width = 14, height = 10, units = "in", res = 300)

df_chaininvestment %>%
  ggplot(aes(x=year, y=total_gross_inv_chain, fill=category, group = category)) +
  geom_col() +
  labs(title = "Basic Infrastructure: gross investment",
       subtitle = "Chained 2012 dollars",
       y = "Gross Investment ($ x million)",
       x = "Year",
       caption = str_wrap("#TidyTuesday week 33 |
       dataviz by @kayleahaynes | Source: BEA")) +
  ggthemes::theme_pander(base_size = 18) +
  scale_fill_ptol() +
  scale_y_continuous(labels = dollar)

dev.off()

## using streamgraph

agg_png("week33_stacked_bar_streamgraph.png",
        width = 14, height = 10, units = "in", res = 300)

df_chaininvestment %>%
  streamgraph(key = category,
              value = total_gross_inv_chain,
              date = year,
              interpolate="step",
              interactive = FALSE) %>%
  sg_legend(show = TRUE)

dev.off()

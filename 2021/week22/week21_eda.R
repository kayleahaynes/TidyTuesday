# load dependencies
library(tidytuesdayR)
library(tidyverse)
library(data.table)

# set up theme for plotting
theme_set(theme_light())

# load data ----------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2021, week = 22)

df_drivers <- tuesdata$drivers
df_records <- tuesdata$records

# transform the data ------------------------------------
## filter to only look at non-shortcut records
df_records_nc <- df_records %>%
  filter(shortcut == "No")

# for every day, who holds each of the records

# this was my original attempt but it didn't take ties into account

# df_daily_record <-
# df_records_nc %>%
#   filter(date >= "2011-01-01") %>% # no records were achieved by Matthias before 2011
#   group_by(track, type, shortcut) %>%
#   complete(date = seq.Date(min(date), max(date), by="day")) %>%
#   fill(player, system_played, time_period, time)

# second idea is to take each record and replicate for the duration. Going to
# use data table to do this

df_records_nc_to_expand <-  df_records_nc %>%
  filter(record_duration > 0)

df_records_nc_no_expand <-  df_records_nc %>%
  filter(record_duration <= 0)

df_records_nc_expanded <- setDT(df_records_nc_to_expand)[ , list(track = track,
                             type = type,
                             player = player,
                             time = time,
                             record_duration = record_duration,
                            date = seq(date, date+record_duration-1, by = "day")), by = 1:nrow(df_records_nc_to_expand)]

df_combine <- bind_rows(df_records_nc_expanded, df_records_nc_no_expand)

# convert back to a dataframe and count how many WR's MR has on each day. Don't include the times where he is then beaten on the same day

df_matthias <- data.frame(df_combine) %>%
  filter(player == "MR") %>%
  count(date)

df_matthias %>%
  ggplot() +
  geom_line(aes(x = date, y = n)) +
  labs(x = "Date",
       y = "Number of records",
       title = "Number of WRs held by Matthias")

# some more creative plots instead of the line plot
# -- summary plots looking at the total number of records held and over which tracks.
df_matthias %>%
  ggplot(aes(x = date, y = 1, fill = n)) +
  geom_tile() +
  scale_fill_distiller(palette = "Spectral", limits = c(0,32),
                       breaks = c(0, 16, 32), direction = -1) +
  theme(
    # main plotting theme -------------------------
    text = element_text(family = "Helvetica Neue"),
    plot.background = element_rect(fill = "black", color = "black"),
    plot.title = element_text(lineheight = 1.1, hjust = 0.5, colour = "white"),
    plot.title.position = "panel",
    plot.caption = element_text(hjust = 1, face= "italic", color = "white"),
    panel.background = element_rect(fill = "black", color = "black"),
    panel.grid = element_blank(),

    # axes ---------------------------------
    # axis.line,

    axis.ticks = element_blank(),
    axis.title.x = element_text(color = "white",
                                size = rel(1)),
    axis.title.y = element_blank(),
    axis.text.x = element_text(color = "white",
                               size = rel(0.8)),
    axis.text.y = element_blank(),

    # legend ------------------------------------
    legend.position = "none"
  )

df_matthias_all <- data.frame(df_combine) %>%
  filter(player == "MR") %>%
  mutate(year = year(date),
         day_month = format.Date(date, "%m-%d"))

alpha <- ifelse(df_matthias_all$type == "Three Lap", 0.9, 0.5)

df_matthias_all %>%
  ggplot(aes(x = day_month, y = paste(track, type))) +
  geom_point(aes(col = track, shape = type, alpha = alpha)) +
  facet_wrap(~year, ncol = 1) +
  scale_shape_manual(values=c(15, 15)) +
  labs(x = "Date",
       y = "") +
  theme(
    # main plotting theme -------------------------
    text = element_text(family = "Helvetica Neue"),
    plot.background = element_rect(fill = "black", color = "black"),
    plot.title = element_text(lineheight = 1.1, hjust = 0.5, colour = "white"),
    plot.title.position = "panel",
    plot.caption = element_text(hjust = 1, face= "italic", color = "white"),
    panel.background = element_rect(fill = "black", color = "black"),
    panel.grid = element_blank(),

    # axes ---------------------------------
    # axis.line,

    axis.ticks = element_blank(),
    axis.title.x = element_text(color = "white",
                              size = rel(1)),
    axis.title.y = element_blank(),
    axis.text.x = element_text(color = "white",
                             size = rel(0.8)),
    axis.text.y = element_blank(),

    # legend ------------------------------------
    legend.position = "none",
    strip.placement = "none",
    strip.text.x = element_blank()
  )

ggsave("test.png")

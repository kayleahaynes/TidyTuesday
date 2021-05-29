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

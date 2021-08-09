# description -------------------------------------------------------------

# TidyTuesday week 31 - Olympics
# 1. barplot looking at the number of events for male and female's at the summer Olympics over time
# 2. Some deeper exploration in the equality of the events looking at the number of events within a sport that each sex can compete in.

# set up  -----------------------------------------------------------------

if(!require(pacman)) install.package("pacman")

pacman::p_load(tidyverse,
               hrbrthemes,
               paletteer,
               ragg,
               kableExtra)

# load data  --------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2021, week = 31)
df_olympics_raw <- tuesdata$olympics

# transform data  ---------------------------------------------------------

# how many events are there each year at the Olympics split by sex?

df_events_count_by_sport <- df_olympics_raw %>%
  filter(season == "Summer") %>%
  select(sex, year, sport, event) %>%
  distinct() %>%
  group_by(sex, year, sport) %>%
  count(name = "number of events") %>%
  group_by(sport) %>%
  mutate(`overall sport events` = sum(`number of events`))

# filter the sports to only look at the ones that were part of the olympics in 2016
df_events_2016 <-
  df_olympics_raw %>%
  filter(year == 2016) %>%
  select(sport, sex) %>%
  distinct()

# count the number of events there are overall split by sex.
df_events_count_overall <- df_olympics_raw %>%
  filter(season == "Summer") %>%
  select(sex, year, sport, event) %>%
  distinct() %>%
  group_by(sex, year) %>%
  count(name = "number of events")

# remove Men and Women from the events name. This will allow us to compare the events across Men and Women

df_events_2016_clean_name <-
  df_olympics_raw %>%
  filter(season == "Summer",
         sport %in% df_events_2016$sport) %>%
  select(sport, event, sex, year) %>%
  mutate(clean_event = str_replace(event, paste(sport, "Men's "), ""),
         clean_event = str_replace(clean_event, paste(sport, "Women's "), "")) %>%
  distinct()

# plot data --------------------------------------------------------------

# plot of how many events are contested by males and females

agg_png("week31_number_of_events_barplot.png", width = 14, height = 10, units = "in", res = 300)

df_events_count_overall %>%
  ungroup() %>%
  add_row(sex = "F", year = 1896, `number of events` = 0) %>%
  ggplot(aes(x = as.factor(year), y = `number of events`)) +
  geom_col(aes(fill = sex, col = sex), position = "dodge", alpha = 0.9) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Year",
       y = "Number of events",
       title = "Number of male and female events at the summer Olympics",
       subtitle = "The number of events for females is steadily growing but there is still a gap",
       caption = str_wrap("#TidyTuesday week 31 |
       dataviz by @kayleahaynes | Source: Kaggle This is a historical dataset on the modern Olympic Games, including all the Games from Athens 1896 to Rio 2016", 100)) +
  theme_ipsum(axis_title_size = 14,
              axis_text_size = 12,
              base_size = 20) +
  scale_fill_manual(values = c("#FF6F59", "#8CBA80")) +
  scale_colour_manual(values = c("#FF6F59", "#8CBA80"))

# Take a look at the proportion of events within each sport that have both male and female
# Equality in events:
# Count how many events in each sport are equal

df_event_equality <- df_events_2016_clean_name %>%
  group_by(sport, clean_event, year) %>%
  # take a count of how many of each event there is. If 2 then there is a male and female event
  mutate(male_and_female = n()) %>%
  mutate(equality = case_when(male_and_female == 2 ~ "equal",
                              male_and_female == 1 & sex == "M" ~ "Male only",
                              TRUE ~ "Female only"))

df_sport_equality <- df_event_equality %>%
  group_by(sport, year, equality) %>%
  # within each sport and year count how many events are male only, female only or both
  summarise(number_of_events = n()) %>%
  # get the total number of events for each sport
  group_by(sport, year) %>%
  mutate(total_number_of_events = sum(number_of_events)) %>%
  mutate(proportion = number_of_events / total_number_of_events) %>%
  # arrange by most years with equality
  group_by(sport) %>%
  mutate(number_of_equality_years = sum(proportion == 1 & equality == "equal"))

df_equality_plot_data <- df_sport_equality %>%
  filter(equality == "equal") %>%
  right_join(unique(df_events_2016_clean_name %>% select(sport))) %>%
  replace_na(list(proportion = 0, number_of_equality_years = 0)) %>%
  group_by(sport) %>%
  mutate(max_equality = max(proportion)) %>%
  arrange(max_equality, number_of_equality_years) %>%
  mutate(sport = factor(sport, levels = unique(.$sport), ordered = TRUE))

agg_png("week31_proportion_of_events.png", width = 12, height = 8, units = "in", res = 300)

df_equality_plot_data %>%
  ggplot(aes(x = year, y = sport, col = proportion)) +
  geom_point(alpha = 0.8, aes(size = proportion))  +
  theme_ipsum(axis_title_size = 14,
              axis_text_size = 12,
              base_size = 20) +
  scale_color_paletteer_c("grDevices::Plasma", direction = -1) +
  labs(y = "Sport",
       x = "Year",
       title = "Proportion of events within each sport contested by both sexes",
       subtitle = "Diving has been the most equal sport across the history of the Olympics",
       caption = str_wrap("#TidyTuesday week 31 |
       dataviz by @kayleahaynes | Source: Kaggle This is a historical dataset on the modern Olympic Games, including all the Games from Athens 1896 to Rio 2016", 100)) +
  guides(size = "none")

dev.off()

# a similar plot to above but this time looking at the number of medals instead of the proportions

agg_png("week31_number_of_events_equality.png", width = 14, height = 10, units = "in", res = 300)

df_equal <- df_events_count_by_sport %>%
  pivot_wider(names_from = sex,
              values_from = `number of events`) %>%
  filter(`F` == `M`) %>%
  right_join(unique(df_events_2016_clean_name %>% select(sport)))

df_events_count_by_sport %>%
  filter(sport %in% df_events_2016$sport,
         !paste(year, sport) %in% paste(df_equal$year, df_equal$sport)) %>%
  right_join(unique(df_events_2016_clean_name %>% select(sport))) %>%
  mutate(sex = factor(sex, levels = c("F", "M"), ordered = TRUE)) %>%
  mutate(sport = factor(sport, levels = unique(df_equality_plot_data$sport), ordered = TRUE)) %>%
  ggplot(aes(x = year, y = sport, size = `number of events`)) +
  geom_point(alpha = 0.9, aes(fill = "#8CBA80", col = "#8CBA80", shape = "23")) +
   geom_point(data = . %>% filter(sex == "F"), alpha = 0.9, aes(fill = "#FF6F59",  col = "#FF6F59", shape = "21")) +
   geom_point(data = df_equal %>%
                filter(sport %in% df_events_2016$sport) %>%
                mutate(sport = factor(sport, levels = unique(df_equality_plot_data$sport), ordered = TRUE)), aes(x = year, y = sport, size = `F`, fill = "#684A52", col = "#684A52", shape = "22"), alpha = 0.9) +
  labs(x = "Year",
       y = "Sport",
       title = "Number of events within each sport* contested by both sexes",
       subtitle = "*only includes sports which were part of the Olympics in 2016, only males competed in 1896",
       caption = str_wrap("#TidyTuesday week 31 |
       dataviz by @kayleahaynes | Source: Kaggle This is a historical dataset on the modern Olympic Games, including all the Games from Athens 1896 to Rio 2016", 100)) +
  scale_size_continuous(range = c(2,10), breaks = c(1:30)) +
  theme_ipsum(axis_title_size = 14,
              axis_text_size = 12,
              base_size = 20) +
  scale_fill_manual(name = 'Sex',
                      values =c('#8CBA80'='#8CBA80','#FF6F59'='#FF6F59', '#684A52' = '#684A52'),
                    labels = c('Male','Female', 'Equal')) +
  scale_color_manual(name = 'Sex',
                    values =c('#8CBA80'='#8CBA80','#FF6F59'='#FF6F59', '#684A52' = '#684A52'),
                    labels = c('Male','Female', 'Equal')) +
  scale_shape_manual(name = "Sex",
                     labels = c('Male','Female', 'Equal'),
                     values = c("23" = 23, "21" = 21, "22" = 22)) +
  guides(size = "none",
         fill = guide_legend(override.aes = list(size=8)))

dev.off()

# in 2016 there are 40 events only ever competed by men
only_men <- df_event_equality %>%
  filter(event %in% df_events_2016_clean_name$event) %>%
  group_by(sport, clean_event) %>%
  mutate(all_time_male_female = max(male_and_female)) %>%
  filter(all_time_male_female == 1, year == 2016) %>%
  select(event, sex) %>%
  distinct() %>%
  filter(sex == "M") %>%
  arrange(event)

kable(only_men) %>%
  save_kable("only_men.png")

# in 2016 there are 14 events only ever competed by men
only_women <- df_event_equality %>%
  filter(event %in% df_events_2016_clean_name$event) %>%
  group_by(sport, event) %>%
  mutate(all_time_male_female = max(male_and_female)) %>%
  filter(all_time_male_female == 1, year == 2016) %>%
  select(event, sex) %>%
  distinct() %>%
  filter(sex == "F") %>%
  arrange(event)

kable(only_women) %>%
  save_kable("only_women.png")

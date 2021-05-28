# Description of script ---------------------------
# Week 20 TidyTuesday
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-05-04/readme.md
# This script explores the data set for tidytuesday. The final output and clean script can be found in rscript_final.R.

# Data: Broadband access and usage in America

# script set up -----------------------------------

# load libraries
library(tidytuesdayR) # tidytuesday package used to get the data
library(tidyverse) # used for EDA and plotting the data
library(zipcodeR) # make working with zip codes easier
library(maps) # draw geographical maps
library(mapdata) # get some map data
library(patchwork) # used to combine plots
library(geofacet) # facet the plots based on geographic location
library(extrafont) # used for custom font types
library(ggtext) # used for rendering plot titles

# Previously installed the fonts I want to use, the following code loads in these fonts
font_import()
loadfonts()

# load data ---------------------------------------

tuesdata <- tt_load(2021, week = 20)
df_broadband <- tuesdata$broadband %>% janitor::clean_names()
# df_broadband_zip <- tuesdata$broadband_zip %>% janitor::clean_names()

glimpse(df_broadband)
#glimpse(df_broadband_zip)

# for this TidyTuesday only planning to use df_broadband data

# use the zipcode info to get the populations in the states
zipcode_info <- zipcodeR::zip_code_db %>%
  select(zipcode,
         county,
         state,
         population) %>%
  mutate(county_name = tolower(unlist(purrr::map(county, gsub, pattern = " County", replacement = ""))))

# transform data -----------------------------------
df_broadband <- df_broadband %>%
  mutate(broadband_availability_per_fcc = as.numeric(broadband_availability_per_fcc), # convert to numeric
         broadband_usage = as.numeric(broadband_usage), # convert to numeric
         county_name = tolower(unlist(purrr::map(county_name, gsub, pattern = " County", replacement = "")))) %>% # remove "county"
  mutate(colour = ifelse(broadband_usage < 0.15, '<15%', '>=15%')) # going to colour the graph based on these groupings

# explore the data ---------------------------------

### try and replicate the plot from https://www.theverge.com/22418074/broadband-gap-america-map-county-microsoft-data looking at the Broadband usage

df_counties <- map_data("county")
df_states <- map_data("state")

df_state_names_and_abbr <- data.frame(abbr = datasets::state.abb,
                                      name = tolower(datasets::state.name),
                                      stringsAsFactors = FALSE)

# note there are multiple counties with the same name
# df_broadband %>% count(county_name, sort = TRUE)

gg_americas_broadband_problems <- df_counties %>%
  left_join(df_state_names_and_abbr, by = c("region" = "name")) %>%
  left_join(df_broadband, by = c("subregion" = "county_name", "abbr" = "st")) %>%
  filter(!is.na(broadband_usage)) %>%
  ggplot(aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=colour), color = "white", size=0.1) +
  geom_polygon(data=df_states, fill=NA, color="white", size=0.5) +
  scale_fill_manual(values = c('#0D0787', '#C4C4C4'), guide_legend("Percentage of people using the internet at 25 Mbps or above per county")) +
  theme_void() +
  labs(title = "This is a map of America's Broadband Problems",
       subtitle = "A county by county look at the broadband gap",
       caption = "TidyTuesday: Week 20 · Source: Microsoft by way of The Verge · Graphic: Kaylea Haynes") +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 24),
    plot.subtitle = element_text(hjust = 0.5, size = 20),
    legend.position = c(0.35,0.05),
    legend.direction = "horizontal",
    legend.title = element_text(face = "bold"),
    text = element_text(family = "PT Sans Caption")) +
  guides(fill = guide_legend(title.position="top", title.hjust = 0))

ggsave("week20_1.png", gg_americas_broadband_problems, width = 14, height = 10, dpi = 320)


### Try and replicate the maps of broadband speeds https://github.com/microsoft/USBroadbandUsagePercentages/blob/master/assets/broadbandmap.png

gg_availability <- df_counties %>%
  left_join(df_state_names_and_abbr, by = c("region" = "name")) %>%
  left_join(df_broadband, by = c("subregion" = "county_name", "abbr" = "st")) %>%
  filter(!is.na(broadband_usage)) %>%
  ggplot(aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=broadband_availability_per_fcc), color = "white", size=0.1) +
  geom_polygon(data=df_states, fill=NA, color="white", size=0.5) +
  scale_fill_continuous("FCC Broadband availability", trans = 'reverse', labels = c("0%", "25%", "50%", "75%", "100%")) +
  theme_void() +
  labs(title = stringr::str_wrap("FCC indicated broadband is not available to 21.3M people", 80),
       subtitle = stringr::str_wrap("Previous FCC 2018 report indicated that broadband was not available to 24.7M people", 60)) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 32),
    plot.subtitle = element_text(hjust = 0.5, size = 24),
    legend.position = c(0.45,0.05),
    legend.direction = "horizontal",
    text = element_text(family = "PT Sans Caption", size = 18)) +
  guides(fill = guide_legend(label.position="top", title.hjust = 0))

### for the microsoft data take the average in the country

gg_usage <- df_counties %>%
  left_join(df_state_names_and_abbr, by = c("region" = "name")) %>%
  left_join(df_broadband, by = c("subregion" = "county_name", "abbr" = "st")) %>%
  filter(!is.na(broadband_usage)) %>%
  ggplot(aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=broadband_usage), color = "white", size=0.1) +
  geom_polygon(data=df_states, fill=NA, color="white", size=0.5) +
  scale_fill_continuous("Broadband Usage", trans = 'reverse', labels = c("0%", "25%", "50%", "75%", "100%")) +
  theme_void() +
  labs(title = stringr::str_wrap("Microsoft data indicates ~157.3M people do not use the internet at broadband speeds", 60),
       subtitle = stringr::str_wrap("Previous data from September 2018 indicated ~162.8M people did not use the internet at broadband speeds", 60),
       caption = "TidyTuesday: Week 20 · Source: Microsoft by way of The Verge · Graphic: Kaylea Haynes") +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 32),
    plot.subtitle = element_text(hjust = 0.5, size = 24),
    plot.caption = element_text(hjust = 0, size = 16),
    legend.position = c(0.35,0.05),
    legend.direction = "horizontal",
    text = element_text(family = "PT Sans Caption", size = 18)) +
  guides(fill = guide_legend(label.position="top", title.hjust = 0))

ggsave("week20_2.png", (gg_availability / gg_usage), width = 20, height = 22, dpi = 320)

### look at the difference between availability and usage by state

df_broadband_by_state <- df_broadband %>%
  # join zipcode info to get populations
  left_join(zipcode_info, by = c("county_name" = "county_name", "st" = "state")) %>%
  group_by(st, county_name) %>%
  # group by state and county to calculate population and broadband stats at county level
  summarise(broadband_availability_per_fcc = broadband_availability_per_fcc[1],
            broadband_usage = broadband_usage[1],
            population = sum(population, na.rm = TRUE)) %>%  # get the total population in a county
  group_by(st) %>%
  # group by state to calculate population and broadband stats at state level
  summarise(total_state_population = sum(population, na.rm = TRUE),
            broadband_usage_population = sum(broadband_usage * population, na.rm = TRUE),
            broadband_usage_percent = round(broadband_usage_population/total_state_population,2),
            broadband_availability = sum(broadband_availability_per_fcc * population, na.rm = TRUE),
            broadband_availability_percent = round(broadband_availability/total_state_population,2))

# normalise the data so that the availability percentage is 1. Note usage is always less than availability
df_broadband_by_state_relative <- df_broadband_by_state %>%
  mutate(broadband_usage_percent_relative = broadband_usage_percent/broadband_availability_percent,
           broadband_availability_percent_relative = 1)

# reshape the data
df_broadband_by_state_relative <- reshape2::melt(data = df_broadband_by_state_relative %>% select(st, broadband_availability_percent_relative, broadband_usage_percent_relative),
                       id = "st",
                       variable.name = "useage_avail",
                       value.name = "percent")

# plot a slopegraph for each state in a geofacet

gg_slopegraph <- df_broadband_by_state_relative %>%
  ggplot() +
  geom_point(aes(x = useage_avail, y = percent, group = st, col = useage_avail), size = 2) +
  geom_line(aes(x = useage_avail, y = percent, group = st)) +
  facet_geo(~st) +
  theme_void() +
  scale_color_manual(values = c("#002768", "#BF0A2F")) +
  labs(title = "The differences between
    <span style=\'color:#002768;\'>broadband availability</span> and <br>
    <span style=\'color:#BF0A2F;\'>broadband usage</span>
    </span>",
       subtitle = stringr::str_wrap("Broadband availability comes from the FCC which is a notoriously inaccurate survey drawn from ISPs’ own descriptions of the areas they serve. In comparison broadband usage comes from an anonymised dataset Microsoft have collected through its cloud services network. The longer the lines the larger the difference between the % of the population with broadband \"available\" and the % of the population using broadband", 120),
       caption = stringr::str_wrap("TidyTuesday: Week 20 · Source: Broadband usage from Microsoft by way of The Verge, state population from zipcodeR package  · Graphic: Kaylea Haynes", 80)) +
  theme(plot.title = element_markdown(hjust = 0.5, size = 24, face = "bold"),
        plot.caption = element_text(),
        plot.subtitle = element_text(size =14, colour = "grey", hjust = 0.5),
        text = element_text(family = "PT Sans Caption", size = 14),
        legend.position = "none")

ggsave("week20_3.png", gg_slopegraph, width = 12, height = 10, dpi = 320)




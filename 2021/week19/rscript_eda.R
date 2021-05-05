# Description of script ---------------------------
# Week 19 TidyTuesday
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-05-04/readme.md
# This script explores the data set for tidytuesday. The final output and clean script can be found in rscript_final.R.

# Data: water

# The amount of water point data being collected is growing rapidly as governments and development partners increasingly monitor water points over time. Without harmonization among these different data sources, the opportunity for learning will be limited, with the true potential of this information remaining untapped.
#By establishing a platform for sharing water point data throughout the global water sector, WPDx adds value to the data already being collected. By bringing together diverse data sets, the water sector can establish an unprecedented understanding of water services.
#Sharing this data has the potential to improve water access for millions of people as a result of better information available to governments, service providers, researchers, NGOs, and others.

# script set up -----------------------------------

# load libraries
library(tidytuesdayR) # tidytuesday package used to get the data
library(tidyverse) # used for EDA and plotting the data
library(viridis) # colour palette

# load data ---------------------------------------

tuesdata <- tt_load(2021, week = 19)
df_water <- tuesdata$water

# transform data -----------------------------------

# explore the data ---------------------------------

glimpse(df_water)
sort(unique(df_water$water_source))
sort(unique(df_water$water_tech))
sort(unique(paste(df_water$water_source, ":", df_water$water_tech)))
sort(unique(df_water$facility_type))
sort(unique(df_water$country_name))
sort(unique(df_water$installer)) # this column is a mess with similar names inputted in slightly different ways
sort(unique(df_water$pay)) # hundreds of different variants
sort(unique(df_water$status)) # thousands of different entries

nrow(sort(unique(paste(df_water$lat_deg, df_water$lon_deg))))

# count of water sources per country and source

df_country_source <- df_water %>%
  group_by(country_name) %>%
  count(water_source, sort = TRUE) %>%
  mutate(water_source = ifelse(is.na(water_source), "Unknown", water_source))

# look at the sources for all countries
df_country_source %>%
  ggplot(aes(x = n, y = water_source, group = country_name)) +
  geom_col() +
  facet_wrap(~country_name)

# look at the sources for one country
df_country_source %>%
  filter(country_name == "Uganda") %>%
  ggplot(aes(x = n, y = fct_reorder(water_source, n))) +
  geom_col()

df_country_source %>%
  ggplot(aes(x = n, group = water_source)) +
  geom_histogram() +
  facet_wrap(~water_source, scales = "free_x")

## Mapping (this data has latitude and longitudes so start with some maps to see if there's anything interesting)

# Plot basemap
world <- map_data("world")

base_map <- world %>%
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group), fill="grey", alpha = 0.3)

# merge data with the shapefile

spdf_country_source <- world %>%
  right_join(df_country_source, by=c("region"="country_name"))

# choropleth map (using Boreholes as an example for now)

base_map +
  geom_polygon(data = spdf_country_source %>% filter(water_source == "Borehole"), aes(fill = n, x = long, y = lat, group = group)) +
  theme_void() +
  scale_fill_viridis(trans = "log",
                     name="Number of Boreholes")

# hexbin map
base_map +
  geom_hex(data = df_water %>% filter(water_source == "Borehole"), aes(x = lon_deg, y = lat_deg)) +
  theme_void() +
  scale_fill_viridis(trans = "log",
                     name="Number of Boreholes",
                     option="B")

# create a hexbin map to look at the proportion of non available water in different locations.

hex_water <- hexbin::hexbin(df_water$lon_deg, df_water$lat_deg,
                             xbins = 30, IDs = TRUE)

hexagons <- data.frame(hexbin::hcell2xy(hex_water),
                       cell = hex_water@cell,
                       count = hex_water@count)

df_water$cell = hex_water@cID

df_water_avail_in_hexes <- df_water %>%
  group_by(cell) %>%
  summarise(total_number_of_sources = n(),
            avail = sum(status_id == "y"),
            noavail = sum(status_id == "n")) %>%
  mutate(prop_avail = avail/total_number_of_sources,
         prop_no_avail = noavail/total_number_of_sources) %>%
  right_join(hexagons, by = "cell") %>%
  select(cell, x, y, prop_no_avail)

base_map +
  geom_hex(aes(x = x, y = y, fill = prop_no_avail),
           stat = "identity", colour = NA, alpha = 0.75,
           data = df_water_avail_in_hexes)

# just look at the values in Ethiopia

df_ethiopia <- df_water %>%
  filter(country_name == "Ethiopia")

base_map +
  geom_point(data = df_ethiopia,
             aes(x = lon_deg,
                 y = lat_deg))
# note there are some latitudes and longitudes that don't look quite right so filter the lats/longs

world %>%
  filter(region == "Ethiopia") %>%
  summarise(min(lat), max(lat), min(long), max(long))

df_ethiopia_clean <- df_ethiopia %>%
  filter(between(lon_deg, 32.99892, 47.97822),
         between(lat_deg, 3.456104, 14.8523))
base_map +
  geom_point(data = df_ethiopia_clean,
             aes(x = lon_deg,
                 y = lat_deg))

hex_ethiopia <- hexbin::hexbin(df_ethiopia_clean$lon_deg,
                               df_ethiopia_clean$lat_deg,
                            xbins = 30, IDs = TRUE)

hexagons <- data.frame(hexbin::hcell2xy(hex_ethiopia),
                       cell = hex_ethiopia@cell,
                       count = hex_ethiopia@count)

df_ethiopia_clean$cell = hex_ethiopia@cID

df_ethiopia_avail_in_hexes <- df_ethiopia_clean %>%
  group_by(cell) %>%
  summarise(total_number_of_sources = n(),
            avail = sum(status_id == "y"),
            noavail = sum(status_id == "n")) %>%
  mutate(prop_avail = avail/total_number_of_sources,
         prop_no_avail = noavail/total_number_of_sources) %>%
  right_join(hexagons, by = "cell") %>%
  select(cell, x, y, prop_no_avail)

base_map_ethiopia <- world %>%
  filter(region == "Ethiopia") %>%
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group), fill="grey", alpha = 0.3)

gg_ethiopia_hex <- base_map_ethiopia +
  # manually remove a couple of hexes which look out of place
#  geom_text(aes(x = x, y = y, label = cell), data = df_ethiopia_avail_in_hexes) +
  geom_hex(aes(x = x, y = y, fill = prop_no_avail),
           stat = "identity", colour = NA, alpha = 0.6,
           data = df_ethiopia_avail_in_hexes %>%
             filter(!cell %in% c(98, 21, 22))) +
  theme_void() +
  scale_fill_viridis(option = "A") +
  theme(legend.position = "bottom")

# for the 5 locations with the highest proportion unavailable what are the types of water source

gg_ethiopia_water_source_bar_chart <- df_ethiopia_clean %>%
  # filter(cell %in% (df_ethiopia_avail_in_hexes %>% dplyr::arrange(-prop_no_avail) %>% top_n(5) %>% select(cell) %>% pull())) %>%
  mutate(water_source = ifelse(is.na(water_source), "Unknown", water_source)) %>%
  group_by(status_id) %>%
  count(water_source) %>%
  ungroup() %>%
  group_by(water_source) %>%
  mutate(total_water_source = sum(n)) %>%
  ggplot(aes(x = n, y = fct_reorder(water_source, total_water_source))) +
  geom_col(aes(fill = status_id)) +
  theme_minimal() +
  scale_fill_viridis_d(direction = -1, option = "A") +
  xlab("Number of water sources") +
  ylab("Water source type") +
  labs(title = "Water source types in Ethiopia",
       subtitle = "This is a count of all recorded water source types, some of which might no longer exist")

# explore the reasons for not available

gg_ethiopia_not_avail_reason <- df_ethiopia_clean %>%
  filter(status_id == "n") %>%
  # filter(cell %in% (df_ethiopia_avail_in_hexes %>% dplyr::arrange(-prop_no_avail) %>% top_n(5) %>% select(cell) %>% pull())) %>%
  mutate(status = ifelse(is.na(status), "Unknown", status)) %>%
  count(status) %>%
  ggplot(aes(x = n, y = fct_reorder(status, n))) +
  geom_col(fill = "#9F2A63FF") +
  theme_minimal() +
  scale_fill_viridis_d(guide = "none") +
  xlab("Count of sources") +
  ylab("Status") +
  labs(title = "Reasons for unavailable water",
       subtitle = "This is a count of all recorded reasons for water being unavailable at source")

# what is the age of source when recorded?

gg_ethiopia_age <- df_ethiopia_clean %>%
  mutate(report_year = lubridate::year(as.Date(report_date, format = "%d/%m/%Y")),
         age = report_year - install_year) %>%
  ggplot() +
  geom_density(aes(x = age, group = status_id, col = status_id), adjust = 5) +
  theme_minimal()

# install year
gg_ethiopia_install <- df_ethiopia_clean %>%
  ggplot() +
  geom_bar(aes(x = install_year, group = status_id, fill = status_id), position = "dodge") +
  theme_minimal()

# reported year
gg_ethiopia_report <- df_ethiopia_clean %>%
  mutate(report_year = lubridate::year(as.Date(report_date, format = "%d/%m/%Y"))) %>%
  ggplot() +
  geom_bar(aes(x = report_year, group = status_id, fill = status_id), position = "dodge") +
  theme_minimal()

# most of the reporting in Ethiopia was done in 2021... filter the data for 2021 and look at the charts

df_ethiopia_clean2021 <- df_ethiopia %>%
  mutate(report_year = lubridate::year(as.Date(report_date, format = "%d/%m/%Y"))) %>%
  filter(between(lon_deg, 32.99892, 47.97822),
         between(lat_deg, 3.456104, 14.8523),
         report_year == "2021")

base_map +
  geom_point(data = df_ethiopia_clean2021,
             aes(x = lon_deg,
                 y = lat_deg))

hex_ethiopia <- hexbin::hexbin(df_ethiopia_clean2021$lon_deg,
                               df_ethiopia_clean2021$lat_deg,
                               xbins = 10, IDs = TRUE)

hexagons <- data.frame(hexbin::hcell2xy(hex_ethiopia),
                       cell = hex_ethiopia@cell,
                       count = hex_ethiopia@count)

df_ethiopia_clean2021$cell = hex_ethiopia@cID

df_ethiopia_avail_in_hexes <- df_ethiopia_clean2021 %>%
  group_by(cell) %>%
  summarise(total_number_of_sources = n(),
            avail = sum(status_id == "y"),
            noavail = sum(status_id == "n")) %>%
  mutate(prop_avail = avail/total_number_of_sources,
         prop_no_avail = noavail/total_number_of_sources) %>%
  right_join(hexagons, by = "cell") %>%
  select(cell, x, y, prop_no_avail)

base_map_ethiopia <- world %>%
  filter(region == "Ethiopia") %>%
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group), fill="grey", alpha = 0.3)

gg_ethiopia_hex <- base_map_ethiopia +
  # manually remove a couple of hexes which look out of place
  #  geom_text(aes(x = x, y = y, label = cell), data = df_ethiopia_avail_in_hexes) +
  geom_hex(aes(x = x, y = y, fill = prop_no_avail),
           stat = "identity", colour = NA, alpha = 0.6,
           data = df_ethiopia_avail_in_hexes) +
  theme_void() +
  scale_fill_viridis(option = "A") +
  theme(legend.position = "bottom") +
  geom_point(data = df_ethiopia_clean2021, aes(x = lon_deg, y = lat_deg))

gg_ethiopia_water_source_bar_chart2021 <- df_ethiopia_clean2021 %>%
  # filter(cell %in% (df_ethiopia_avail_in_hexes %>% dplyr::arrange(-prop_no_avail) %>% top_n(5) %>% select(cell) %>% pull())) %>%
  mutate(water_source = ifelse(is.na(water_source), "Unknown", water_source)) %>%
  group_by(status_id) %>%
  count(water_source) %>%
  ungroup() %>%
  group_by(water_source) %>%
  mutate(total_water_source = sum(n)) %>%
  ggplot(aes(x = n, y = fct_reorder(water_source, total_water_source))) +
  geom_col(aes(fill = status_id)) +
  theme_minimal() +
  scale_fill_viridis_d(direction = -1, option = "A") +
  xlab("Number of water sources") +
  ylab("Water source type") +
  labs(title = "Water source types in Ethiopia",
       subtitle = "This is a count of all recorded water source types, some of which might no longer exist")

gg_ethiopia_not_avail_reason2021 <- df_ethiopia_clean2021 %>%
  filter(status_id == "n") %>%
  # filter(cell %in% (df_ethiopia_avail_in_hexes %>% dplyr::arrange(-prop_no_avail) %>% top_n(5) %>% select(cell) %>% pull())) %>%
  mutate(status = ifelse(is.na(status), "Unknown", status)) %>%
  group_by(water_source) %>%
  count(status) %>%
  ggplot(aes(x = n, y = fct_reorder(status, n))) +
  geom_col(aes(fill = water_source)) +
  theme_minimal() +
  scale_fill_viridis_d() +
  xlab("Count of sources") +
  ylab("Status") +
  labs(title = "Reasons for unavailable water",
       subtitle = "This is a count of all recorded reasons for water being unavailable at source")

### notes

# When looking only at Ethiopia some of the latitude and longitudes didn't look right, for this analysis I just filtered out any suspect locations.
# Reasons for water source type is pretty messy across the entire data set. It was ok and required minimal cleaning for Ethiopia but some text analysis would be required to group similiar entries in the whole data set
# Explore unavailable reasons and water source types together
# Combine with location/population data

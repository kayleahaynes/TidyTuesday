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
library(showtext) # add google fonts
library(grid) # combine final plot in grid
library(gridExtra) # combine final plot in grid
# add fonts for plotting

font_add_google(name = "Anton",
                family = "anton")

font_add_google(name = "Roboto",
                family = "roboto_light",
                regular.wt = 300)

showtext_auto()

# load data ---------------------------------------

tuesdata <- tt_load(2021, week = 19)
df_water <- tuesdata$water

# transform data ---------------------------------------

# filter for water sources recorded in Ethiopia and clean up
# longitude and latitudes that don't look quite right.

world <- map_data("world")

world %>%
  filter(region == "Ethiopia") %>%
  summarise(min(lat), max(lat), min(long), max(long))

df_ethiopia <- df_water %>%
  filter(country_name == "Ethiopia",
         between(lon_deg, 32.99892, 47.97822),
         between(lat_deg, 3.456104, 14.8523))


# plots for TidyTuesday -------------------------------

# hexbin for all Ethiopia looking at the proportion of water sources where water was unavailable on the report date

## define hexes (xbins choosing to be 10 as want a high level view of the data with little missing data)

hex_ethiopia <- hexbin::hexbin(df_ethiopia$lon_deg,
                               df_ethiopia$lat_deg,
                               xbins = 10,
                               IDs = TRUE)

hexagons <- data.frame(hexbin::hcell2xy(hex_ethiopia),
                       cell = hex_ethiopia@cell,
                       count = hex_ethiopia@count)

df_ethiopia$cell = hex_ethiopia@cID

# calculate the proportion of water sources with unavailable water in each hex

df_ethiopia_avail_in_hexes <- df_ethiopia %>%
  group_by(cell) %>%
  summarise(total_number_of_sources = n(),
            avail = sum(status_id == "y"),
            noavail = sum(status_id == "n")) %>%
  mutate(prop_avail = avail/total_number_of_sources,
         prop_no_avail = noavail/total_number_of_sources) %>%
  right_join(hexagons, by = "cell") %>%
  select(cell, x, y, prop_no_avail)

# map the outline of Ethiopia
base_map_ethiopia <- world %>%
  filter(region == "Ethiopia") %>%
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group), fill="grey", alpha = 0.5, col = "grey", size = 1)

# add the hexes to the map
gg_ethiopia_hex <-
  base_map_ethiopia +
  geom_hex(aes(x = x, y = y, fill = prop_no_avail),
           stat = "identity", colour = NA, alpha = 0.6,
           data = df_ethiopia_avail_in_hexes %>%
             filter(!cell %in% c(98, 21, 22))) +
  theme_void() +
  scale_fill_viridis(option = "D",
                     name="Proportion",
                     guide = guide_colorbar(label.position = "bottom", title.position = 'top', barwidth = 10)) +
  labs(title = "Proportion of unavailable water",
       subtitle = "The number of water sources where no water is available when reported / the total number of water sources reported within the regions") +
  theme(plot.title = element_text(family = "anton",
                                  face = "bold",
                                  size = 24,
                                  hjust = 0.5,
                                  margin = margin(10,0,10,0)),
        plot.subtitle = element_text(family = "roboto_light",
                                     size = 18,
                                     hjust = 0.5,
                                     margin = margin(10,0,10,0)),
    legend.position = c(0.8,0.8),
    legend.direction = "horizontal",
    legend.title=element_text(color="black", size=14, family = "roboto_light", hjust = 0.5),
    legend.text = element_text(size=10, family = "roboto_light", hjust = 0.5),
    plot.margin = margin(10,0,20,0)
    )

# bar chart of water sources in Ethiopia

colours <- viridis(20)[c(3,8,13)]

gg_ethiopia_water_source_bar_chart <-
  df_ethiopia %>%
  mutate(water_source = ifelse(is.na(water_source), "Unknown", water_source)) %>%
  group_by(status_id) %>%
  count(water_source) %>%
  ungroup() %>%
  group_by(water_source) %>%
  mutate(total_water_source = sum(n)) %>%
  ggplot(aes(x = n, y = fct_reorder(water_source, total_water_source))) +
  geom_col(aes(fill = status_id)) +
  theme_minimal() +
  scale_fill_manual(values = rev(colours), guide = guide_legend(title = "Water Available?"), labels = c("No", "Unknown", "Yes")) +
  xlab("Number of water sources") +
  ylab("") +
  labs(title = "Water source types",
       subtitle = "This is a count of all recorded water source types") +
    theme(plot.title = element_text(family = "anton",
                                    face = "bold",
                                    size = 18,
                                    hjust = 0.5),
          plot.subtitle = element_text(family = "roboto_light",
                                       size = 16,
                                       hjust = 0.5),
          axis.text = element_text(family = "roboto_light",
                                       size = 14),
          axis.title = element_text(family = "roboto",
                                   size = 14),
          legend.position = c(0.85,0.5),
          legend.direction = "vertical",
          legend.title=element_text(color="black", size=14, family = "roboto_light", hjust = -1),
          legend.text = element_text(size=14, family = "roboto_light"),
          plot.margin = margin(10,0,10,-50)
    )

# bar chart of reasons why the water is unavailable

gg_ethiopia_not_avail_reason <-
  df_ethiopia %>%
  filter(status_id == "n") %>%
  mutate(status = ifelse(is.na(status), "Unknown", status)) %>%
  count(status) %>%
  ggplot(aes(x = n, y = fct_reorder(status, n))) +
  geom_col(fill = "#2D718EFF") +
  theme_minimal() +
  scale_fill_viridis_d(guide = "none") +
  xlab("Count of sources") +
  ylab("") +
  labs(title = "Reasons for unavailable water",
       subtitle = "This is a count of all recorded reasons for water being unavailable at source") +
  theme(plot.title = element_text(family = "anton",
                                  face = "bold",
                                  size = 18,
                                  hjust = 0.5),
        plot.subtitle = element_text(family = "roboto_light",
                                     size = 16,
                                     hjust = 0.5),
        axis.text = element_text(family = "roboto_light",
                                 size = 14),
        axis.title = element_text(family = "roboto",
                                  size = 14),
        legend.position = c(0.95,0.5),
        legend.direction = "vertical",
        legend.title=element_text(color="black", size=14, family = "roboto_light", hjust = -1),
        legend.text = element_text(size=10, family = "roboto_light"),
        plot.margin = margin(10,0,10,-50)
  )
# combine the 3 plots using patchwork
#

gg_ethiopia_water_source_bar_chartA <- ggplotGrob(gg_ethiopia_water_source_bar_chart)
gg_ethiopia_not_avail_reasonA <- ggplotGrob(gg_ethiopia_not_avail_reason)
maxWidth = grid::unit.pmax(gg_ethiopia_water_source_bar_chartA$widths[2:5], gg_ethiopia_not_avail_reasonA$widths[2:5])
gg_ethiopia_water_source_bar_chartA$widths[2:5] <- as.list(maxWidth)
gg_ethiopia_not_avail_reasonA$widths[2:5] <- as.list(maxWidth)

ggsave("week19.png", grid.arrange(gg_ethiopia_hex , gg_ethiopia_water_source_bar_chartA, gg_ethiopia_not_avail_reasonA,
                                  widths = c(1.5,1),
                                  layout_matrix = rbind(c(1, 2),
                                                        c(1, 3)), top = textGrob("Water sources in Ethiopia", gp=gpar(fontfamily ="anton", cex = 6))), width = 28, height = 12, dpi = 320)

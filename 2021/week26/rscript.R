library(tidyverse)
library(janitor)
library(colourvalues)

tuesdata <- tidytuesdayR::tt_load(2021, week = 26)

df_parks <- tuesdata$parks %>%
  janitor::clean_names()

glimpse(df_parks)

# filter the data to be 2020 only

df_parks_2020 <- df_parks %>%
  filter(year == 2020)

df_radar_data <- df_parks_2020 %>%
  mutate(city = paste0(rank, ". ", city)) %>%
  mutate(city = fct_reorder(city, rank)) %>%
  select(city,
         med_park_size_points,
         park_pct_city_points,
         pct_near_park_points,
         spend_per_resident_points,
         amenities_points) %>%
  # put on the same scale
  mutate(med_park_size_points = med_park_size_points * 2,
         park_pct_city_points = park_pct_city_points * 2)

# 1. radar plot using fmsb::radarChart
# I like the way this one formats but due to it being base R it's difficult to align the top plot and plots 2 to 97.

colours <- color_values(df_parks_2020$pct_near_park_points)

df_radar_data2 <-
  rbind(c(100,100,100,100,100),
        rep(0,ncol(df_radar_data[,-1])),
        df_radar_data[1,-1])

par(mfrow=c(1,1))

top_plot_radarchart <- radarchart(df_radar_data2,
           axistype=1, # 0 means no axis label
           seg = 4,  # number of segments on the axis
           pty = 16, # 16 is closed circles
           pcol = colours[1],  # color of the line
           plty = 1, # line type, default as a solid line for now
           plwd = 3, # line width
           pdensity =  NULL,  # filling density of polygons, keep as NULL for now
           pangle = 45, # keep as default for now, unused if pdensity is NULL
           pfcol = scales::alpha(colours[1], 0.5), # fill colour
           cglty = 1, #grid line type
           cglwd = 1, # grid line width
           cglcol = "grey", #grid line colour
           axislabcol =  "grey", #axis label colours (only if axis label is not 0
           title = df_radar_data[1,1]$city, # label plot with city name
           maxmin = TRUE,
           vlabels = c("Median park \n size",
                       "Parkland as % \n of city area",
                       "Percent of residents \n within a 10 minute walk",
                       "Spending per resident",
                       "Amenities"),
           vlcex = 1,,
           cex.main=1
)

par(mar=rep(0.5,4))
par(mfrow=c(12,8))

for(i in 2:97){

  # add a maximum of 100 and minimum of 0 to the data for radar plot
  df_radar_data2 <-
    rbind(rep(100,ncol(df_radar_data[,-1])),
          rep(0,ncol(df_radar_data[,-1])),
          df_radar_data[i,-1])

  # Custom the radarChart !
  radarchart(df_radar_data2,
             axistype=0, # 0 means no axis label
             seg = 4,  # number of segments on the axis
             pty = 16, # 16 is closed circles
             pcol = colours[i],  # color of the line
             plty = 1, # line type, default as a solid line for now
             plwd = 1, # line width, default as 1 for now)
             pdensity =  NULL,  # filling density of polygons, keep as NULL for now
             pangle = 45, # keep as default for now, unused if pdensity is NULL
             pfcol = scales::alpha(colours[i], 0.6), # fill colour
             cglty = 1, #grid line type
             cglwd = 0.5, # grid line width
             cglcol = "grey", #grid line colour
             axislabcol =  "black", #axis label colours (only if axis label is not 0
             title = df_radar_data[i,1]$city, # label plot with city name
             maxmin = TRUE,
             vlabels = rep("", 5), #remove labels (will include a legend instead)
             cex.main=1
  )
}

# 2. radar plot using ggradar
# - can't fill the polygons
# - don't like the cicles

gg_ggradar_toprankings  <- df_radar_data %>%
  filter(city == "1. Minneapolis") %>%
  rename(group = city) %>%
  mutate(group = as.character(group)) %>%
  as_tibble() %>%
  mutate(group = fct_reorder(group, parse_number(group))) %>%
  ggradar::ggradar(
    base.size = 2,
    font.radar = "Montserrat",
    axis.labels = c("Median park \n size",
                    "Parkland as % \n of city area",
                    "Percent of residents \n within a 10 minute walk",
                    "Spending per resident",
                    "Amenities"),
    grid.line.width = 0.2,
    background.circle.transparency = 0.1,
    axis.label.size = 3,
    grid.label.size = 3,
    group.point.size = 2,
    group.line.width = 1,
    group.colours = colours,
    grid.max = 100) +
  facet_wrap(~group) +
  theme_minimal(base_size =12) +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.text = element_blank())

gg_ggradar_otherrankings  <- df_radar_data %>%
  filter(city != "1. Minneapolis") %>%
  rename(group = city) %>%
  mutate(group = as.character(group)) %>%
  as_tibble() %>%
  mutate(group = fct_reorder(group, parse_number(group))) %>%
  ggradar::ggradar(
    grid.max = 100,
    base.size = 2,
    font.radar = "Montserrat",
    axis.labels = c("Median park \n size",
                    "Parkland as % \n of city area",
                    "Percent of residents \n within a 10 minute walk",
                    "Spending per resident",
                    "Amenities"),
    grid.line.width = 0.2,
    background.circle.transparency = 0.1,
    axis.label.size = 0,
    grid.label.size = 0,
    group.point.size = 2,
    group.line.width = 1,
    group.colours = colours) +
  facet_wrap(~group) +
  theme_minimal(base_size =12) +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.text = element_blank())

layout <- "
#BB
ABB
#BB
"
gg_ggradar_toprankings +
gg_ggradar_otherrankings +
  plot_layout(design = layout) +
  plot_annotation(title = 'Radar plots for 2020 Park Rankings using `ggradar::ggradar`')

ggsave("gg_ggradar.png", width = 20, height = 10)

# 3. radar plot using ggRadar
# colour doesn't seem to work for one plot
# can fill the polygons, circle background

gg_ggRadar_topranking <- df_radar_data %>%
  filter(city == "1. Minneapolis") %>%
  rename(group = city) %>%
  mutate(group = as.character(group)) %>%
  rename("Median park \n size" = med_park_size_points,
         "Parkland as % \n of city area" = park_pct_city_points,
         "Percent of residents \n within a 10 minute walk" = pct_near_park_points,
         "Spending per resident" = spend_per_resident_points,
         "Amenities" = amenities_points) %>%
  as_tibble() %>%
  ggiraphExtra::ggRadar(rescale = FALSE, aes(group = group), colour = colours[1]) +
  theme_minimal(base_size =12) +
  labs(title = "1. Minneapolis") +
  theme(legend.position = "none")

gg_ggRadar_otherrankings <- df_radar_data %>%
  filter(city != "1. Minneapolis") %>%
  rename(group = city) %>%
  mutate(group = as.character(group)) %>%
  as_tibble() %>%
  ggiraphExtra::ggRadar(aes(group = group, colour = pct_near_park_points, facet = group)) +
  theme_minimal(base_size =12) +
  theme(axis.text = element_blank()) +
  scale_colour_viridis_c() +
  scale_fill_viridis_c()

gg_ggRadar_topranking + gg_ggRadar_otherrankings +
  plot_layout(design = layout) +
  plot_annotation(title = 'Radar plots for 2020 Park Rankings using `ggiraphExtra::ggRadar`')

ggsave("ggRadar.png", width = 20, height = 10)

# 4. radar plot using ggRadar2
# rescales based on data in data frame so doesn't work with a subset of data
gg_ggradar2_toprankings <- df_radar_data %>%
  filter(city == "1. Minneapolis") %>%
  rename(group = city) %>%
  mutate(group = as.character(group)) %>%
  as_tibble() %>%
  ggradar2::ggradar2(
    radarshape = "sharp",
    base.size = 2,
    axis.labels = c("Median park \n size",
                    "Parkland as % \n of city area",
                    "Percent of residents \n within a 10 minute walk",
                    "Spending per resident",
                    "Amenities"),
    grid.line.width = 0.2,
    background.circle.transparency = 0.1,
    axis.label.size = 0,
    grid.label.size = 0,
    group.point.size = 2,
    group.line.width = 1,
    group.colours = colours,
    grid.max = 100) +
  theme_minimal(base_size =12) +
  theme(legend.position = "none",
        panel.grid = element_blank())

gg_ggradar2_otherrankings <- df_radar_data %>%
  filter(city != "1. Minneapolis") %>%
  rename(group = city) %>%
  mutate(group = as.character(group)) %>%
  as_tibble() %>%
  mutate_at(vars(-group), rescale) %>%
  ggradar2::ggradar2(
    radarshape = "sharp",
    base.size = 2,
    axis.labels = c("Median park \n size",
                    "Parkland as % \n of city area",
                    "Percent of residents \n within a 10 minute walk",
                    "Spending per resident",
                    "Amenities"),
    grid.line.width = 0.2,
    background.circle.transparency = 0.1,
    axis.label.size = 0,
    grid.label.size = 0,
    group.point.size = 2,
    group.line.width = 1,
    group.colours = colours) +
  facet_wrap(~group) +
  theme_minimal(base_size =12) +
  theme(legend.position = "none",
        axis.text = element_blank(),
        panel.grid = element_blank())

gg_ggradar2_toprankings + gg_ggradar2_otherrankings +
  plot_layout(design = layout) +
  plot_annotation(title = 'Radar plots for 2020 Park Rankings using `ggradar2::ggradar2`')

ggsave("gg_ggradar2.png", width = 20, height = 10)

# 5. lollipop chart

top_plot <- df_radar_data %>%
  filter(city == "1. Minneapolis") %>%
  mutate(col = pct_near_park_points) %>%
  rename("Median park \n size" = med_park_size_points,
         "Parkland as % \n of city area" = park_pct_city_points,
         "Percent of residents \n within a 10 minute walk" = pct_near_park_points,
         "Spending per resident" = spend_per_resident_points,
         "Amenities" = amenities_points) %>%
  pivot_longer(cols = -c(city, col),
               names_to = "Criteria",
               values_to = "Points") %>%
  ggplot() +
  geom_segment(aes(x = Criteria, y = 0, xend = Criteria, yend = Points), size = 2, colour = colours[1]) +
  geom_point(aes(x = Criteria, y = Points, size = 2), colour = colours[1]) +
  facet_wrap(~city, ncol = 8, nrow = 12) +
  theme_minimal() +
  labs(x = "Criteria",
       y = "Points") +
  theme(text = element_text(size = 14, family = "Montserrat"),
        axis.text.x = element_text(angle = 45, vjust = 0.7),
        legend.position = "none",
        panel.grid = element_blank())

other_plot <- df_radar_data %>%
  filter(city != "1. Minneapolis") %>%
  mutate(col = pct_near_park_points) %>%
  pivot_longer(cols = -c(city, col),
               names_to = "Criteria",
               values_to = "Points") %>%
  ggplot() +
  # geom_rect(aes(fill = col),xmin = -Inf,xmax = Inf,
  #           ymin = -Inf,ymax = Inf,alpha = 0.3) +
  geom_segment(aes(x = Criteria, y = 0, xend = Criteria, yend = Points, col = col), size = 1) +
  geom_point(aes(x = Criteria, y = Points, col = col), size = 2) +
  facet_wrap(~city,  labeller = label_wrap_gen(width = 20), ncol = 12, nrow = 8) +
  theme_minimal() +
  labs(x = "",
       y = "") +
  theme(text = element_text(family = "Montserrat"),
        axis.text = element_blank(),
        panel.grid = element_blank()) +
  scale_colour_viridis_c() +
  scale_fill_viridis_c()

layout <- "
#BBB
ABBB
#BBB
"

top_plot + other_plot +
  plot_layout(design = layout) +
  plot_annotation(title = 'Lollipop plots for 2020 Park Rankings')

ggsave("gg_lollipop.png", width = 20, height = 10)

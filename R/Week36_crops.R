# description of script --------------------------------------------------------
# TidyTuesday 1st Swptember 2020 - week 36 Global Crop Yields
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-09-01/readme.md
#
#inspired by @AndrewJamesMott, @danielvaisanen (previous endangered flowers plots)
# inputs - : key crop yields
#
# outputs - a plot looking at the regional crop yields

# set up -----------------------------------------------------------------------
message("Set up the script")

library(tidyverse)

# load data --------------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2020, week = 36)

key_crop_yields <- tuesdata$key_crop_yields

# data transformations ---------------------------------------------------------

# filter to only look at the last year of data

key_crop_yields_last <-
  key_crop_yields %>%
  filter(Year == max(Year))

# the data has either country or region name, for this we will use the regions

key_crop_regions <-
  key_crop_yields_last %>%
  filter(is.na(Code)) %>%
  mutate(Entity = gsub('(.{1,20})(\\s|$)', '\\1\n', Entity))


# transform the data for plotting
#
p1 <- ggplot(key_crop_regions) +
  geom_circle(aes(x0 = 5, y0 = 9, r = 0.5, fill = `Wheat (tonnes per hectare)`), color = "grey") +
  geom_circle(aes(x0 = 4.6, y0 = 8.2, r = 0.5, fill = `Rice (tonnes per hectare)`), color = "grey") +
  geom_circle(aes(x0 = 5.4, y0 = 8.2, r = 0.5, fill  = `Maize (tonnes per hectare)`), color = "grey") +
  geom_circle(aes(x0 = 5.4, y0 = 7.6, r = 0.5, fill  = `Soybeans (tonnes per hectare)`), color = "grey") +
  geom_circle(aes(x0 = 4.6, y0 = 7.6, r = 0.5, fill  = `Potatoes (tonnes per hectare)`), color = "grey") +
  geom_circle(aes(x0 = 5.4, y0 = 6.8, r = 0.5, fill  = `Beans (tonnes per hectare)`), color = "grey") +
  geom_circle(aes(x0 = 4.6, y0 = 6.8, r = 0.5, fill  = `Peas (tonnes per hectare)`), color = "grey") +
  geom_circle(aes(x0 = 5.4, y0 = 6, r = 0.5, fill  = `Cassava (tonnes per hectare)`), color = "grey") +
  geom_circle(aes(x0 = 4.6, y0 = 6, r = 0.5, fill  = `Barley (tonnes per hectare)`), color = "grey") +
  geom_circle(aes(x0 = 5.4, y0 = 5.2, r = 0.5, fill  = `Cocoa beans (tonnes per hectare)`), color = "grey") +
  geom_circle(aes(x0 = 4.6, y0 = 5.2, r = 0.5, fill  = `Bananas (tonnes per hectare)`), color = "grey") +
  scale_fill_gradient(
    low = "gold",
    high = "darkgoldenrod3",
    space = "Lab",
    na.value = "white",
    guide = "colourbar",
    aesthetics = "fill"
  ) +
  geom_ellipse(aes(x0 = 5.5, y0 = 4, a = 6, b = 0.3, angle = pi / 3, m1 = 2), fill="#919c4c", color = "transparent") +
  geom_ellipse(aes(x0 = 4.5, y0 = 4, a = 6, b = 0.3, angle = 2*pi / 3, m1 = 2), fill="#919c4c",color = "transparent") +
  ylim(2.5,10) +
  xlim(0,10) +
  facet_wrap(~Entity) +
  theme_void() +
  theme(
    plot.background = element_rect(fill="cornsilk", color = "cornsilk"),
    strip.text.y.left = element_text(angle = 0, size = 10),
    strip.text.x = element_text( size = 8),
    legend.position = "bottom",
    plot.margin = margin(0, 0, 2, 2),
    plot.title = element_text(size = 18,
                              hjust = 0.5),
    plot.subtitle = element_text(size = 14,
                                 hjust = 0.5)) +
  labs(fill = "Tonnes per hectare") +
  ggtitle('Regional Crop Yields', subtitle = "#TidyTuesday week 36")

annotate_crops <- tibble(x1 = c(6.5, 3.5, 6.5, 3.5, 6.5, 3.5, 6.5, 3.5, 6.5, 3.5, 6.5),
                         x2 = c(5, 4.6, 5.4, 4.6, 5.4, 4.6, 5.4, 4.6, 5.6, 4.6, 5.6 ),
                         y1 = c(10, 9, 9, 8, 8, 7, 7, 6, 6, 5, 5),
                         y2 = c(9, 8.4, 8.4, 7.8, 7.8, 7, 7, 6.2, 6.2, 5.4, 5.4),
                         group = c(2,1,2,1,2,1,2,1,2,1,2),
                         label = c("Wheat", "Rice", "Maize", "Soybeans", "Potatoes", "Beans", "Peas", "Cassava", "Barley", "Cocoa Beans", "Bananas"))

p2 <- ggplot(key_crop_regions) +
  geom_circle(aes(x0 = 5, y0 = 9, r = 0.5), fill = "gold1", color = "grey") +
  geom_circle(aes(x0 = 4.6, y0 = 8.2, r = 0.5), fill = "gold1", color = "grey") +
  geom_circle(aes(x0 = 5.4, y0 = 8.2, r = 0.5), fill = "gold1", color = "grey") +
  geom_circle(aes(x0 = 5.4, y0 = 7.6, r = 0.5), fill = "gold1", color = "grey") +
  geom_circle(aes(x0 = 4.6, y0 = 7.6, r = 0.5), fill = "gold1", color = "grey") +
  geom_circle(aes(x0 = 5.4, y0 = 6.8, r = 0.5), fill = "gold1", color = "grey") +
  geom_circle(aes(x0 = 4.6, y0 = 6.8, r = 0.5), fill = "gold1", color = "grey") +
  geom_circle(aes(x0 = 5.4, y0 = 6, r = 0.5), fill = "gold1", color = "grey") +
  geom_circle(aes(x0 = 4.6, y0 = 6, r = 0.5), fill = "gold1", color = "grey") +
  geom_circle(aes(x0 = 5.4, y0 = 5.2, r = 0.5), fill = "gold1", color = "grey") +
  geom_circle(aes(x0 = 4.6, y0 = 5.2, r = 0.5), fill = "gold1", color = "grey") +
  geom_ellipse(aes(x0 = 5.5, y0 = 4, a = 6, b = 0.3, angle = pi / 3, m1 = 2, alpha = 0.1), fill="#919c4c", color = "transparent") +
  geom_ellipse(aes(x0 = 4.5, y0 = 4, a = 6, b = 0.3, angle = 2*pi / 3, m1 = 2, alpha = 0.1), fill="#919c4c",color = "transparent") +
  geom_curve(data = filter(annotate_crops, group == 2),
             aes(x = x1, y = y1,
                 xend = x2, yend = y2),
             curvature = -0.3,
             arrow = arrow(angle = 20, length = unit(0.04, "npc")),
             color = "black") +
  geom_text(data = filter(annotate_crops, group == 2), aes(x = x1+0.5, y = y1+ 0.02, label = label),
            color = "black") +
  geom_curve(data = filter(annotate_crops, group == 1),
             aes(x = x1, y = y1,
                 xend = x2, yend = y2),
             curvature = -0.3,
             arrow = arrow(angle = 20, length = unit(0.04, "npc")),
             color = "black") +
  geom_text(data = filter(annotate_crops, group == 1), aes(x = x1-0.5, y = y1+ 0.02, label = label),
            color = "black") +
  geom_textbox(aes(x= 5, y = 3, label = "Regional crop yields in tonnes per hectare. Each corn kernel represents a different crop. Data from ourworldindata.org/crop-yields."
              ), fill = "cornsilk", box.colour = "cornsilk") +
  ylim(2.5,10.2) +
  xlim(0,10) +
  theme_void() +
  theme(
    plot.background = element_rect(fill="cornsilk", color = "cornsilk"),
    strip.text.y.left = element_text(angle = 0, size = 10),
    strip.text.x = element_text( size = 8),
    legend.position = "none",
    plot.margin = margin(0, 0, 2, 2)
  )


crop_plot <- p1 + p2

ggsave("Week36_crops.png",plot = crop_plot)



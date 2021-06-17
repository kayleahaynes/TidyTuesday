library(readr)
library(extrafont)
library(RColorBrewer)
library(ggtext)
library(tidyverse)
library(gghighlight)
library(patchwork)
library(gridExtra)

devtools::install_github('thomasp85/ggfx')
remotes::install_github("coolbutuseless/ggpattern")

library(ggpattern)

library(ggfx)
library(colorspace)

#font_import()
#loadfonts()

CottonViz_data1 <- read_csv("cottonviz/CottonViz-data1.csv")
names(CottonViz_data1) <- CottonViz_data1[1,]
CottonViz_data1 <- CottonViz_data1[-1,]

classic_line <- CottonViz_data1 %>%
  pivot_longer(c(`US consumption`, Exports, Stocks, `Total supply`)) %>%
  filter(name != "Total supply") %>%
  mutate(value = as.numeric(value),
         Year = as.numeric(Year)) %>%
  ggplot() +
  geom_line(aes(x = Year, y = value, group = name, linetype = name), size = 1, col = "black", alpha = 0.8) +
  scale_linetype_manual(values = c("longdash", "dashed", "solid")) +
  annotate(geom = "text", x = 1945, y = 10500, label = "U.S. Consumption", family = "RoutedGothic", hjust = 0) +
  annotate(
    geom = "curve", x = 1945, y = 10500, xend = 1944.8, yend = 9700, curvature = 0,
   arrow = arrow(length = unit(2, "mm"), type = "closed")
  ) +
  annotate(geom = "text", x = 1944, y = 3000, label = "Exports", family = "RoutedGothic") +
  annotate(
    geom = "curve", x = 1944.3, y = 3000, xend = 1944.5, yend = 2950, curvature = 0,
    arrow = arrow(length = unit(2, "mm"), type = "closed")
  ) +
  annotate(geom = "text", x = 1946, y = 6000, label = "Carry-Over\nStocks", family = "RoutedGothic", hjust = 0.5) +
  annotate(
    geom = "curve", x = 1945.7, y = 6000, xend = 1945.32, yend = 5800, curvature = 0,
    arrow = arrow(length = unit(2, "mm"), type = "closed")
  ) +
   theme(panel.border = element_rect(size = 2, color = "black", fill = NA),
         panel.background = element_rect(fill = "white"),
     text = element_text(family = "RoutedGothic", size = 12),
     legend.position = "none",
         plot.title = element_text(size = 10, hjust = 0.05),
         plot.title.position = "plot",
         panel.grid = element_blank(),
         axis.ticks.length = unit(-0.5, "lines"),
         axis.ticks = element_line(colour = "black", size = c(0,rep(0.5, 5), 0)),
        axis.text.x = element_text(margin=margin(12,5,5,5,"pt")),
         axis.text.y = element_text(margin=margin(5,10,5,5,"pt")),
         axis.text.y.right = element_blank()
         ) +
  labs(x = "",
       y = "",
       title = "Millions of Boles") +
  scale_x_continuous(breaks = 1942:1948,
                   labels = c(1942, paste0("'", 43:48)),
                   expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0, 12000, 2000),
                     labels = seq(0,12,2),
                     expand = c(0,0),
                     limits = c(0,12000),
                     sec.axis = dup_axis())

colours <- brewer.pal(3, "Dark2")

new_line <- CottonViz_data1 %>%
  pivot_longer(c(`US consumption`, Exports, Stocks, `Total supply`)) %>%
  filter(name != "Total supply") %>%
  mutate(value = as.numeric(value),
         Year = as.numeric(Year)) %>%
  ggplot(aes(x = Year, y = value, group = name, color = name, linetype = name)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = colours) +
  theme_minimal() +
  labs(y = "Cotton Supply (Millions of Boles)") +
  theme(legend.position = "none",
        text = element_text(size = 12, family = "Montserrat")) +
  scale_y_continuous(breaks = seq(0, 12000, 2000),
                     labels = seq(0,12,2),
                     expand = c(0,0),
                     limits = c(0,12000)) +
  scale_x_continuous(breaks = 1942:1948,
                     expand = c(0,0))

labels <- data.frame(name = c("Exports", "Stocks", "US consumption"),
                 x = c(1945,1945,1945),
                 y = c(0,0,0))

classic_bar <- CottonViz_data1 %>%
  pivot_longer(c(`US consumption`, Exports, Stocks, `Total supply`)) %>%
  mutate(value = as.numeric(value),
         Year = as.numeric(Year),
         name = factor(name, levels = c("Stocks", "Exports", "US consumption"), ordered = TRUE)) %>%
  filter(name != "Total supply") %>%
  ggplot() +
  geom_col_pattern(aes(x= Year, y = value, pattern = name, pattern_spacing = name, pattern_density = name), color = "black", fill="white", width = 0.7) +
  scale_pattern_manual("", values=c("US consumption"="crosshatch", "Exports"="stripe", "Stocks"="crosshatch")) +
  scale_pattern_spacing_manual(values = c(0.03,0.01,0.015)) +
  scale_pattern_density_manual(values = c(0.05,0.2,0.2)) +
  geom_label(data=data.frame(x=1945, y=7000, label="U.S. CONSUMPTION"), mapping=aes(x=x, y=y, label=label), family="RoutedGothic", hjust=0.5, size=6.5, label.size = 0) +
  geom_label(data=data.frame(x=1945, y=11000, label="Exports"), mapping=aes(x=x, y=y, label=label), family="RoutedGothic", hjust=0.5, size=7, label.size = 0) +
  geom_label(data=data.frame(x=1945, y=15000, label="Stocks*"), mapping=aes(x=x, y=y, label=label), family="RoutedGothic", hjust=0.5, size=7, label.size = 0) +
  theme(panel.border = element_rect(size = 2, color = "black", fill = NA),
        panel.background = element_rect(fill = "white"),
        text = element_text(family = "RoutedGothic", size = 12),
        legend.position = "none",
        plot.title = element_text(size = 10, hjust = 0.05),
        plot.title.position = "plot",
        panel.grid = element_blank(),
        axis.ticks.length.x = unit(0, "lines"),
        axis.ticks.length.y = unit(-1, "lines"),
        axis.ticks = element_line(colour = "black", size = c(0,rep(0.5, 4), 0)),
        axis.text.x = element_text(margin=margin(5,5,5,5,"pt")),
        axis.text.y = element_text(margin=margin(5,15,5,5,"pt")),
        axis.text.y.right = element_blank()
  ) +
  labs(x = "",
       y = "",
       title = "Millions of Boles",
       caption = "* END OF SEASON, JULY 31") +
  scale_x_continuous(breaks = 1942:1948,
                     labels = c(1942, paste0("'", 43:48))) +
  scale_y_continuous(breaks = seq(0, 25000, 5000),
                     labels = seq(0,25,5),
                     expand = c(0,0),
                     limits = c(0,25000))

new_bar <- CottonViz_data1 %>%
  pivot_longer(c(`US consumption`, Exports, Stocks, `Total supply`)) %>%
  mutate(value = as.numeric(value)/1000,
         Year = as.numeric(Year)) %>% filter(name != "Total supply") %>%
  ggplot() +
  geom_bar(aes(x = Year, y = value, fill = name), stat = "identity") +
  scale_fill_manual(values = colours) +
  theme_minimal() +
  theme(plot.title = element_markdown(lineheight = 1.1, hjust = 0.5),
        legend.position = "none",
        text = element_text(size = 12, family = "Montserrat"),
        strip.text = element_blank()) +
  labs(y = "Cotton Supply (Millions of Boles)") +
  scale_x_continuous(breaks = 1942:1948,
                     expand = c(0,0))

experimental_plot <- CottonViz_data1 %>%
  pivot_longer(c(`US consumption`, Exports, Stocks, `Total supply`)) %>%
  mutate(value = as.numeric(value)/1000,
         Year = as.numeric(Year)) %>% filter(name != "Total supply") %>%
  ggplot() +
  geom_bar(data = CottonViz_data1 %>%
             pivot_longer(c(`US consumption`, Exports, Stocks, `Total supply`)) %>%
             mutate(value = as.numeric(value)/1000,
                    Year = as.numeric(Year)) %>% filter(name == "Total supply") %>% rename(name1 = name), aes(x = Year, y = value, fill = name1), stat = "identity", fill = "grey", width = 1) +
  as_reference(
    geom_text(data = labels, aes(label = toupper(name), x = x, y = y, color = name), size = c(30,30,30), hjust = 0.5, vjust = -0.1),
    id = "text") +
  with_blend(
    geom_bar(aes(x = Year, y = value, fill = name), stat = "identity", alpha = 0.8, width = 1, col = NA),
    bg_layer = "text",
    blend_type = "xor") +
  facet_wrap(~name, ncol = 1) +
  theme_minimal() +
  scale_fill_manual(values = colours) +
  scale_colour_manual(values = colours) +
  theme(plot.title = element_markdown(lineheight = 1.1, hjust = 0.5, face = "bold", size = 16),
        legend.position = "none",
        text = element_text(size = 12, family = "Montserrat", color = "white"),
        strip.text = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = '#3C415B'),
        axis.text = element_text(colour = "white")
  ) +
  labs(y = "Cotton Supply (Millions of Boles)",
       title = "Distribution of United States Cotton",
       caption = "Source: U.S. Department of Agriculture") +
  scale_x_continuous(breaks = 1942:1948,
                     expand = c(0,0))


classic_plot <- classic_line + classic_bar  +
  plot_annotation(title = c("MULTIPLE CURVE", "COMPONENT COLUMN"),
                  subtitle = "Distribution of United States Cotton",
                  caption = c("Source: U.S. Department of Agriculture",
                              "U.S. Supply of U.S. Cotton"),
  theme = theme(plot.title = element_text(hjust = c(0, 0.7), size = 12, family = "Caveat"),
                plot.subtitle = element_text(hjust = 0.5, size = 20, family = "Catamaran ExtraBold"),
                plot.caption = element_text(hjust = c(0,1), family = "RoutedGothic")))

new_plot <- new_line + new_bar +
  plot_annotation(title = paste0('**Distribution of United States Cotton**,
    <br> <span style=\'font-size:11pt\'>Quantity of
    <span style=\'color:', colours[1],' ;\'>**Exports**</span>,
    <span style=\'color:', colours[2],' ;\'>**Stocks**</span>, and
    <span style=\'color:', colours[3], ' ;\'>**U.S. Consumption**</span>
    </span>'),
                  caption = "Source: U.S. Department of Agriculture",
                  theme = theme(plot.title = element_markdown(size = 16, hjust = 0.5),
                                text = element_text(family = "Montserrat")))

ggsave("classic_plot.png", classic_plot, width = 12, height = 8, dpi = 300)
ggsave("new_plot.png", new_plot, width = 12, height = 8, dpi = 300)
ggsave("experimental_plot.png", experimental_plot, width = 12, height = 8, dpi = 300)

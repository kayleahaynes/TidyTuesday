# description -------------------------------------------------------------

# TidyTuesday week 37 Formula 1

# set up  -----------------------------------------------------------------

if(!require(pacman)) install.package("pacman")

devtools::install_github("davidsjoberg/ggsankey")

pacman::p_load(tidyverse,
               showtext,
               ggsankey,
               ggalluvial)

font_add_google("")
showtext_auto()

col_pallette <- c("#C30201",
                  "#06CFBA",
                  "#07007D",
                  "#FF7B09",
                  "#FFFFFF",
                  "#FEB800",
                  "#B5323C",
                  "#388748",
                  "#5B819D")

font_col <- c("white",
              "black",
              "white",
              "white",
              "black",
              "black",
              "white",
              "white",
              "white")
# load data  --------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2021, week = 37)

# join results, with drivers and constructors

df_results_dr_co <- tuesdata$results %>%
  left_join(tuesdata$races %>% select("raceId", "year"), by = "raceId") %>%
  left_join(tuesdata$drivers, by = "driverId") %>%
  left_join(tuesdata$constructors, by = "constructorId")

# get the number of points by driver and constructor

df_sankey_data <- df_results_dr_co %>%
  mutate(driver = paste(forename, surname)) %>%
  group_by(driverId, constructorId, driver, name) %>%
  rename(constructor = name) %>%
  summarise(total_points = sum(points)) %>%
  ungroup() %>%
  group_by(driverId) %>%
  filter(total_points >= 300) %>%
  group_by(constructorId) %>%
  mutate(total_constuctor_points = sum(total_points)) %>%
  arrange(desc(total_constuctor_points), desc(total_points)) %>%
 ungroup() %>%
  select(driver, constructor, total_points, total_constuctor_points) %>%
  pivot_longer(cols = c("driver", "constructor"), names_to = "what", values_to = "name")

fct_levels <- data.frame(constructor = levels((factor(df_sankey_data$name[df_sankey_data$what == "constructor"], ordered = TRUE, levels = unique(df_sankey_data$name[df_sankey_data$what == "constructor"]))))) %>%
  mutate(position = 1:n())

df_sankey_data2 <- df_sankey_data %>%
  merge(fct_levels, by.x = "name", by.y = "constructor", all.x = TRUE) %>%
  arrange(desc(total_constuctor_points), desc(total_points), desc(what)) %>%
  mutate(cohort = rep(1:(n()/2), each = 2)) %>%
  mutate(colour = ifelse(what == "driver", col_pallette[lead(position)], "grey60")) %>%
  mutate(colour2 = ifelse(what == "driver", "grey60", col_pallette[position])) %>%
  mutate(font_col = ifelse(what == "driver", "white", font_col[position]))

ragg::agg_png("week37.png", width = 5, height = 5, units = "in", res = 300, scaling = )

df_sankey_data2 %>%
  ggplot(aes(x = fct_rev(what), stratum = name, alluvium = cohort, y = total_points)) +
  geom_alluvium(aes(fill = colour), decreasing = FALSE) +
  scale_fill_identity() +
  geom_stratum(aes(fill= colour2), color = "black", decreasing = FALSE, size = 0.1) +
  geom_text(stat = "stratum", aes(label = name, color = font_col), decreasing = FALSE, size = 5) +
  scale_color_identity() +
  theme_void(base_size = 20) +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#353535"),
        plot.title = element_text(hjust = 0.5, color = "white"),
        plot.subtitle = element_text(hjust = 0.5, color = "white"),
        plot.caption = element_text(hjust = 0.5, color = "white"),
        text = element_text(lineheight = 0.3)) +
  labs(title = "History of Formula 1 (1950 - 2021)",
       subtitle = str_wrap("This alluvial graph links drivers to constructors by looking at where each driver scored their points. Only driver-constructor scores of >= 300 are included for visibility.",60),
       caption = paste("\n\n #TidyTuesday week 37 | dataviz by @kayleahaynes | Source: Ergast API by the way of  Data is Plural \n\n"))

invisible(dev.off())







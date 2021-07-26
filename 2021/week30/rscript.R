# description -------------------------------------------------------------

# TidyTuesday week 30 - Drought in America
# 1. Take a sad plot and make it better (bar, line or scatter)
# - barplot of the % of the state experiencing extreme and exceptional drought in the week commencing 13th July 2021
# - use the "dust" theme from the ggthemr package
# - use gghighlight to highlight the states with > 50 % drought
# - use usmap to create a state map of the states affected and add it to the plot using `inset_element` function from patchwork.

# set up  -----------------------------------------------------------------

if(!require(pacman)) install.package("pacman")

pacman::p_load(tidyverse,
               gghighlight,
               ggthemr,
               usmap,
               patchwork)

# load data  --------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2021, week = 30)
df_drought_raw <- tuesdata$drought

# transform data  ---------------------------------------------------------

df_drought_tidy <- df_drought_raw %>%
  # filter to use only the most recent data
  filter(valid_start == "2021-07-13") %>%
  # filter to only look at Extreme drought and Exceptional drought
  filter(drought_lvl %in% c("D3", "D4")) %>%
  # group by state and then count the total level of drought
  group_by(state_abb) %>%
  mutate(total_drought = sum(area_pct))

# plot a bar chart

# default plot
gg_default <- df_drought_tidy %>%
  filter(total_drought > 0) %>%
  ggplot(aes(x = area_pct, y = fct_reorder(as.factor(state_abb), total_drought))) +
  geom_col(aes(fill = drought_lvl)) +
   labs(title = "It's extremely dry in the West",
        subtitle = str_wrap("The % of state area which is experiencing extreme (D3) and exceptional (D4) drought in the week commencing 13th July 2021.", 90),
     x = "% of state area",
        y = "State abbreviation",
   caption = str_wrap("#TidyTuesday week 30 |
                           dataviz by @kayleahaynes |
                           The U.S. Drought Monitor is jointly produced by the National Drought Mitigation Center at the University of Nebraska-Lincoln, the United States Department of Agriculture, and the National Oceanic and Atmospheric Administration. Map courtesy of NDMC.", 100))


gg_creative <- gg_default +
  geom_vline(aes(xintercept = 50), linetype = "dashed", size = 0.5) +
  gghighlight(total_drought >= 50,
              unhighlighted_params = list(fill = NULL, alpha = 0.5)) +
  guides(fill = guide_legend(title = "Drought level",
                             title.position = "left",
                             title.hjust = 0.5,
                             label.position = "bottom",
                             label.hjust = 0.5,
                             nrow = 1,
                             reverse = TRUE,
                             keyheight = 1,
                             keywidth = 5)) +
  theme(plot.caption = element_text(size = 12, margin = margin(20,0,0,0)),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "top",
        text = element_text(size = 18)) +
  scale_fill_discrete(labels = c("Extreme", "Exceptional"))


# plot the US states which are experiences drought

df_state_plot <- df_drought_tidy %>%
  filter(total_drought > 0) %>%
  rename(state = state_abb) %>%
  mutate(cols = case_when(total_drought >= 50 ~ ">=50%",
                          total_drought >= 25 & total_drought < 50 ~ ">=25%, <50%",
                          TRUE ~ "<25%"))

gg_state <- usmap::plot_usmap(data = df_state_plot, values = "cols", labels = T)  +
  scale_fill_discrete("% of area with extreme or exceptional drought") +
  guides(fill = guide_legend(title.position = "top",
                             title.hjust = 0.5)) +
  theme(legend.position = "bottom",
        legend.background = element_blank(),
        text = element_text(size = 16))

gg_state$layers[[2]]$aes_params$size <- 3

# use patchwork to insert the map onto the barchart

gg_final <- gg_creative +
  inset_element(gg_state, left = 0.55, bottom = -0.3, right = 1, top = 1, align_to = "plot")

ggthemr_reset()
ggsave(plot = gg_default, "default_week30.png", width = 10, height = 8)

ggthemr('dust')
ggsave(plot = gg_final, "week30.png", width = 12, height = 12)

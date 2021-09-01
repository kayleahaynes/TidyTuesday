# description -------------------------------------------------------------

# TidyTuesday week 35 Lemurs

# set up  -----------------------------------------------------------------

if(!require(pacman)) install.package("pacman")

pacman::p_load(tidyverse,
               showtext,
               ggdist,
               colorspace)

font_add_google("Montserrat Alternates")
font_add_google("PT Sans Narrow")

showtext_auto()

# load data  --------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2021, week = 35)

df_lemur <- tuesdata$lemur_data
df_taxonomy <- tuesdata$taxonomy

glimpse(df_lemur)

top_3_taxon <- df_lemur %>%
  select(taxon, dlc_id, dam_age_at_concep_y) %>%
  distinct() %>%
  count(taxon, sort = TRUE) %>%
  top_n(3)

df_age_at_conception <- df_lemur %>%
  filter(taxon %in% top_3_taxon$taxon) %>%
  select(taxon, dlc_id, dam_age_at_concep_y) %>%
  distinct() %>%
  mutate(taxon = ifelse(taxon == "CMED", "CMEAD", taxon)) %>%
  left_join(df_taxonomy %>% select(taxon, common_name)) %>%
  group_by(common_name) %>%
  mutate(median_age = median(dam_age_at_concep_y, na.rm = TRUE),
         count = n(),
         max_age = max(dam_age_at_concep_y, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(common_name = fct_reorder(common_name, median_age),
         common_name = fct_rev(common_name))

# raincloud plots

df_age_at_conception %>%
  ggplot(aes(x = dam_age_at_concep_y, y = common_name, color = taxon, fill = after_scale(desaturate(lighten(color, .4), .4)))) +
  ## add half-violin from {ggdist} package
  ggdist::stat_halfeye(
    ## custom bandwidth
    adjust = .5,
    ## adjust height
    width = .6,
    ## move geom to the right
    justification = -.1,
    ## remove slab interval
    .width = 0,
    point_colour = NA
  ) +
  geom_boxplot(
    width = .12,
    ## remove outliers
    outlier.color = NA ## `outlier.shape = NA` works as well
  ) +
  ggdist::stat_dots(
    side = "left",
    dotsize = .8,
    justification = 1.05,
    binwidth = .3
  )  +
  ## remove white space on the left
  coord_cartesian(ylim = c(1.2, NA)) +
  theme(legend.position = "none") +
  labs(x = "Age (years)",
       y = "Common Name",
       title = str_wrap("Estimated age of female parent at conception of focal animal", 50),
       subtitle = "(Estimated_Concep-Dam_DOB)/365)",
       caption = paste("\n\n #TidyTuesday week 35 | dataviz by @kayleahaynes | Source: Duke Lemur Center \n\n", str_wrap("Lemurs are the most threatened group of mammals on the planet, and 95% of lemur species are at risk of extinction. The Duke Lemur centre's mission is to learn everything they can about lemurs – because the more we learn, the better we can work to save them from extinction. They are endemic only to Madagascar, so it’s essentially a one-shot deal: once lemurs are gone from Madagascar, they are gone from the wild.", 110))) +
  theme(text = element_text(family = "PT Sans Narrow", size = 24, lineheight = 0.4),
        plot.title = element_text(family = "Montserrat Alternates", face = "bold", size = 32),
        plot.subtitle = element_text(family = "PT Sans Narrow", size = 26),
        axis.text.y = element_text(colour = c("#D7907B", "#7B9E87", "#B3679B")),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(colour = "#F1D6B8", size = 0.1),
        panel.grid.minor.x = element_line(colour = "#F1D6B8", size = 0.2)) +
  scale_fill_manual(values = c("#7B9E87", "#D7907B", "#B3679B"), guide = "none") +
  scale_colour_manual(values = c("#7B9E87", "#D7907B", "#B3679B"), guide = "none") +
  geom_text(aes(x = median_age, y = common_name, label = median_age),
                nudge_y = 0.25, size = 8,
            fontface = "bold") +
  geom_text(aes(x = max_age, y = common_name, label = paste0("n = ", count)),
            size = 6)

ggsave("week35.png", width = 5, height = 6, units = "in", dpi = 300)

# Load packages -----------------------------------------------------------------------------------------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(showtext)
library(forcats)

source("2024/theme_kaylea.r")

# Load fonts --------------------------------------------------------------------------------------------------------------------------------------------------

font_add_google("Butcherman", "butcherman")
font_add_google("Metal Mania", "metal_mania")
showtext_auto()

# Colours -----------------------------------------------------------------------------------------------------------------------------------------------------

dark_color <- "#006400"    # Dark shade
light_color <- "#90EE90"   # Lighter shade

# Load data ---------------------------------------------------------------------------------------------------------------------------------------------------

df_monster_movie_genres <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-10-29/monster_movie_genres.csv')
df_monster_movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-10-29/monster_movies.csv')

# EDA/transformations ---------------------------------------------------------------------------------------------------------------------------------------------------

head(df_monster_movie_genres)
head(df_monster_movies)

df_monster_movies_clean <- na.omit(df_monster_movies)

# count the occurences of each genre
df_monster_movies_clean$sorted_genres <- unlist(lapply(df_monster_movies_clean$genres, sort))
genre_counts <- as.data.frame(table(monster_movies$genres))
colnames(genre_counts) <- c("genre", "count")

# find the top occuring genres 
top_genres <- genre_counts %>%
  arrange(desc(count)) %>%
  slice_head(n = 15) %>%
  pull(genre)

df_monster_movies_clean = df_monster_movies_clean[df_monster_movies_clean$genres %in% top_genres,]

df_monster_movies_genre_agg = df_monster_movies_clean %>% 
  group_by(grouped_genres) %>% 
  summarise(count = n(), avg_rating = mean(average_rating))

df_monster_movies_genre_agg$grouped_genres <- fct_reorder(df_monster_movies_genre_agg$grouped_genres, df_monster_movies_genre_agg$count, .desc = FALSE)

# Plot ------------------------------------------------------------------------------------------------------------------------------------------------------------------

ggplot(df_monster_movies_genre_agg, aes(y = grouped_genres, x = count, fill = avg_rating)) +
  geom_bar(stat = "identity") +
  labs(title = "What type of monster movies do people like?", 
        subtitle = "Horror rates better when it is a documentary.",
        x = "Number of films", 
        y = "Genre", 
        fill = "Average Rating",
        caption = "#TidyTuesday 2024 week 44 | dataviz by @kayleahaynes | Source: IMDB") + 
  theme_kaylea(bg_col = "black", 
               text_col = "white", 
               family = "metal_mania", 
               base_size = 50) + 
  theme(plot.title = element_text(family = "butcherman", color = "purple"), 
        legend.position = "right",  
        legend.direction = "vertical") + 
  scale_fill_gradient(low = "lightgreen", high = "#006400")

ggsave(file.path("2024", "week44", "plot.png"),
    width = 12, height = 8)


# description of script --------------------------------------------------------
# TidyTuesday  8th September 2020week 37 Friends
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-09-08/readme.md
#
# inputs - : friends data set
#
# outputs - a network of scene emotions between the main characters
#
# code for faces found in @bob_from_space

library(tidyverse)
library(ggraph)
library(tidygraph)
library(ggimage)
library(patchwork)
# load data ---------------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load('2020-09-08')
tuesdata <- tidytuesdayR::tt_load(2020, week = 37)

friends_text <- tuesdata$friends
friends_emotion <- tuesdata$friends_emotions

main_characters <- data.frame(name = c("Rachel Green",
                     "Monica Geller",
                     "Chandler Bing",
                     "Joey Tribbiani",
                     "Phoebe Buffay",
                     "Ross Geller"))

main_characters$images <-
  c("https://media-s3-us-east-1.ceros.com/editorial-content/images/2019/08/06/823e97e11be833cb07bb356f5f1ec88a/rachel-copy.png",
                  "https://media-s3-us-east-1.ceros.com/editorial-content/images/2019/08/06/453d6c2129da2f9b2ce271f6fe91a812/monica-copy.png",
                  "https://media-s3-us-east-1.ceros.com/editorial-content/images/2019/08/06/54007d55df82f058f3b3632191e272e9/chandler-copy.png",
                  "https://media-s3-us-east-1.ceros.com/editorial-content/images/2019/08/06/faf03d07c388d9b81cd78e7fd5f2b943/joey-copy.png",
                  "https://media-s3-us-east-1.ceros.com/editorial-content/images/2019/08/06/c2df59bbe16d9ff188a9569ff2a44ab2/phoebe-copy.png",
                  "https://media-s3-us-east-1.ceros.com/editorial-content/images/2019/08/06/c8844c2c04b63d7c14f0d17d54b9625d/ross-copy.png")


# transform data ----------------------------------------------------------------
friends_with_emotions <- friends_text %>%
  merge(friends_emotion, by = c("season", "episode", "scene", "utterance"), all.x = TRUE)

# find the most common emotion in a scene
scene_emotion <- friends_with_emotions %>%
  group_by(season, episode, scene) %>%
  count(emotion) %>%
  top_n(1) %>%
  rename(scene_emotion = emotion)

friends_with_scene_emotion <- friends_with_emotions  %>%
  merge(scene_emotion, by = c("season", "episode", "scene"), all.x = TRUE)

main_character_scene_emotions <- friends_with_scene_emotion %>%
  filter(speaker %in% main_characters$name,
         !is.na(emotion))  %>% # removes the scenes with no emotions recorded
  select(season, episode, scene, speaker, scene_emotion) %>%
  distinct() %>%
  arrange(season, episode, scene, scene_emotion, speaker)

# for all of the combinations of the main characters get a count of the number of scenes that they appear in together which have
# the different emotions

scene_emotion_count <- main_character_scene_emotions %>%
  merge(main_character_scene_emotions, by = c("season", "episode", "scene", "scene_emotion")) %>% # merge the data to get all combinations of speakers
  filter(speaker.y > speaker.x) %>% # remove the duplicates
  group_by(scene_emotion, speaker.x, speaker.y) %>%
  count()  %>% # get a count of the scenes where the main characters appear together which have each of the emotions
  arrange(scene_emotion, desc(n))

# a lot of the scenes are Joey and Chandler which suggests they appear in a lot of scenes together so should look at the results as a % of the scenes

total_scenes_by_character_combination <- main_character_scene_emotions %>%
  merge(main_character_scene_emotions, by = c("season", "episode", "scene", "scene_emotion")) %>% # merge the data to get all combinations of speakers
  filter(speaker.y > speaker.x) %>% # remove the duplicates
  group_by(speaker.x, speaker.y) %>%
  count()  %>% # get a count of the scenes where the main characters appear together which have each of the emotions
  arrange(desc(n))

# merge the emotion scene count and the total scene count
scene_merge <- scene_emotion_count %>%
  merge(total_scenes_by_character_combination, by = c("speaker.x", "speaker.y"), all.x = TRUE) %>%
  rename("scene_emotion_count" = n.x,
         "all_count" = n.y) %>%
  mutate("emotion_percent" = scene_emotion_count/all_count) %>%
  arrange(scene_emotion, desc(emotion_percent))

emotion_graph_data <- scene_merge %>%
  rename(to = speaker.x,
         from = speaker.y) %>%
  select(to, from, scene_emotion_count, scene_emotion, emotion_percent) %>%
  group_by(scene_emotion) %>%
  arrange(scene_emotion, emotion_percent) %>%
  mutate(rank = 1:n())%>%
  ungroup() %>%
  mutate(scene_emotion = ifelse(is.na(scene_emotion), "Emotionless", scene_emotion))

character_node <- data.frame(speaker = main_characters,
                             order = 1:6)

emotion_graph = tbl_graph(
  nodes = character_node, edges = emotion_graph_data,
  directed = F, node_key = "speaker")

emotion_plot <- ggraph(emotion_graph, layout = 'auto') +
  geom_edge_link(
    aes(alpha = rank, col = scene_emotion), width = 1) +
  geom_node_point(aes(color = speaker), size = 5) +
  facet_edges(~scene_emotion) +
  ggtitle("The one with the custom fonts and network graphs",
          subtitle = "Each network shows the combinations of characters who have appeared \n in the most scenes with that overall emotion \n relative to the total number of scenes the pair have appeared in together.") +
  theme_void() + # remove plotting grid lines and boxes
  theme(plot.title = element_text(hjust = 0.5, size = 24, family = "Gabriel Weiss' Friends Font", color = "white"),
        plot.subtitle = element_text(hjust = 0.5, size = 12, family = "Gabriel Weiss' Friends Font", color = "white"),
        strip.text.x = element_text(family = "Gabriel Weiss' Friends Font", color = "white"),
        plot.background = element_rect(fill="black", color = "black"),
        legend.position = "none") +
  scale_edge_colour_manual(values = c("wheat3", "yellow", "red", "white", "thistle", "maroon3", "dodgerblue3", "olivedrab3")) +
  scale_color_manual(values = c("#FF4328", "#FFDC00", "#42A2D6", "#9A0006",
                                "#fff580", "#00009E"))

all_interactions_graph_data <- scene_merge %>%
  rename(to = speaker.x,
         from = speaker.y) %>%
  group_by(to, from) %>%
  summarise(scene_emotion_count = sum(scene_emotion_count)) %>%
  arrange(scene_emotion_count) %>%
  ungroup() %>%
  mutate(rank = 1:n())

main_characters$name = factor(main_characters$name, label = character_node$speaker, ordered = TRUE)

label_plot <- ggplot() +
  geom_point(aes(x = 1, y  = c(0, 1, 2, 3, 4, 5), col = main_characters$name), size = 35) +
  xlim(0.8,2) +
  ylim(-0.5,5.5) +
  geom_image(aes(x = 1, y = c(0, 1, 2, 3, 4, 5),
                 image = main_characters$images), size = 0.1, by="width") +
  theme_void() + # remove plotting grid lines and boxes
  theme(strip.text.x = element_text(family = "Gabriel Weiss' Friends Font", color = "white"),
        plot.background = element_rect(fill="black", color = "black"),
        legend.position = "none") +
  scale_color_manual(values = c("#FF4328", "#FFDC00", "#42A2D6", "#9A0006",
                                "#fff580", "#00009E"))+
  scale_size_identity()


layout <- c(
  area(t = 1, l = 0, b = 5, r = 3),
  area(t = 1, l = 2, b = 5, r = 5)
)

network_plot <- label_plot + emotion_plot +
  plot_layout(design = layout)


network_plot

ggsave("Week38_friends.png",plot = network_plot)

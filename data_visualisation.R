library(tidyverse)
library(lubridate)
library(stringr)

csv_url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRkTa6aNQoSCO0K4k09MklEJsfwABqqwqCaCcmmmkJS17FYiNRI3EY33LRvvTI6n2do0NVX8PHcLbg-/pub?gid=45323572&single=true&output=csv"
logged_data <- read_csv(csv_url)

my_youtube_data <- logged_data %>%
  drop_na() %>%
  filter(`How many views does this video have?` >= 0) %>%
  mutate(
    upload_time = dmy_hms(Timestamp),
    day_of_week = wday(upload_time, label = TRUE, abbr = FALSE),
    is_korean_content = str_detect(`What is the title language?`, "Korean"),
    views = as.numeric(`How many views does this video have?`),
    comments = as.numeric(`How many comments in this video?`),
    length_min = as.numeric(`How long is the video? (minutes)`)
  ) %>%
  arrange(desc(views))

plot1_data <- my_youtube_data %>%
  group_by(`What type of video is it?`) %>%
  summarise(avg_views = mean(views, na.rm = TRUE))

plot1 <- ggplot(plot1_data, aes(x = `What type of video is it?`, y = avg_views, fill = `What type of video is it?`)) +
  geom_col() +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Average Views by Video Category",
       subtitle = "Comparison of performance across different VLOG types",
       x = "Video Type", y = "Average Views",
       caption = "Data: My YouTube Observation Log") +
  theme_minimal() +
  guides(fill = "none")

plot2 <- ggplot(my_youtube_data, aes(x = `What is the title language?`, y = views, fill = `What is the title language?`)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("Korean" = "#e74c3c", "English" = "#3498db")) +
  labs(title = "Views Distribution by Title Language",
       x = "Title Language", y = "Total Views") +
  theme_light()


plot3 <- ggplot(my_youtube_data, aes(x = day_of_week, y = views, fill = day_of_week)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_brewer(palette = "Set3") + 
  labs(title = "Audience Reach by Day of the Week",
       subtitle = "Analysis using Timestamp data to identify the best upload days",
       x = "Day of the Week", 
       y = "Total Views",
       caption = "Data processed using {lubridate}") +
  theme_minimal() +
  guides(fill = "none")


ggsave("plot1.png", plot1, width = 8, height = 5)
ggsave("plot2.png", plot2, width = 8, height = 5)
ggsave("plot3.png", plot3, width = 8, height = 5)
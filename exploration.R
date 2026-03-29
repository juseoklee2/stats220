library(tidyverse)

# Read data from the published CSV link
logged_data <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRkTa6aNQoSCO0K4k09MklEJsfwABqqwqCaCcmmmkJS17FYiNRI3EY33LRvvTI6n2do0NVX8PHcLbg-/pub?output=csv")

# Check original data
glimpse(logged_data)

# Rebuild a clean dataset using column positions
latest_data <- tibble(
  Timestamp = logged_data[[1]],
  length_minutes = as.numeric(logged_data[[2]]),
  video_type = as.character(logged_data[[3]]),
  thumbnail_face = as.character(logged_data[[4]]),
  views = as.numeric(logged_data[[5]])
)

# Check cleaned data
glimpse(latest_data)

# --------------------------------------------------
# Summary values
# --------------------------------------------------

mean_length <- mean(latest_data$length_minutes, na.rm = TRUE)
mean_views <- mean(latest_data$views, na.rm = TRUE)
max_views <- max(latest_data$views, na.rm = TRUE)
min_length <- min(latest_data$length_minutes, na.rm = TRUE)

mean_length
mean_views
max_views
min_length

# --------------------------------------------------
# Bar chart 1: video type
# --------------------------------------------------

video_type_counts <- latest_data |>
  count(video_type)

ggplot(video_type_counts, aes(x = video_type, y = n)) +
  geom_col() +
  labs(
    title = "Distribution of Video Types",
    x = "Video Type",
    y = "Number of Videos"
  )

# --------------------------------------------------
# Bar chart 2: thumbnail face
# --------------------------------------------------

thumbnail_face_counts <- latest_data |>
  count(thumbnail_face)

ggplot(thumbnail_face_counts, aes(x = thumbnail_face, y = n)) +
  geom_col() +
  labs(
    title = "Thumbnail Face Appearance",
    x = "Face in Thumbnail",
    y = "Number of Videos"
  )

# --------------------------------------------------
# Final code chosen for the report
# --------------------------------------------------

mean_length <- mean(latest_data$length_minutes, na.rm = TRUE)
mean_views <- mean(latest_data$views, na.rm = TRUE)

video_type_counts <- latest_data |>
  count(video_type)

thumbnail_face_counts <- latest_data |>
  count(thumbnail_face)

ggplot(video_type_counts, aes(x = video_type, y = n)) +
  geom_col() +
  labs(
    title = "Distribution of Video Types",
    x = "Video Type",
    y = "Number of Videos"
  )

ggplot(thumbnail_face_counts, aes(x = thumbnail_face, y = n)) +
  geom_col() +
  labs(
    title = "Thumbnail Face Appearance",
    x = "Face in Thumbnail",
    y = "Number of Videos"
  )
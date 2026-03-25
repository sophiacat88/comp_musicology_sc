library(tidyverse)

basspunk <- read_csv("/Users/sophiacatranis/Downloads/Basspunk copy.csv")

basspunk |>
  summarise(
    mean_speechiness = mean(Speechiness),
    mean_acousticness = mean(Acousticness),
    mean_liveness = mean(Liveness),
    sd_speechiness = sd(Speechiness),
    sd_acousticness = sd(Acousticness),
    sd_liveness = sd(Liveness),
    median_speechiness = median(Speechiness),
    median_acousticness = median(Acousticness),
    median_liveness = median(Liveness),
    mad_speechiness = mad(Speechiness),
    mad_acousticness = mad(Acousticness),
    mad_liveness = mad(Liveness)
  )

awards <-
  bind_rows(
    basspunk |> mutate(Category = "basspunk"),
  )

basspunk |> ggplot(aes(x = Energy)) + geom_histogram(binwidth = 0.1)

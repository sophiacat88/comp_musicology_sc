library(tidyverse)
library(compmus)

flop <- read_csv("/Users/sophiacatranis/Desktop/flop copy.csv")
dumg <- read_csv("/Users/sophiacatranis/Desktop/dumb copy.csv")

bind_rows(
  flop |>
    compmus_wrangle_chroma() |>
    filter(row_number() %% 50L == 0L) |>
    mutate(pitches = map(pitches, compmus_normalise, "manhattan")) |>
    compmus_self_similarity(pitches, "manhattan") |>
    filter(!is.na(d)) |> 
    mutate(d = d / max(d), type = "flop"),
  dumg |>
    compmus_wrangle_timbre() |>
    filter(row_number() %% 50L == 0L) |>
    mutate(timbre = map(timbre, compmus_normalise, "euclidean")) |>
    compmus_self_similarity(timbre, "cosine") |>
    filter(!is.na(d)) |> 
    mutate(d = d / max(d), type = "dumg")
) |>
  mutate() |>
  ggplot(
    aes(
      x = xstart + xduration / 2,
      width = 100 * xduration,
      y = ystart + yduration / 2,
      height = 100 * yduration,
      fill = d
    )
  ) +
  geom_tile() +
  coord_fixed() +
  facet_wrap(~type) +
  scale_fill_viridis_c(guide = "none") +
  theme_classic() +
  labs(x = "", y = "")



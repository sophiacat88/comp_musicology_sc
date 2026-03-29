library(tidyverse)
library(compmus) 

flop_pitch <- read_csv("/Users/sophiacatranis/Desktop/main-portfolio/dataset/flop_pitches.csv")
#flop_timbre <- read_csv("/Users/sophiacatranis/Desktop/main-portfolio/dataset/flop_timbre.csv")
#disorder_pitch <- read_csv("/Users/sophiacatranis/Desktop/main-portfolio/disorder_pitch.csv")
#disorder_timbre <- read_csv("/Users/sophiacatranis/Desktop/main-portfolio/dataset/disorder_timbre copy.csv")


flop_pitch |>
  compmus_wrangle_pitches() |> 
  filter(row_number() %% 50L == 0L) |> 
  mutate(pitches = map(pitches, compmus_normalise, "euclidean")) |>
  compmus_self_similarity(pitches, "cosine") |> 
  ggplot(
    aes(
      x = xstart + xduration / 2,
      width = 50 * xduration,
      y = ystart + yduration / 2,
      height = 50 * yduration,
      fill = d
    )
  ) +
  geom_tile() +
  coord_fixed() +
  scale_fill_viridis_c(guide = "none") +
  theme_classic() +
  labs(x = "", y = "")


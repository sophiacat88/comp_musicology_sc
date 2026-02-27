library(tidyverse)
library(compmus)

# ---- LOAD ----
flop_pitch <- read_csv("/Users/sophiacatranis/Desktop/flop_pitch copy.csv")
flop <- read_csv("/Users/sophiacatranis/Desktop/flop copy.csv")

# ---- FIX SHARPS → FLATS ----
flop_pitch <- flop_pitch |>
  rename(
    Db = `C#`,
    Eb = `D#`,
    Gb = `F#`,
    Ab = `G#`,
    Bb = `A#`
  )

# ---- CHROMA SELF SIMILARITY ----
chroma_ssm <- flop_pitch |>
  compmus_wrangle_chroma() |>
  filter(row_number() %% 50L == 0L) |>
  mutate(chroma = map(chroma, compmus_normalise, "manhattan")) |>
  compmus_self_similarity(chroma, "manhattan") |>
  filter(!is.na(d)) |>
  mutate(d = d / max(d), type = "Chroma")

# ---- TIMBRE SELF SIMILARITY ----
timbre_ssm <- flop |>
  compmus_wrangle() |>
  filter(row_number() %% 50L == 0L) |>
  mutate(timbre = map(timbre, compmus_normalise, "euclidean")) |>
  compmus_self_similarity(timbre, "cosine") |>
  filter(!is.na(d)) |>
  mutate(d = d / max(d), type = "Timbre")

# ---- COMBINE ----
bind_rows(chroma_ssm, timbre_ssm) |>
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

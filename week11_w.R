library(tidyverse)
library(compmus) 


flop_novelty <- read_csv("dataset/flop_novelty.csv")
flop_act <- read_csv("dataset/flop_act.csv")
flop_dft <- read_csv("dataset/flop_dft.csv")


# NOVELTy PLOTS

flop_novelty |>
  ggplot(aes(x = TIME, y = VALUE)) +
  geom_line() +
  xlim(0, 30) +                         # Adjust the limits to the desired time range
  theme_minimal() +
  labs(x = "Time (s)", y = "Novelty")

# ACT PLOTS

flop_act |> 
  pivot_longer(-TIME, names_to = "tempo") |> 
  mutate(tempo = as.numeric(tempo)) |> 
  ggplot(aes(x = TIME, y = tempo, fill = value)) +
  geom_raster() +
  scale_fill_viridis_c(guide = "none") +
  labs(x = "Time (s)", y = "Tempo (BPM)") +
  theme_classic()

# DFT PLOTS

flop_dft |> 
  pivot_longer(-TIME, names_to = "tempo") |> 
  mutate(tempo = as.numeric(tempo)) |> 
  ggplot(aes(x = TIME, y = tempo, fill = value)) +
  geom_raster() +
  scale_fill_viridis_c(guide = "none") +
  labs(x = "Time (s)", y = "Tempo (BPM)") +
  theme_classic()


# # Convert loudness (dB) to energy
# flop_tempogram <- flop_tempogram %>%
#   mutate(energy = 10^(VALUE / 10))
# 
# # Compute novelty function (energy difference)
# flop_tempogram <- flop_tempogram %>%
#   mutate(novelty = c(0, diff(energy)))
# 
# flop_tempogram |>
#   ggplot(aes(x = TIME, y = VALUE)) +
#   geom_line() +
#   xlim(0, 30) +                         # Adjust the limits to the desired time range
#   theme_minimal() +
#   labs(x = "Time (s)", y = "Novelty")


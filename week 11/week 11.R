library(tidyverse)
library(compmus) 


pata_pata <- read_csv("/Users/sophiacatranis/Downloads/pata-pata-novelty.csv", show_col_types = FALSE)

# Inspect parsing problems if needed
problems(pata_pata)

# Convert loudness (dB) to energy
pata_pata <- pata_pata %>%
  mutate(energy = 10^(VALUE / 10))

# Compute novelty function (energy difference)
pata_pata <- pata_pata %>%
  mutate(novelty = c(0, diff(energy)))

pata_pata |>
  ggplot(aes(x = TIME, y = VALUE)) +
  geom_line() +
  xlim(0, 30) +                         # Adjust the limits to the desired time range
  theme_minimal() +
  labs(x = "Time (s)", y = "Novelty")


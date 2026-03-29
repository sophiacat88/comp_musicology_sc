library(tidyverse)
library(compmus)

Basspunk <- read_csv("/Users/sophiacatranis/Downloads/Basspunk copy.csv")
unknownpleasures <- read_csv("/Users/sophiacatranis/Desktop/coding/Unknown_Pleasures.csv")

Corpus <-
  bind_rows(
    Basspunk |> mutate(Category = "Basspunk"),
    unknownpleasures |> mutate(Category = "unknownpleasures")
  )

Corpus |>                    # Start with Corpus
  mutate(
    Mode = ifelse(Mode == 0, "Minor", "Major")
  ) |>
  ggplot(                     # Set up the plot.
    aes(
      x = Valence,
      y = Energy,
      size = Loudness,
      colour = Mode
    )
  ) +
  geom_point() +              # Scatter plot.
  geom_rug(linewidth = 0.1) + # Add 'fringes' to show data distribution.
  geom_text(                  # Add text labels from above.
    aes(
      x = Valence,
      y = Energy,
      label = Label
    ),
    data = 
      tibble(
        Label = c("Wacht op mij", "Soft Spine"),
        Category = c("Basspunk", "Unknownpleasures"),
        Valence = c(0.271, 0.128),
        Energy = c(0.166, 0.997)
      ),
    colour = "black",         # Override colour (not mode here).
    size = 3,                 # Override size (not loudness here).
    hjust = "left",           # Align left side of label with the point.
    vjust = "center",         # Align vertical centre of label with the point.
    nudge_x = 0.04            # Nudge the label slightly right.
  ) +
  facet_wrap(~ Category) +    # Separate charts per playlist.
  scale_x_continuous(         # Fine-tune the x axis.
    limits = c(0, 1),
    breaks = c(0, 0.50, 1),   # Use grid-lines for quadrants only.
    minor_breaks = NULL       # Remove 'minor' grid-lines.
  ) +
  scale_y_continuous(         # Fine-tune the y axis in the same way.
    limits = c(0, 1),
    breaks = c(0, 0.50, 1),
    minor_breaks = NULL
  ) +
  scale_colour_brewer(        # Use the Color Brewer to choose a palette.
    type = "qual",            # Qualitative set.
    palette = "Paired"        # Name of the palette is 'Paired'.
  ) +
  scale_size_continuous(      # Fine-tune the sizes of each point.
    trans = "exp",            # Use an exp transformation to emphasise loud.
    guide = "none"            # Remove the legend for size.
  ) +
  theme_light() +             # Use a simpler theme.
  labs(                       # Make the titles nice.
    x = "Valence",
    y = "Energy",
    colour = "Mode"
  )

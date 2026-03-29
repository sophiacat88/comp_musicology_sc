library(tidyverse) 
library(tidymodels)
library(ggdendro)
library(heatmaply)

library(compmus)

get_conf_mat <- function(fit) {
  outcome <- .get_tune_outcome_names(fit)
  fit |> 
    collect_predictions() |> 
    conf_mat(truth = outcome, estimate = .pred_class)
}  

get_pr <- function(fit) {
  fit |> 
    conf_mat_resampled() |> 
    group_by(Prediction) |> mutate(precision = Freq / sum(Freq)) |> 
    group_by(Truth) |> mutate(recall = Freq / sum(Freq)) |> 
    ungroup() |> filter(Prediction == Truth) |> 
    select(class = Prediction, precision, recall)
}  

unknown_pleasures <- read_csv("/Users/sophiacatranis/Desktop/coding/Unknown_Pleasures.csv")

unknown_pleasures_juice <-
  recipe(
    `Track Name` ~
      Danceability +
      Energy +
      Loudness +
      Speechiness +
      Acousticness +
      Instrumentalness +
      Liveness +
      Valence +
      Tempo +
      `Duration (ms)`,
    data = unknown_pleasures
  ) |>
  step_center(all_predictors()) |>
  step_scale(all_predictors()) |> 
  # step_range(all_predictors()) |> 
  prep(unknown_pleasures |> mutate(`Track Name` = str_trunc(`Track Name`, 36))) |>
  juice() |>
  column_to_rownames("Track Name")

unknown_pleasures_dist <- dist(unknown_pleasures_juice, method = "euclidean")

heatmaply(
  unknown_pleasures_juice,
  hclustfun = hclust,
  hclust_method = "average",  # Change for single, average, or complete linkage.
  dist_method = "euclidean"
)



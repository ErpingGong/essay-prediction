
# Load the necessary libraries
library(dplyr)
library(tidyr)
library(caret)
library(lubridate)
library(nflverse)
qb_regular_season_stats <-
  load_player_stats(seasons = TRUE) %>%
  filter(season_type == "REG" & position == "QB")
# Filter for QB data up to Week 9 of the 2023 season
qb_data <- qb_regular_season_stats %>%
  filter(position == "QB" & week <= 9 & season == 2023)

# Prepare the data for modeling (this assumes 'passing_epa' is the variable we want to predict)
# Replace 'passing_epa' with the correct name of the variable for EPA in your dataset
train_data <- qb_data %>%
  select(player_id, player_name, recent_team, week, passing_epa) %>%
  na.omit() # Remove rows with NAs, especially in the response variable

# Split into training and test sets
set.seed(123)
training_index <- createDataPartition(train_data$passing_epa, p = 0.8, list = FALSE)
train_set <- train_data[training_index, ]
test_set <- train_data[-training_index, ]
train_set

# Train the model - placeholder for model training
# Train the model - placeholder for model training
# Replace 'response_variable' with the actual EPA variable
model <- lm(passing_epa ~ recent_team + week, data = train_set)

# Validate the model
predictions <- predict(model, newdata = test_set)
validation_rmse <- sqrt(mean((predictions - test_set$passing_epa)^2))

print(validation_rmse)

nfl_with_predictions <- cbind(qb_data, predictions) |> as_tibble()

# If you are summarizing predictions by team, use this:
nfl_with_predictions_summary <- nfl_with_predictions |>
  group_by(recent_team) |>
  summarise(mean_prediction = mean(predictions, na.rm = TRUE), count = n()) |>
  ungroup() # Don't forget to ungroup to remove the grouping structure.

# Use knitr::kable() to create a nicely formatted table
knitr::kable(
  nfl_with_predictions_summary,
  col.names = c("Team", "Average Predicted passing_epa", "Number of Predictions"),
  digits = 2,
  format.args = list(big.mark = ",")
)


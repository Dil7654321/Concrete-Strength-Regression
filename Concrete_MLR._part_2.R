rm(list = ls())


# Load necessary libraries
library(ggplot2)
library(plotly)
library(car)
library(stats)

# Assuming the dataset is saved as 'mlr_dataset.csv', load the dataset
mlr_data <- read.csv("C:/Users/Dilshan/Desktop/portfolio projects/MLR and MNLR/concrete_data.csv", header = TRUE)

# Display the first few rows of the dataset
head(mlr_data)

# Perform Multiple Linear Regression
#mlr_model <- lm(concrete_compressive_strength ~ cement + blast_furnace_slag + fly_ash + water + superplasticizer + coarse_aggregate + fine_aggregate + age, data = mlr_data)
mlr_model <- lm(concrete_compressive_strength ~ cement + blast_furnace_slag + fly_ash + water + superplasticizer + age, data = mlr_data)
#mlr_model <- lm(concrete_compressive_strength ~ age, data = mlr_data)

# Display the summary of the regression model
summary(mlr_model)
summary(mlr_data)

mlr_data$predicted_strength <- predict(mlr_model,mlr_data)

ggplot(mlr_data, aes(x = concrete_compressive_strength, y = predicted_strength)) +
  geom_point(alpha = .4) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Actual vs Predicted Concrete Strength",
       x = "Actual Concrete Compressive Strength",
       y = "Predicted Concrete Compressive Strength") +
  theme_minimal()


correlation <- cor(mlr_data$concrete_compressive_strength, mlr_data$predicted_strength)
print(correlation)


# Function to calculate R-squared for individual linear models
calculate_r_squared <- function(variable, response) {
  model <- lm(response ~ variable, data = mlr_data)
  rss <- sum(residuals(model)^2)
  tss <- sum((response - mean(response))^2)
  r_squared <- 1 - (rss / tss)
  return(r_squared)
}

# Calculate R-squared for each variable
r_squared_cement <- calculate_r_squared(mlr_data$cement, mlr_data$concrete_compressive_strength)
r_squared_blast_furnace_slag <- calculate_r_squared(mlr_data$blast_furnace_slag, mlr_data$concrete_compressive_strength)
r_squared_fly_ash <- calculate_r_squared(mlr_data$fly_ash, mlr_data$concrete_compressive_strength)
r_squared_water <- calculate_r_squared(mlr_data$water, mlr_data$concrete_compressive_strength)
r_squared_superplasticizer <- calculate_r_squared(mlr_data$superplasticizer, mlr_data$concrete_compressive_strength)
r_squared_coarse_aggregate <- calculate_r_squared(mlr_data$coarse_aggregate, mlr_data$concrete_compressive_strength)
r_squared_fine_aggregate <- calculate_r_squared(mlr_data$fine_aggregate, mlr_data$concrete_compressive_strength)
r_squared_age <- calculate_r_squared(mlr_data$age, mlr_data$concrete_compressive_strength)

# Print the R-squared values for each variable
cat("R-squared for Cement: ", r_squared_cement, "\n")
cat("R-squared for Blast Furnace Slag: ", r_squared_blast_furnace_slag, "\n")
cat("R-squared for Fly Ash: ", r_squared_fly_ash, "\n")
cat("R-squared for Water: ", r_squared_water, "\n")
cat("R-squared for Superplasticizer: ", r_squared_superplasticizer, "\n")
cat("R-squared for coarse_aggregate: ", r_squared_coarse_aggregate, "\n")
cat("R-squared for fine_aggregate: ", r_squared_fine_aggregate, "\n")
cat("R-squared for Age: ", r_squared_age, "\n")

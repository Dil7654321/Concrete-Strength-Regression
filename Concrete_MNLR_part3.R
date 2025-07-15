# Clear environment
rm(list = ls())

# Load necessary libraries
library(ggplot2)

# Load the dataset
data1 <- read.csv("C:/Users/Dilshan/Desktop/portfolio projects/MLR and MNLR/concrete_data.csv", header = TRUE)

# 1. Polynomial regression (quadratic) using nls() for Cement
poly_cement <- nls(concrete_compressive_strength ~ a * cement^2 + b * cement + c, 
                   data = data1, start = list(a = 1, b = 1, c = 1))

# 2. Polynomial regression (quadratic) using nls() for Blast Furnace Slag
poly_blast_furnace_slag <- nls(concrete_compressive_strength ~ a * blast_furnace_slag^2 + b * blast_furnace_slag + c, 
                               data = data1, start = list(a = 1, b = 1, c = 1))

# 3. Logarithmic regression using nls() for Fly Ash
log_fly_ash <- nls(concrete_compressive_strength ~ a * log(fly_ash) + b, 
                   data = data1[data1$fly_ash > 0, ], start = list(a = 1, b = 1))

# 4. Polynomial regression (quadratic) using nls() for Water
poly_water <- nls(concrete_compressive_strength ~ a * water^2 + b * water + c, 
                  data = data1, start = list(a = 1, b = 1, c = 1))

# 5. Polynomial regression (quadratic) using nls() for Superplasticizer
poly_superplasticizer <- nls(concrete_compressive_strength ~ a * superplasticizer^2 + b * superplasticizer + c, 
                             data = data1, start = list(a = 1, b = 1, c = 1))

# 6. Polynomial regression (quadratic) using nls() for Coarse Aggregate
poly_coarse_aggregate <- nls(concrete_compressive_strength ~ a * coarse_aggregate^2 + b * coarse_aggregate + c, 
                             data = data1, start = list(a = 1, b = 1, c = 1))

# 7. Polynomial regression (quadratic) using nls() for Fine Aggregate
poly_fine_aggregate <- nls(concrete_compressive_strength ~ a * fine_aggregate^2 + b * fine_aggregate + c, 
                           data = data1, start = list(a = 1, b = 1, c = 1))

# 8. Logarithmic regression using nls() for Age
log_age <- nls(concrete_compressive_strength ~ a * log(age) + b, 
               data = data1, start = list(a = 1, b = 1))

# Predict values for each model
data1$pred_cement <- predict(poly_cement, data1)
data1$pred_blast_furnace_slag <- predict(poly_blast_furnace_slag, data1)
data1$pred_fly_ash <- NA
data1$pred_fly_ash[data1$fly_ash > 0] <- predict(log_fly_ash, data1[data1$fly_ash > 0, ])
data1$pred_water <- predict(poly_water, data1)
data1$pred_superplasticizer <- predict(poly_superplasticizer, data1)
data1$pred_coarse_aggregate <- predict(poly_coarse_aggregate, data1)
data1$pred_fine_aggregate <- predict(poly_fine_aggregate, data1)
data1$pred_age <- predict(log_age, data1)

# Plot actual vs predicted for each model
ggplot(data1, aes(x = cement)) +
  geom_point(aes(y = concrete_compressive_strength), color = "blue", size = 1) +
  geom_line(aes(y = pred_cement), color = "red", linetype = "dashed", size = 1) +
  labs(title = "Cement vs Concrete Compressive Strength",
       x = "Cement",
       y = "Concrete Compressive Strength") +
  theme_minimal()

ggplot(data1, aes(x = blast_furnace_slag)) +
  geom_point(aes(y = concrete_compressive_strength), color = "blue", size = 1) +
  geom_line(aes(y = pred_blast_furnace_slag), color = "red", linetype = "dashed", size = 1) +
  labs(title = "Blast Furnace Slag vs Concrete Compressive Strength",
       x = "Blast Furnace Slag",
       y = "Concrete Compressive Strength") +
  theme_minimal()

ggplot(data1[data1$fly_ash > 0, ], aes(x = fly_ash)) +
  geom_point(aes(y = concrete_compressive_strength), color = "blue", size = 1) +
  geom_line(aes(y = pred_fly_ash), color = "purple", linetype = "dashed", size = 1) +
  labs(title = "Fly Ash vs Concrete Compressive Strength",
       x = "Fly Ash",
       y = "Concrete Compressive Strength") +
  theme_minimal()

ggplot(data1, aes(x = water)) +
  geom_point(aes(y = concrete_compressive_strength), color = "blue", size = 1) +
  geom_line(aes(y = pred_water), color = "red", linetype = "dashed", size = 1) +
  labs(title = "Water vs Concrete Compressive Strength",
       x = "Water",
       y = "Concrete Compressive Strength") +
  theme_minimal()

ggplot(data1, aes(x = superplasticizer)) +
  geom_point(aes(y = concrete_compressive_strength), color = "blue", size = 1) +
  geom_line(aes(y = pred_superplasticizer), color = "red", linetype = "dashed", size = 1) +
  labs(title = "Superplasticizer vs Concrete Compressive Strength",
       x = "Superplasticizer",
       y = "Concrete Compressive Strength") +
  theme_minimal()

ggplot(data1, aes(x = coarse_aggregate)) +
  geom_point(aes(y = concrete_compressive_strength), color = "blue", size = 1) +
  geom_line(aes(y = pred_coarse_aggregate), color = "red", linetype = "dashed", size = 1) +
  labs(title = "Coarse Aggregate vs Concrete Compressive Strength",
       x = "Coarse Aggregate",
       y = "Concrete Compressive Strength") +
  theme_minimal()

ggplot(data1, aes(x = fine_aggregate)) +
  geom_point(aes(y = concrete_compressive_strength), color = "blue", size = 1) +
  geom_line(aes(y = pred_fine_aggregate), color = "red", linetype = "dashed", size = 1) +
  labs(title = "Fine Aggregate vs Concrete Compressive Strength",
       x = "Fine Aggregate",
       y = "Concrete Compressive Strength") +
  theme_minimal()

ggplot(data1, aes(x = age)) +
  geom_point(aes(y = concrete_compressive_strength), color = "blue", size = 1) +
  geom_line(aes(y = pred_age), color = "purple", linetype = "dashed", size = 1) +
  labs(title = "Age vs Concrete Compressive Strength",
       x = "Age",
       y = "Concrete Compressive Strength") +
  theme_minimal()

# Calculate RSS and R-squared for each model
calculate_r_squared <- function(model, data, response) {
  rss <- sum(residuals(model)^2)
  tss <- sum((response - mean(response))^2)
  return(1 - (rss / tss))
}

r_squared_cement <- calculate_r_squared(poly_cement, data1, data1$concrete_compressive_strength)
r_squared_blast_furnace_slag <- calculate_r_squared(poly_blast_furnace_slag, data1, data1$concrete_compressive_strength)
r_squared_fly_ash <- calculate_r_squared(log_fly_ash, data1[data1$fly_ash > 0, ], data1$concrete_compressive_strength)
r_squared_water <- calculate_r_squared(poly_water, data1, data1$concrete_compressive_strength)
r_squared_superplasticizer <- calculate_r_squared(poly_superplasticizer, data1, data1$concrete_compressive_strength)
r_squared_coarse_aggregate <- calculate_r_squared(poly_coarse_aggregate, data1, data1$concrete_compressive_strength)
r_squared_fine_aggregate <- calculate_r_squared(poly_fine_aggregate, data1, data1$concrete_compressive_strength)
r_squared_age <- calculate_r_squared(log_age, data1, data1$concrete_compressive_strength)

# Print R-squared values
r_squared_cement
r_squared_blast_furnace_slag
r_squared_fly_ash
r_squared_water
r_squared_superplasticizer
r_squared_coarse_aggregate
r_squared_fine_aggregate
r_squared_age

# Clear environment
rm(list = ls())

# Load necessary libraries
library(ggplot2)

# Load the dataset
data1 <- read.csv("C:/Users/Dilshan/Desktop/portfolio projects/MLR and MNLR/concrete_data.csv", header = TRUE)

# Create a regression model with linear cement, polynomial blast_furnace_slag, 
# polynomial water, linear superplasticizer, log age, polynomial fine_aggregate, 
# and polynomial coarse_aggregate
reg_model <- nls(concrete_compressive_strength ~ a * cement + 
                   b * blast_furnace_slag^2 + c * blast_furnace_slag + 
                   d * water^2 + e * water + 
                   f * superplasticizer + 
                   g * log(age) + 
                   h * fine_aggregate^2 + i * fine_aggregate + 
                   j * coarse_aggregate^2 + k * coarse_aggregate +
                   l, 
                 data = data1, start = list(a = 1, b = 1, c = 1, d = 1, e = 1, f = 1, g = 1, h = 1, i = 1, j = 1, k = 1, l = 1))

# Summary of the model
summary(reg_model)

# Predict values from the model
data1$pred_strength <- predict(reg_model, data1)

# Plot actual vs predicted values
ggplot(data1, aes(x = concrete_compressive_strength, y = pred_strength)) +
  geom_point(color = "blue", size = 1) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Actual vs Predicted Concrete Compressive Strength",
       x = "Actual Concrete Compressive Strength",
       y = "Predicted Concrete Compressive Strength") +
  theme_minimal()

# Calculate Residual Sum of Squares (RSS) and Total Sum of Squares (TSS)
rss <- sum(residuals(reg_model)^2)
tss <- sum((data1$concrete_compressive_strength - mean(data1$concrete_compressive_strength))^2)

# Calculate R-squared value
r_squared <- 1 - (rss / tss)
print(paste("R-squared: ", r_squared))

# Calculate the correlation between predicted and actual values
correlation <- cor(data1$concrete_compressive_strength, data1$pred_strength)
print(paste("Correlation: ", correlation))

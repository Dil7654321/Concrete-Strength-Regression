rm(list = ls())
# Load necessary libraries
library(ggplot2)

data1 <- read.csv("C:/Users/Dilshan/Desktop/portfolio projects/MLR and MNLR/concrete_data.csv", header = TRUE)


# 1. Polynomial regression (quadratic) using nls()
poly_model <- nls(concrete_compressive_strength ~ a * cement^2 + b * cement + c, 
                  data = data1, start = list(a = 1, b = 1, c = 1))

# 2. Exponential regression using nls()
exp_model <- nls(concrete_compressive_strength ~ a * exp(b * cement), 
                 data = data1, start = list(a = 1, b = 0.01))

# 3. Logarithmic regression using nls()
log_model <- nls(concrete_compressive_strength ~ a * log(cement) + b, 
                 data = data1, start = list(a = 1, b = 1))

# Summary of each model
summary(poly_model)
summary(exp_model)
summary(log_model)

# Predict values from each model
data1$pred_poly <- predict(poly_model, data1)
data1$pred_exp <- predict(exp_model, data1)
data1$pred_log <- predict(log_model, data1)

# 4. Plot actual vs predicted for each model
ggplot(data1, aes(x = cement)) +
  geom_point(aes(y = concrete_compressive_strength), color = "blue", size = 1) +
  geom_line(aes(y = pred_poly), color = "red", linetype = "dashed", size = 1) +
  geom_line(aes(y = pred_exp), color = "green", linetype = "dashed", size = 1) +
  geom_line(aes(y = pred_log), color = "purple", linetype = "dashed", size = 1) +
  labs(title = "Cement vs Concrete Compressive Strength",
       x = "Cement",
       y = "Concrete Compressive Strength") +
  theme_minimal()

# 5. Calculate Residual Sum of Squares (RSS) and R-squared for each model
rss_poly <- sum(residuals(poly_model)^2)
rss_exp <- sum(residuals(exp_model)^2)
rss_log <- sum(residuals(log_model)^2)
tss <- sum((data1$concrete_compressive_strength - mean(data1$concrete_compressive_strength))^2)

r_squared_poly <- 1 - (rss_poly / tss)
r_squared_exp <- 1 - (rss_exp / tss)
r_squared_log <- 1 - (rss_log / tss)

# Print R-squared values for comparison
r_squared_poly
r_squared_exp
r_squared_log






rm(list = ls())
# Load necessary libraries
library(ggplot2)

data1 <- read.csv("C:/Users/Dilshan/Desktop/BUAN 6357/concrete_data.csv", header = TRUE)


# 1. Polynomial regression (quadratic) using nls() for blast_furnace_slag
poly_model <- nls(concrete_compressive_strength ~ a * blast_furnace_slag^2 + b * blast_furnace_slag + c, 
                  data = data1, start = list(a = 1, b = 1, c = 1))

# 2. Exponential regression using nls() for blast_furnace_slag
exp_model <- nls(concrete_compressive_strength ~ a * exp(b * blast_furnace_slag), 
                 data = data1, start = list(a = 1, b = 0.01))

# 3. Filter out rows where blast_furnace_slag is zero or negative for logarithmic regression
data_log <- data1[data1$blast_furnace_slag > 0, ]

# 3. Logarithmic regression using nls() for blast_furnace_slag
log_model <- nls(concrete_compressive_strength ~ a * log(blast_furnace_slag) + b, 
                 data = data_log, start = list(a = 1, b = 1))

# Summary of each model
summary(poly_model)
summary(exp_model)
summary(log_model)

# Predict values from each model
data1$pred_poly <- predict(poly_model, data1)
data1$pred_exp <- predict(exp_model, data1)
data_log$pred_log <- predict(log_model, data_log)

# 4. Plot actual vs predicted for each model
ggplot() +
  geom_point(data = data1, aes(x = blast_furnace_slag, y = concrete_compressive_strength), color = "blue", size = 1) +
  geom_line(data = data1, aes(x = blast_furnace_slag, y = pred_poly), color = "red", linetype = "dashed", size = 1) +
  geom_line(data = data1, aes(x = blast_furnace_slag, y = pred_exp), color = "green", linetype = "dashed", size = 1) +
  geom_line(data = data_log, aes(x = blast_furnace_slag, y = pred_log), color = "purple", linetype = "dashed", size = 1) +
  labs(title = "Blast Furnace Slag vs Concrete Compressive Strength",
       x = "Blast Furnace Slag",
       y = "Concrete Compressive Strength") +
  theme_minimal()

# 5. Calculate Residual Sum of Squares (RSS) and R-squared for each model

# For polynomial regression
rss_poly <- sum(residuals(poly_model)^2)

# For exponential regression
rss_exp <- sum(residuals(exp_model)^2)

# For logarithmic regression (based on filtered data)
rss_log <- sum(residuals(log_model)^2)

# Total sum of squares (TSS) for all models (based on the full data)
tss <- sum((data1$concrete_compressive_strength - mean(data1$concrete_compressive_strength))^2)

# Calculate R-squared for each model
r_squared_poly <- 1 - (rss_poly / tss)
r_squared_exp <- 1 - (rss_exp / tss)

# TSS for log model needs to be calculated using the filtered data
tss_log <- sum((data_log$concrete_compressive_strength - mean(data_log$concrete_compressive_strength))^2)
r_squared_log <- 1 - (rss_log / tss_log)

# Print R-squared values for comparison
r_squared_poly
r_squared_exp
r_squared_log






rm(list = ls())
# Load necessary libraries
library(ggplot2)

data1 <- read.csv("C:/Users/Dilshan/Desktop/BUAN 6357/concrete_data.csv", header = TRUE)


# 1. Polynomial regression (quadratic) using nls() for fly_ash
poly_model <- nls(concrete_compressive_strength ~ a * fly_ash^2 + b * fly_ash + c, 
                  data = data1, start = list(a = 1, b = 1, c = 1))

# 2. Exponential regression using nls() for fly_ash
exp_model <- nls(concrete_compressive_strength ~ a * exp(b * fly_ash), 
                 data = data1, start = list(a = 1, b = 0.01))

# 3. Filter out rows where fly_ash is zero or negative for logarithmic regression
data_log <- data1[data1$fly_ash > 0, ]

# 3. Logarithmic regression using nls() for fly_ash
log_model <- nls(concrete_compressive_strength ~ a * log(fly_ash) + b, 
                 data = data_log, start = list(a = 1, b = 1))

# Summary of each model
summary(poly_model)
summary(exp_model)
summary(log_model)

# Predict values from each model
data1$pred_poly <- predict(poly_model, data1)
data1$pred_exp <- predict(exp_model, data1)
data_log$pred_log <- predict(log_model, data_log)

# Plot actual vs predicted for each model
ggplot() +
  geom_point(data = data1, aes(x = fly_ash, y = concrete_compressive_strength), color = "blue", size = 1) +
  geom_line(data = data1, aes(x = fly_ash, y = pred_poly), color = "red", linetype = "dashed", size = 1) +
  geom_line(data = data1, aes(x = fly_ash, y = pred_exp), color = "green", linetype = "dashed", size = 1) +
  geom_line(data = data_log, aes(x = fly_ash, y = pred_log), color = "purple", linetype = "dashed", size = 1) +
  labs(title = "Fly Ash vs Concrete Compressive Strength",
       x = "Fly Ash",
       y = "Concrete Compressive Strength") +
  theme_minimal()

# Calculate RSS and R-squared for each model
rss_poly <- sum(residuals(poly_model)^2)
rss_exp <- sum(residuals(exp_model)^2)
rss_log <- sum(residuals(log_model)^2)

tss <- sum((data1$concrete_compressive_strength - mean(data1$concrete_compressive_strength))^2)

r_squared_poly <- 1 - (rss_poly / tss)
r_squared_exp <- 1 - (rss_exp / tss)

tss_log <- sum((data_log$concrete_compressive_strength - mean(data_log$concrete_compressive_strength))^2)
r_squared_log <- 1 - (rss_log / tss_log)

# Print R-squared values
r_squared_poly
r_squared_exp
r_squared_log















rm(list = ls())
# Load necessary libraries
library(ggplot2)

data1 <- read.csv("C:/Users/Dilshan/Desktop/BUAN 6357/concrete_data.csv", header = TRUE)




# 1. Polynomial regression (quadratic) using nls() for superplasticizer
poly_model <- nls(concrete_compressive_strength ~ a * superplasticizer^2 + b * superplasticizer + c, 
                  data = data1, start = list(a = 1, b = 1, c = 1))

# 2. Exponential regression using nls() for superplasticizer
exp_model <- nls(concrete_compressive_strength ~ a * exp(b * superplasticizer), 
                 data = data1, start = list(a = 1, b = 0.01))

# 3. Filter out rows where superplasticizer is zero or negative for logarithmic regression
data_log <- data1[data1$superplasticizer > 0, ]

# 3. Logarithmic regression using nls() for superplasticizer
log_model <- nls(concrete_compressive_strength ~ a * log(superplasticizer) + b, 
                 data = data_log, start = list(a = 1, b = 1))

# Summary of each model
summary(poly_model)
summary(exp_model)
summary(log_model)

# Predict values from each model
data1$pred_poly <- predict(poly_model, data1)
data1$pred_exp <- predict(exp_model, data1)
data_log$pred_log <- predict(log_model, data_log)

# Plot actual vs predicted for each model
ggplot() +
  geom_point(data = data1, aes(x = superplasticizer, y = concrete_compressive_strength), color = "blue", size = 1) +
  geom_line(data = data1, aes(x = superplasticizer, y = pred_poly), color = "red", linetype = "dashed", size = 1) +
  geom_line(data = data1, aes(x = superplasticizer, y = pred_exp), color = "green", linetype = "dashed", size = 1) +
  geom_line(data = data_log, aes(x = superplasticizer, y = pred_log), color = "purple", linetype = "dashed", size = 1) +
  labs(title = "Superplasticizer vs Concrete Compressive Strength",
       x = "Superplasticizer",
       y = "Concrete Compressive Strength") +
  theme_minimal()

# Calculate RSS and R-squared for each model
rss_poly <- sum(residuals(poly_model)^2)
rss_exp <- sum(residuals(exp_model)^2)
rss_log <- sum(residuals(log_model)^2)

tss <- sum((data1$concrete_compressive_strength - mean(data1$concrete_compressive_strength))^2)

r_squared_poly <- 1 - (rss_poly / tss)
r_squared_exp <- 1 - (rss_exp / tss)

tss_log <- sum((data_log$concrete_compressive_strength - mean(data_log$concrete_compressive_strength))^2)
r_squared_log <- 1 - (rss_log / tss_log)

# Print R-squared values
r_squared_poly
r_squared_exp
r_squared_log










rm(list = ls())
# Load necessary libraries
library(ggplot2)

data1 <- read.csv("C:/Users/Dilshan/Desktop/BUAN 6357/concrete_data.csv", header = TRUE)



# Polynomial regression (quadratic) using nls() for coarse_aggregate
poly_model <- nls(concrete_compressive_strength ~ a * coarse_aggregate^2 + b * coarse_aggregate + c, 
                  data = data1, start = list(a = 1, b = 1, c = 1))

# Exponential regression using nls() for coarse_aggregate
exp_model <- nls(concrete_compressive_strength ~ a * exp(b * coarse_aggregate), 
                 data = data1, start = list(a = 1, b = 0.01))

# Logarithmic regression using nls() for coarse_aggregate
log_model <- nls(concrete_compressive_strength ~ a * log(coarse_aggregate) + b, 
                 data = data1, start = list(a = 1, b = 1))

# Summary of each model
summary(poly_model)
summary(exp_model)
summary(log_model)

# Predict values from each model
data1$pred_poly <- predict(poly_model, data1)
data1$pred_exp <- predict(exp_model, data1)
data1$pred_log <- predict(log_model, data1)

# Plot actual vs predicted for each model
ggplot() +
  geom_point(data = data1, aes(x = coarse_aggregate, y = concrete_compressive_strength), color = "blue", size = 1) +
  geom_line(data = data1, aes(x = coarse_aggregate, y = pred_poly), color = "red", linetype = "dashed", size = 1) +
  geom_line(data = data1, aes(x = coarse_aggregate, y = pred_exp), color = "green", linetype = "dashed", size = 1) +
  geom_line(data = data1, aes(x = coarse_aggregate, y = pred_log), color = "purple", linetype = "dashed", size = 1) +
  labs(title = "Coarse Aggregate vs Concrete Compressive Strength",
       x = "Coarse Aggregate",
       y = "Concrete Compressive Strength") +
  theme_minimal()

# Calculate RSS and R-squared for each model
rss_poly <- sum(residuals(poly_model)^2)
rss_exp <- sum(residuals(exp_model)^2)
rss_log <- sum(residuals(log_model)^2)

tss <- sum((data1$concrete_compressive_strength - mean(data1$concrete_compressive_strength))^2)

r_squared_poly <- 1 - (rss_poly / tss)
r_squared_exp <- 1 - (rss_exp / tss)
r_squared_log <- 1 - (rss_log / tss)

# Print R-squared values
r_squared_poly
r_squared_exp
r_squared_log










rm(list = ls())
# Load necessary libraries
library(ggplot2)

data1 <- read.csv("C:/Users/Dilshan/Desktop/BUAN 6357/concrete_data.csv", header = TRUE)



# Polynomial regression (quadratic) using nls() for fine_aggregate
poly_model <- nls(concrete_compressive_strength ~ a * fine_aggregate^2 + b * fine_aggregate + c, 
                  data = data1, start = list(a = 1, b = 1, c = 1))

# Exponential regression using nls() for fine_aggregate
exp_model <- nls(concrete_compressive_strength ~ a * exp(b * fine_aggregate), 
                 data = data1, start = list(a = 1, b = 0.01))

# Logarithmic regression using nls() for fine_aggregate
log_model <- nls(concrete_compressive_strength ~ a * log(fine_aggregate) + b, 
                 data = data1, start = list(a = 1, b = 1))

# Summary of each model
summary(poly_model)
summary(exp_model)
summary(log_model)

# Predict values from each model
data1$pred_poly <- predict(poly_model, data1)
data1$pred_exp <- predict(exp_model, data1)
data1$pred_log <- predict(log_model, data1)

# Plot actual vs predicted for each model
ggplot() +
  geom_point(data = data1, aes(x = fine_aggregate, y = concrete_compressive_strength), color = "blue", size = 1) +
  geom_line(data = data1, aes(x = fine_aggregate, y = pred_poly), color = "red", linetype = "dashed", size = 1) +
  geom_line(data = data1, aes(x = fine_aggregate, y = pred_exp), color = "green", linetype = "dashed", size = 1) +
  geom_line(data = data1, aes(x = fine_aggregate, y = pred_log), color = "purple", linetype = "dashed", size = 1) +
  labs(title = "Fine Aggregate vs Concrete Compressive Strength",
       x = "Fine Aggregate",
       y = "Concrete Compressive Strength") +
  theme_minimal()

# Calculate RSS and R-squared for each model
rss_poly <- sum(residuals(poly_model)^2)
rss_exp <- sum(residuals(exp_model)^2)
rss_log <- sum(residuals(log_model)^2)

tss <- sum((data1$concrete_compressive_strength - mean(data1$concrete_compressive_strength))^2)

r_squared_poly <- 1 - (rss_poly / tss)
r_squared_exp <- 1 - (rss_exp / tss)
r_squared_log <- 1 - (rss_log / tss)

# Print R-squared values
r_squared_poly
r_squared_exp
r_squared_log













rm(list = ls())
# Load necessary libraries
library(ggplot2)

data1 <- read.csv("C:/Users/Dilshan/Desktop/BUAN 6357/concrete_data.csv", header = TRUE)




# Polynomial regression (quadratic) using nls() for age
poly_model <- nls(concrete_compressive_strength ~ a * age^2 + b * age + c, 
                  data = data1, start = list(a = 1, b = 1, c = 1))

# Exponential regression using nls() for age
exp_model <- nls(concrete_compressive_strength ~ a * exp(b * age), 
                 data = data1, start = list(a = 1, b = 0.01))

# Logarithmic regression using nls() for age
log_model <- nls(concrete_compressive_strength ~ a * log(age) + b, 
                 data = data1, start = list(a = 1, b = 1))

# Summary of each model
summary(poly_model)
summary(exp_model)
summary(log_model)

# Predict values from each model
data1$pred_poly <- predict(poly_model, data1)
data1$pred_exp <- predict(exp_model, data1)
data1$pred_log <- predict(log_model, data1)

# Plot actual vs predicted for each model
ggplot() +
  geom_point(data = data1, aes(x = age, y = concrete_compressive_strength), color = "blue", size = 1) +
  geom_line(data = data1, aes(x = age, y = pred_poly), color = "red", linetype = "dashed", size = 1) +
  geom_line(data = data1, aes(x = age, y = pred_exp), color = "green", linetype = "dashed", size = 1) +
  geom_line(data = data1, aes(x = age, y = pred_log), color = "purple", linetype = "dashed", size = 1) +
  labs(title = "Age vs Concrete Compressive Strength",
       x = "Age",
       y = "Concrete Compressive Strength") +
  theme_minimal()

# Calculate RSS and R-squared for each model
rss_poly <- sum(residuals(poly_model)^2)
rss_exp <- sum(residuals(exp_model)^2)
rss_log <- sum(residuals(log_model)^2)

tss <- sum((data1$concrete_compressive_strength - mean(data1$concrete_compressive_strength))^2)

r_squared_poly <- 1 - (rss_poly / tss)
r_squared_exp <- 1 - (rss_exp / tss)
r_squared_log <- 1 - (rss_log / tss)

# Print R-squared values
r_squared_poly
r_squared_exp
r_squared_log










rm(list = ls())
# Load necessary libraries
library(ggplot2)

data1 <- read.csv("C:/Users/Dilshan/Desktop/BUAN 6357/concrete_data.csv", header = TRUE)

# Polynomial regression (quadratic) using nls() for water
poly_model <- nls(concrete_compressive_strength ~ a * water^2 + b * water + c, 
                  data = data1, start = list(a = 1, b = 1, c = 1))

# Exponential regression using nls() for water
exp_model <- nls(concrete_compressive_strength ~ a * exp(b * water), 
                 data = data1, start = list(a = 1, b = 0.01))

# Logarithmic regression using nls() for water
log_model <- nls(concrete_compressive_strength ~ a * log(water) + b, 
                 data = data1, start = list(a = 1, b = 1))

# Summary of each model
summary(poly_model)
summary(exp_model)
summary(log_model)

# Predict values from each model
data1$pred_poly <- predict(poly_model, data1)
data1$pred_exp <- predict(exp_model, data1)
data1$pred_log <- predict(log_model, data1)

# Plot actual vs predicted for each model
ggplot() +
  geom_point(data = data1, aes(x = water, y = concrete_compressive_strength), color = "blue", size = 1) +
  geom_line(data = data1, aes(x = water, y = pred_poly), color = "red", linetype = "dashed", size = 1) +
  geom_line(data = data1, aes(x = water, y = pred_exp), color = "green", linetype = "dashed", size = 1) +
  geom_line(data = data1, aes(x = water, y = pred_log), color = "purple", linetype = "dashed", size = 1) +
  labs(title = "Water vs Concrete Compressive Strength",
       x = "Water",
       y = "Concrete Compressive Strength") +
  theme_minimal()

# Calculate RSS and R-squared for each model
rss_poly <- sum(residuals(poly_model)^2)
rss_exp <- sum(residuals(exp_model)^2)
rss_log <- sum(residuals(log_model)^2)

tss <- sum((data1$concrete_compressive_strength - mean(data1$concrete_compressive_strength))^2)

r_squared_poly <- 1 - (rss_poly / tss)
r_squared_exp <- 1 - (rss_exp / tss)
r_squared_log <- 1 - (rss_log / tss)

# Print R-squared values
r_squared_poly
r_squared_exp
r_squared_log


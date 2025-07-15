rm(list = ls())


#data(mtcars)

# Non-linear regression model
#nls_model <- nls(
#  mpg ~ a + b * exp(c * hp) + d * wt^2,
#  data = mtcars,
#  start = list(a = 10, b = 1, c = 0.01, d = 1)
#)

# Summary of the model
#summary(nls_model)

# Predictions
#predictions <- predict(nls_model, newdata = mtcars)
#dev.off()  # Closes current plot device (if one is open)

# Plot the results
#plot(mtcars$mpg, predictions, mar = c(0, 0, 0, 0), main = "Non-linear Regression Predictions vs Actual")
#abline(a = 0, b = 1, col = "red")  # Add a diagonal line for reference




data1 <- read.csv("C:/Users/Dilshan/Desktop/portfolio projects/MLR and MNLR/concrete_data.csv", header = TRUE)
nls_data_age <- nls(
  concrete_compressive_strength ~ a + b * cement + d * age^2,
  data = data1,
  start = list(a = 50, b = 0.01, d = 0.0001),
  control = list(maxiter = 1000)
)

summary(nls_data_age)

predictions <- predict(nls_data_age, newdata = data1)
#dev.off()
plot(data1$concrete_compressive_strength, predictions, mar = c(0,0,0,0), main = "Non-linear Regression Prediction vs Actual(Age)")
abline(a = 0, b = 1, col = "red")










nls_data_water <- nls(
  concrete_compressive_strength ~ a + b * cement + d * water^2,
  data = data1,
  start = list(a = 50, b = 0.01, d = 0.01),
  control = list(maxiter = 1000)
)

summary(nls_data_water)

predictions_water <- predict(nls_data_water, newdata = data1)
#dev.off()
plot(data1$concrete_compressive_strength, predictions_water, mar = c(0,0,0,0), main = "Non-linear Regression Prediction vs Actual (Water)")
abline(a = 0, b = 1, col = "red")







nls_data_superplasticizer <- nls(
  concrete_compressive_strength ~ a + b * cement + d * superplasticizer^2,
  data = data1,
  start = list(a = 50, b = 0.01, d = 0.01),
  control = list(maxiter = 1000)
)

summary(nls_data_superplasticizer)

predictions_superplasticizer <- predict(nls_data_superplasticizer, newdata = data1)
#dev.off()
plot(data1$concrete_compressive_strength, predictions_superplasticizer, mar = c(0,0,0,0), main = "Non-linear Regression Prediction vs Actual (Superplasticizer)")
abline(a = 0, b = 1, col = "red")






nls_data_coarse_aggregate <- nls(
  concrete_compressive_strength ~ a + b * cement + d * coarse_aggregate^2,
  data = data1,
  start = list(a = 50, b = 0.01, d = 0.01),
  control = list(maxiter = 1000)
)

summary(nls_data_coarse_aggregate)

predictions_coarse_aggregate <- predict(nls_data_coarse_aggregate, newdata = data1)
#dev.off()
plot(data1$concrete_compressive_strength, predictions_coarse_aggregate, mar = c(0,0,0,0), main = "Non-linear Regression Prediction vs Actual (Coarse Aggregate)")
abline(a = 0, b = 1, col = "red")








nls_data_fine_aggregate <- nls(
  concrete_compressive_strength ~ a + b * cement + d * fine_aggregate^2,
  data = data1,
  start = list(a = 50, b = 0.01, d = 0.01),
  control = list(maxiter = 1000)
)

summary(nls_data_fine_aggregate)

predictions_fine_aggregate <- predict(nls_data_fine_aggregate, newdata = data1)
#dev.off()
plot(data1$concrete_compressive_strength, predictions_fine_aggregate, mar = c(0,0,0,0), main = "Non-linear Regression Prediction vs Actual (Fine Aggregate)")
abline(a = 0, b = 1, col = "red")









nls_data_blast_furnace_slag <- nls(
  concrete_compressive_strength ~ a + b * cement + d * blast_furnace_slag^2,
  data = data1,
  start = list(a = 50, b = 0.01, d = 0.01),
  control = list(maxiter = 1000)
)

summary(nls_data_blast_furnace_slag)

predictions_blast_furnace_slag <- predict(nls_data_blast_furnace_slag, newdata = data1)
#dev.off()
plot(data1$concrete_compressive_strength, predictions_blast_furnace_slag, mar = c(0,0,0,0), main = "Non-linear Regression Prediction vs Actual (Blast Furnace Slag)")
abline(a = 0, b = 1, col = "red")










nls_data_fly_ash <- nls(
  concrete_compressive_strength ~ a + b * cement + d * fly_ash^2,
  data = data1,
  start = list(a = 50, b = 0.01, d = 0.01),
  control = list(maxiter = 1000)
)

summary(nls_data_fly_ash)

predictions_fly_ash <- predict(nls_data_fly_ash, newdata = data1)
#dev.off()
plot(data1$concrete_compressive_strength, predictions_fly_ash, mar = c(0,0,0,0), main = "Non-linear Regression Prediction vs Actual (Fly Ash)")
abline(a = 0, b = 1, col = "red")


nls_data_cement <- nls(
  concrete_compressive_strength ~ a + b * water + d * cement^2,
  data = data1,
  start = list(a = 50, b = 0.01, d = 0.01),
  control = list(maxiter = 1000)
)

summary(nls_data_cement)

predictions_cement <- predict(nls_data_cement, newdata = data1)
#dev.off()
plot(data1$concrete_compressive_strength, predictions_cement, mar = c(0,0,0,0), main = "Non-linear Regression Prediction vs Actual (Cement)")
abline(a = 0, b = 1, col = "red")



nls_combined_model <- nls(
  concrete_compressive_strength ~ a + b * cement + c * age^2 + d * water^2 + e * superplasticizer^2,
  data = data1,
  start = list(a = 50, b = 0.01, c = 0.01, d = 0.01, e = 0.01),
  control = list(maxiter = 1000)
)

summary(nls_combined_model)



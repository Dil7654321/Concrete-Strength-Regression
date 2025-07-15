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
mlr_model <- lm(concrete_compressive_strength ~ cement + blast_furnace_slag + fly_ash + superplasticizer + coarse_aggregate + fine_aggregate + age, data = mlr_data)

# Display the summary of the regression model
summary(mlr_model)
summary(mlr_data)


# Create a new data frame with values for prediction
#new_data <- data.frame(
#  SquareFootage = c(2500, 1800),
#  NumBedrooms = c(3, 4),
#  DistanceToCityCenter = c(10, 20),
#  AgeOfHouse = c(20, 50),
#  IncomeOfOwner = c(80000, 60000)
#)

#new_data <- data.frame(
#  cement = c(350, 400),               # Reasonable cement values based on the dataset and its impact
#  blast_furnace_slag = c(120, 150),    # Values for blast furnace slag
#  fly_ash = c(60, 70),                 # Values for fly ash
#  superplasticizer = c(8, 10),         # Superplasticizer has a strong impact
#  coarse_aggregate = c(1000, 1050),    # Coarse aggregate in a typical range
#  fine_aggregate = c(850, 880)         # Fine aggregate in a typical range
#)

new_data <- data.frame(
  cement = c(300, 400),               
  blast_furnace_slag = c(100, 150),    
  fly_ash = c(50, 100),                
  superplasticizer = c(8, 12),         
  coarse_aggregate = c(950, 1000),     
  fine_aggregate = c(800, 850),
  age = c(1,365)
)



# Predict house prices using the MLR model
predicted_strength <- predict(mlr_model, newdata = new_data)

# Display the predicted prices
predicted_strength

# Residual plots for assumption checking
#dev.off()  # Closes current plot device (if one is open)
par(mfrow = c(2,2))  # Plot in 2x2 grid

plot(mlr_model)






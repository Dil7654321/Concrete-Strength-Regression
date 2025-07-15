# Assuming you've already fitted the MLR model and loaded the dataset
# Extract the coefficients from the MLR model
coef_values <- coef(mlr_model)

# Assign the coefficients to variables
intercept <- coef_values['(Intercept)']
cement_coef <- coef_values['cement']
blast_furnace_slag_coef <- coef_values['blast_furnace_slag']
fly_ash_coef <- coef_values['fly_ash']
water_coef <- coef_values['water']
superplasticizer_coef <- coef_values['superplasticizer']
age_coef <- coef_values['age']

# Define the objective function using the coefficients from the MLR model
objective_function <- function(params) {
  cement <- params[1]
  blast_furnace_slag <- params[2]
  fly_ash <- params[3]
  water <- params[4]
  superplasticizer <- params[5]
  age <- params[6]
  
  # Ensure the sum of cement, blast furnace slag, fly ash, water, and superplasticizer does not exceed 2300
  if (cement + blast_furnace_slag + fly_ash + water + superplasticizer > 2300) {
    return(Inf)  # Penalize by returning a large value if constraint is violated
  }
  
  # Use the MLR regression equation with the extracted coefficients
  strength <- intercept + cement_coef * cement + blast_furnace_slag_coef * blast_furnace_slag +
    fly_ash_coef * fly_ash + water_coef * water +
    superplasticizer_coef * superplasticizer + age_coef * age
  
  # We minimize the negative strength to maximize it
  return(-strength)
}

# Initial values for parameters
initial_params <- c(300, 50, 100, 150, 5, 28)

# Run the optimization with realistic bounds for each parameter and the constraint for the sum <= 2300
result <- optim(initial_params, objective_function, method = "L-BFGS-B",
                lower = c(100, 0, 0, 100, 0, 1),   # Lower bounds for each variable
                upper = c(600, 350, 250, 250, 30, 28))  # Upper bounds for each variable, age max is 28

# Extract the optimal values
optimal_params <- result$par
cement_opt <- optimal_params[1]
blast_furnace_slag_opt <- optimal_params[2]
fly_ash_opt <- optimal_params[3]
water_opt <- optimal_params[4]
superplasticizer_opt <- optimal_params[5]
age_opt <- optimal_params[6]

# Calculate the predicted strength for the optimal parameters
predicted_strength <- -(result$value)  # Since we minimized the negative strength

# Print the optimal values and predicted strength
cat("Optimal Cement: ", cement_opt, "\n")
cat("Optimal Blast Furnace Slag: ", blast_furnace_slag_opt, "\n")
cat("Optimal Fly Ash: ", fly_ash_opt, "\n")
cat("Optimal Water: ", water_opt, "\n")
cat("Optimal Superplasticizer: ", superplasticizer_opt, "\n")
cat("Optimal Age: ", age_opt, "\n")
cat("Predicted Maximum Concrete Compressive Strength: ", predicted_strength, "\n")


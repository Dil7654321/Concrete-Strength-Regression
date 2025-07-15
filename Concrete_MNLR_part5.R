# Assuming you already have your nls model fitted
# Extract the coefficients from the model
coef_values <- coef(reg_model)

# Assign the coefficients to variables
a <- coef_values['a']
b <- coef_values['b']
c <- coef_values['c']
d <- coef_values['d']
e <- coef_values['e']
f <- coef_values['f']
g <- coef_values['g']
h <- coef_values['h']
i <- coef_values['i']
j <- coef_values['j']
k <- coef_values['k']
l <- coef_values['l']

# Define the objective function using the coefficients from the regression model
objective_function <- function(params) {
  cement <- params[1]
  blast_furnace_slag <- params[2]
  water <- params[3]
  superplasticizer <- params[4]
  age <- params[5]
  fine_aggregate <- params[6]
  coarse_aggregate <- params[7]
  
  # Sum of the constrained variables
  sum_of_vars <- cement + blast_furnace_slag + water + superplasticizer + fine_aggregate + coarse_aggregate
  
  # Add a penalty if the sum exceeds 2300
  penalty <- 0
  if (sum_of_vars > 2300) {
    penalty <- 10000 * (sum_of_vars - 2300)^2  # Penalty term
  }
  
  # Use the regression equation with the extracted coefficients
  strength <- a * cement + b * blast_furnace_slag^2 + c * blast_furnace_slag +
    d * water^2 + e * water +
    f * superplasticizer + g * log(age) +
    h * fine_aggregate^2 + i * fine_aggregate +
    j * coarse_aggregate^2 + k * coarse_aggregate + l
  
  # We minimize the negative strength plus penalty
  return(-(strength) + penalty)
}

# Initial values for parameters
initial_params <- c(300, 50, 150, 5, 30, 700, 1000)

# Run the optimization
result <- optim(initial_params, objective_function, method = "L-BFGS-B",
                lower = c(100, 0, 100, 0, 1, 500, 800),  # Lower bounds
                upper = c(600, 350, 250, 30, 28, 1000, 1200))  # Upper bounds

# Extract the optimal values
optimal_params <- result$par
cement_opt <- optimal_params[1]
blast_furnace_slag_opt <- optimal_params[2]
water_opt <- optimal_params[3]
superplasticizer_opt <- optimal_params[4]
age_opt <- optimal_params[5]
fine_aggregate_opt <- optimal_params[6]
coarse_aggregate_opt <- optimal_params[7]

# Calculate the predicted strength for the optimal parameters
predicted_strength <- -(result$value)  # Since we minimized the negative strength

# Print the optimal values and predicted strength
cat("Optimal Cement: ", cement_opt, "\n")
cat("Optimal Blast Furnace Slag: ", blast_furnace_slag_opt, "\n")
cat("Optimal Water: ", water_opt, "\n")
cat("Optimal Superplasticizer: ", superplasticizer_opt, "\n")
cat("Optimal Age: ", age_opt, "\n")
cat("Optimal Fine Aggregate: ", fine_aggregate_opt, "\n")
cat("Optimal Coarse Aggregate: ", coarse_aggregate_opt, "\n")
cat("Predicted Concrete Compressive Strength: ", predicted_strength, "\n")


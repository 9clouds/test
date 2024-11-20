
# 1 Load necessary libraries

library(nycflights13)
library(doParallel)
library(foreach)
library(boot) 

# 2 Write functions for calculation

# 2.1 Analytical Standard Error Calculation
analytical_se <- function(data) {
  model <- lm(arr_delay ~ log(distance), data = data)
  return(summary(model)$coefficients["log(distance)", "Std. Error"])
}

# 2.2 Non-Parallel Bootstrap Standard Error Calculation
bootstrap_se_nonparallel <- function(N, R, data) {
  # Bootstrap function to refit the model and extract coefficient
  boot_fn <- function(data, indices) {
    sample_data <- data[indices, ]
    coef(lm(arr_delay ~ log(distance), data = sample_data))["log(distance)"]
  }
  
  # Perform bootstrapping
  boot_results <- boot(data, boot_fn, R = R)
  return(sd(boot_results$t))
}

# 2.3 Parallel Bootstrap Standard Error Calculation
bootstrap_se_parallel <- function(N, R, data) {
  # Set up parallel backend
  ncore <- detectCores()  # Detect the number of available cores
  cl <- makeCluster(ncore - 1, type = "PSOCK")  # Create a cluster with ncore - 1 cores
  registerDoParallel(cl)  # Register the cluster with doParallel
  
  # Create bootstrapping resamples in advance
  resamples <- lapply(1:R, function(i) sample(1:N, N, replace = TRUE))
  
  # Run the bootstrap in parallel using foreach and %dopar%
  boot_results <- foreach(indices = resamples, .combine = 'c', .packages = 'nycflights13') %dopar% {
    sample_data <- data[indices, ]
    coef(lm(arr_delay ~ log(distance), data = sample_data))["log(distance)"]
  }
  
  # Stop the cluster
  stopCluster(cl)
  
  return(sd(boot_results))
}

# 3 Set parameters for bootstrap (e.g., N = number of observations, R = number of resamples)

N <- nrow(flights)
R <- 500  # Adjust R to a reasonable number for testing

# 4 Run functions

# 4.1 Calculate the analytical SE and measure time
start_time_analytical <- Sys.time()
analytical_se_val <- analytical_se(flights)
end_time_analytical <- Sys.time()
time_analytical <- end_time_analytical - start_time_analytical

# 4.2 Calculate bootstrap SE without parallelization and measure time
start_time_nonparallel <- Sys.time()
nonparallel_se_val <- bootstrap_se_nonparallel(N, R, flights)
end_time_nonparallel <- Sys.time()
time_nonparallel <- end_time_nonparallel - start_time_nonparallel

# 4.3 Calculate bootstrap SE with parallelization and measure time
start_time_parallel <- Sys.time()
parallel_se_val <- bootstrap_se_parallel(N, R, flights)
end_time_parallel <- Sys.time()
time_parallel <- end_time_parallel - start_time_parallel

# 5 Verification and Output Results

# Check that bootstrapped SE values are close to the analytical SE value
tolerance <- 0.01  # Define a tolerance level (1% difference)
if (abs(nonparallel_se_val - analytical_se_val) > tolerance) {
  stop("Non-parallel bootstrap SE is not within tolerance of analytical SE")
}
if (abs(parallel_se_val - analytical_se_val) > tolerance) {
  stop("Parallel bootstrap SE is not within tolerance of analytical SE")
}

# 6 Output results to a text file for reproducibility

output <- data.frame(
  Method = c("Analytical", "Bootstrap Non-Parallel", "Bootstrap Parallel"),
  SE = c(analytical_se_val, nonparallel_se_val, parallel_se_val),
  Time = c(time_analytical, time_nonparallel, time_parallel)
)

# Write output to file
write.table(output, file = "output.txt", row.names = FALSE, quote = FALSE)

# Print the output to the console as well
print(output)

set.seed(123)

SAMPLE_COUNT <- 100

# Generate samples
generate_uniform_sample <- function(n, min, max) {
  runif(n, min = min, max = max)
}

run_experiments <- function(sample_count, sample_size, estimation_function) {
  results <- numeric(sample_count)
  for (i in 1:sample_count) {
    results[i] <- estimation_function(
      generate_uniform_sample(sample_size, 0, 20)
    )
  }
  return(results)
}

run_estimators <- function(estimator_list) {
  est_result_list <- list()
  for (estimator_nr in names(estimator_list)) {
    estimator <- estimator_list[[estimator_nr]]
    est_results <- run_experiments(
      sample_count = SAMPLE_COUNT,
      sample_size = 10,
      estimation_function = estimator
    )

    est_result_list[[estimator_nr]] <- est_results
  }
  return(est_result_list)
}

# Schätzer Erwartungswert

educated_guess_est <- function(sample) {
  10.5
}

arithmetic_mean_est <- function(sample) {
  sum(sample) / length(sample)
}

distorted_arithmetic_mean_est <- function(sample) {
  sum(sample) / (length(sample) - 1)
}

geometric_mean_est <- function(sample) {
  prod(sample) ^ (1 / length(sample))
}

harmonic_mean_est <- function(sample) {
  length(sample) / sum(1 / sample)
}

median_est <- function(sample) {
  median(sample)
}

# Schätzermetriken

mean_and_distance_metric <- function(est_sample, expected_value) {
  est_mean <- mean(est_sample)
  abs_dist <- abs(expected_value - est_mean)
  return(list(est_mean = est_mean, abs_dist = abs_dist))
}

empiric_variance_metric <- function(est_sample) {
  var(est_sample)
  #mean((est_sample - mean(est_sample))^2)
}

closest_to_parameter_metric <- function(est_result_list, expected_value) {
  count_closests <- setNames(rep(0, length(est_result_list)), names(est_result_list))

  for (i in 1:SAMPLE_COUNT) { # TODO: remove magic number
    values_at_i <- sapply(est_result_list, function(vec) vec[i])
    closest_est <- names(which.min(abs(values_at_i - expected_value)))
    count_closests[closest_est] <- count_closests[closest_est] + 1
  }
  return(count_closests)
}

in_interval_metric <- function(est_sample, expected_value) {
  sum(est_sample >= (expected_value - 1) & est_sample <= (expected_value + 1))
}

mse_metric <- function(est_sample, expected_value) {
  mean((est_sample - expected_value)^2)
}

# Calculate Stuff

expected_value <- 10

estimator_list <- list(
  T1 = educated_guess_est,
  T2 = arithmetic_mean_est,
  T3 = distorted_arithmetic_mean_est,
  T4 = geometric_mean_est,
  T5 = harmonic_mean_est,
  T6 = median_est
)
est_result_list <- run_estimators(estimator_list)

# Metric a)
for (estimator_nr in names(est_result_list)) {
  est_results <- est_result_list[[estimator_nr]]
  metric_results <- mean_and_distance_metric(est_results, expected_value)

  print(sprintf(
    "%s - Mittelwert: %f | Abstand zu μ (%g): %f",
    estimator_nr,
    metric_results$est_mean,
    expected_value,
    metric_results$abs_dist
  ))
}

# Metric b)
for (estimator_nr in names(est_result_list)) {
  est_results <- est_result_list[[estimator_nr]]
  metric_result <- empiric_variance_metric(est_results)

  print(sprintf(
    "%s - Varianz: %f",
    estimator_nr,
    metric_result
  ))
}

# Metric c)
closest_list <- closest_to_parameter_metric(est_result_list, expected_value)
for (closest_nr in names(closest_list)) {
  closest_res <- closest_list[[closest_nr]]

  print(sprintf(
    "%s - Am nähsten an μ: %g mal",
    closest_nr,
    closest_res
  ))
}

# Metric d)
for (estimator_nr in names(est_result_list)) {
  est_results <- est_result_list[[estimator_nr]]
  metric_result <- in_interval_metric(est_results, expected_value)

  print(sprintf(
    "%s - Im Intervall [μ-1;μ+1]: %g mal",
    estimator_nr,
    metric_result
  ))
}

# Metric e)
for (estimator_nr in names(est_result_list)) {
  est_results <- est_result_list[[estimator_nr]]
  metric_result <- mse_metric(est_results, expected_value)

  print(sprintf(
    "%s - emp. MSE: %f",
    estimator_nr,
    metric_result
  ))
}

#' Calculate success rate of simulation
#' 
#' @param results A SimResults object
#' @param threshold Minimum portfolio value to be considered successful
#' @return Numeric success rate between 0 and 1
#' @export
sim_success_rate <- function(results, threshold = 0) {
  final_values <- results@trajectories[, ncol(results@trajectories)]
  mean(final_values > threshold)
}

#' Get percentiles of simulation trajectories
#' 
#' @param results A SimResults object
#' @param probs Probability values for percentiles
#' @return Matrix of percentiles (rows = percentiles, columns = periods)
#' @export
sim_percentiles <- function(results, probs = c(0.1, 0.25, 0.5, 0.75, 0.9)) {
  apply(results@trajectories, 2, quantile, probs = probs)
}

#' Create summary statistics of simulation
#' 
#' @param results A SimResults object
#' @return List containing summary statistics
#' @export
sim_summary <- function(results) {
  list(
    success_rate = sim_success_rate(results),
    final_percentiles = quantile(
      results@trajectories[, ncol(results@trajectories)],
      c(0.05, 0.25, 0.5, 0.75, 0.95)
    ),
    min_value = min(results@trajectories),
    max_value = max(results@trajectories)
  )
}
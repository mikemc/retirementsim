#' Generic for generating returns
#'
#' Currently defined for GBMModel and HistoricalModel.
#'
#' @param model A MarketModel object
#' @param n_sims Number of simulations
#' @param n_periods Number of periods
#' @param periods_per_year Number of periods per year
#' @return Matrix of returns (n_sims x n_periods)
#' @include classes.R
#' @export
generate_returns <- new_generic("generate_returns", "model")

#' @export
method(generate_returns, GBMModel) <- function(
  model,
  n_sims,
  n_periods,
  periods_per_year
) {
  # Convert parameters to period frequency if needed
  if (model@return_period == "annual" && periods_per_year != 1) {
    period_mean <- (1 + model@mean_return)^(1/periods_per_year) - 1
    period_sd <- model@sd_return / sqrt(periods_per_year)
  } else {
    period_mean <- model@mean_return
    period_sd <- model@sd_return
  }

  # Generate returns using GBM
  drift_adjusted <- period_mean - (period_sd^2 / 2)

  log_returns <- matrix(
    rnorm(n_sims * n_periods, mean = drift_adjusted, sd = period_sd),
    nrow = n_sims,
    ncol = n_periods
  )

  exp(log_returns)
}

#' @export
method(generate_returns, HistoricalModel) <- function(
  model,
  n_sims,
  n_periods,
  periods_per_year
) {
  # Bootstrap from historical returns in blocks
  n_blocks <- ceiling(n_periods / model@block_size)
  n_historical_blocks <- length(model@historical_returns) - model@block_size + 1

  returns_matrix <- matrix(nrow = n_sims, ncol = n_periods)

  for (i in 1:n_sims) {
    sampled_blocks <- sample(1:n_historical_blocks, n_blocks, replace = TRUE)
    sampled_returns <- numeric()

    for (block_start in sampled_blocks) {
      block_end <- min(block_start + model@block_size - 1, length(model@historical_returns))
      sampled_returns <- c(sampled_returns, model@historical_returns[block_start:block_end])
    }

    returns_matrix[i, ] <- sampled_returns[1:n_periods]
  }

  returns_matrix
}

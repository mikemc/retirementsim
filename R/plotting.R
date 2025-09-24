#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon scale_y_continuous labs theme_minimal
#' @importFrom scales dollar
#' @include classes.R
#' @export
#' @noRd
method(plot, SimResults) <- function(x, n_paths = 100, ...) {
  results <- x  # Rename for clarity

  # Sample paths for visualization
  n_sims <- nrow(results@trajectories)
  sample_idx <- sample(1:n_sims, min(n_paths, n_sims))

  # Convert to long format
  periods <- ncol(results@trajectories)
  ages <- results@start_age + (0:(periods-1)) / results@periods_per_year

  plot_data <- data.frame(
    age = rep(ages, times = length(sample_idx)),
    value = as.vector(t(results@trajectories[sample_idx, ])),
    sim = rep(1:length(sample_idx), each = periods)
  )

  # Calculate percentiles
  percentiles <- apply(results@trajectories, 2, quantile, c(0.1, 0.5, 0.9))
  percentile_data <- data.frame(
    age = ages,
    p10 = percentiles[1, ],
    p50 = percentiles[2, ],
    p90 = percentiles[3, ]
  )

  ggplot() +
    geom_line(
      data = plot_data,
      aes(x = age, y = value, group = sim),
      alpha = 0.1, color = "gray40"
    ) +
    geom_ribbon(
      data = percentile_data,
      aes(x = age, ymin = p10, ymax = p90),
      fill = "blue", alpha = 0.2
    ) +
    geom_line(
      data = percentile_data,
      aes(x = age, y = p50),
      color = "darkblue", linewidth = 1.5
    ) +
    scale_y_continuous(labels = scales::dollar) +
    labs(x = "Age", y = "Portfolio value") +
    theme_minimal()
}

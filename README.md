# retirementsim

Monte Carlo Retirement Simulation Tools

## Description

`retirementsim` is a composable, extensible R package for Monte Carlo retirement simulations using S7 classes and matrix-based computations. It provides tools for modeling retirement portfolios with various market models, cash flows, and investment strategies.

## Installation

You can install the development version from your local directory:

```r
# install.packages("devtools")
devtools::install("path/to/retirementsim")
```

## Example

Here's a complete working example showing how to create and run a retirement simulation with monthly periods, with

- An initial balance of $50,000
- A market model (stocks with 7% mean return, 18% volatility)
- A accumulation phase (age 30-65) with monthly contributions that start at $1000 and grow 3% annually
- A distribution phase (age 65-95) with monthly $5000 withdrawals (adjusted for 2.5% inflation)

```r
library(retirementsim)

sim <- sim_define(
  n_simulations = 1000,
  periods_per_year = 12,
  start_age = 30,
  end_age = 95,
  initial_balance = 50000,
  seed = 42
) |>
  add_market_model(
    GBMModel(mean_return = 0.07, sd_return = 0.18, return_period = "annual"),
    asset = "stocks"
  ) |>
  add_phase(
    accumulation_phase(
      from_age = 30,
      to_age = 65,
      contribution = ContributionFlow(
        amount = 1000,
        growth_rate = 0.03
      )
    )
  ) |>
  add_phase(
    distribution_phase(
      from_age = 65,
      to_age = 95,
      withdrawal = WithdrawalFlow(
        amount = 5000,
        inflation_adjusted = TRUE,
        inflation_rate = 0.025
      )
    )
  )

# Run the simulation and calculate summary statistics
results <- sim_run(sim)

summary_stats <- sim_summary(results)
print(summary_stats)

success_rate <- sim_success_rate(results)
cat("Success Rate:", round(success_rate * 100, 1), "%\n")

percentiles <- sim_percentiles(results)

# Create visualization
library(ggplot2)
plot(results)
```

## Current features

### Core Components

- **S7 Classes**: Modern object-oriented system for clean, extensible code
- **Matrix-based computations**: Fast Monte Carlo simulations using vectorized operations
- **Period-agnostic design**: Works with any time period (annual, monthly, daily)
- **Composable phases**: Build complex retirement scenarios from simple components

### Market Models

- **GBM (Geometric Brownian Motion)**: Standard model for stock returns
- **Historical Bootstrap**: Sample from historical return data

### Cash Flows

- **Contributions**: Regular investments with optional growth rates
- **Withdrawals**: Fixed or inflation-adjusted distributions

### Analysis Tools

- **Success rate calculation**: Probability of meeting retirement goals
- **Percentile trajectories**: Understand the range of outcomes
- **Summary statistics**: Key metrics for decision-making
- **Visualization**: ggplot2-based charts showing simulation results

## License

MIT License - see LICENSE file for details

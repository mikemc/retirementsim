# RetirementSim R Package Design Document

## Overview

RetirementSim is a composable, extensible R package for Monte Carlo retirement simulations. It follows a "grammar of financial simulation" philosophy, allowing users to build complex retirement models from simple, orthogonal components.

## Core Design Principles

1. **Period-agnostic**: All calculations work with abstract "periods" (yearly, monthly, daily)
2. **Matrix-first**: Core computations use vectorized matrix operations for speed
3. **Composable**: Small, focused components that combine into complex simulations
4. **Extensible**: Users can easily add custom models, strategies, and cash flows
5. **Simple**: Minimal validation and complexity while in development

## S7 Class Architecture

```r
library(S7)

# Base class for all market models
MarketModel <- new_class("MarketModel",
  properties = list(
    periods_per_year = new_property(class_numeric, default = 12)
  )
)

# GBM model implementation
GBMModel <- new_class("GBMModel",
  parent = MarketModel,
  properties = list(
    mean_return = new_property(class_numeric),
    sd_return = new_property(class_numeric),
    return_period = new_property(class_character, default = "annual")
  )
)

# Cash flow base class
CashFlow <- new_class("CashFlow",
  properties = list(
    timing = new_property(class_character, default = "start")
  )
)

# Contribution flow
ContributionFlow <- new_class("ContributionFlow",
  parent = CashFlow,
  properties = list(
    amount = new_property(class_numeric),
    growth_rate = new_property(class_numeric, default = 0),
    periods = new_property(class_numeric, default = NULL)  # NULL = all periods
  )
)

# Portfolio strategy base
Strategy <- new_class("Strategy",
  properties = list(
    rebalance_frequency = new_property(class_numeric, default = NULL)
  )
)

# Fixed allocation strategy
FixedAllocation <- new_class("FixedAllocation",
  parent = Strategy,
  properties = list(
    allocations = new_property(class_list)  # list(stocks = 0.7, bonds = 0.3)
  )
)

# Phase (accumulation, distribution, etc)
Phase <- new_class("Phase",
  properties = list(
    start_age = new_property(class_numeric),
    end_age = new_property(class_numeric),
    cash_flows = new_property(class_list, default = list()),
    strategy = new_property(class_any, default = NULL),
    market_models = new_property(class_list, default = list())
  )
)

# Main simulation object
Simulation <- new_class("Simulation",
  properties = list(
    n_simulations = new_property(class_numeric, default = 1000),
    periods_per_year = new_property(class_numeric, default = 12),
    start_age = new_property(class_numeric),
    end_age = new_property(class_numeric),
    seed = new_property(class_numeric, default = NULL),
    phases = new_property(class_list, default = list()),
    global_market_models = new_property(class_list, default = list())
  )
)

# Simulation results container
SimResults <- new_class("SimResults",
  properties = list(
    trajectories = new_property(class_matrix),
    periods_per_year = new_property(class_numeric),
    start_age = new_property(class_numeric),
    metadata = new_property(class_list)
  )
)
```

## Core Functions

### Simulation Creation and Composition

```r
# Create a new simulation
sim_define <- function(n_simulations = 1000,
                      periods_per_year = 12,
                      start_age = 30,
                      end_age = 95,
                      seed = NULL) {
  Simulation(
    n_simulations = n_simulations,
    periods_per_year = periods_per_year,
    start_age = start_age,
    end_age = end_age,
    seed = seed
  )
}

# Add a phase to the simulation
add_phase <- function(sim, phase) {
  sim@phases <- append(sim@phases, list(phase))
  sim
}

# Create accumulation phase
accumulation_phase <- function(from_age, to_age, contribution = NULL, strategy = NULL) {
  phase <- Phase(
    start_age = from_age,
    end_age = to_age,
    strategy = strategy
  )

  if (!is.null(contribution)) {
    phase@cash_flows <- list(contribution)
  }

  phase
}

# Create distribution phase
distribution_phase <- function(from_age, to_age, withdrawal = NULL, strategy = NULL) {
  phase <- Phase(
    start_age = from_age,
    end_age = to_age,
    strategy = strategy
  )

  if (!is.null(withdrawal)) {
    phase@cash_flows <- list(withdrawal)
  }

  phase
}

# Add global market model
add_market_model <- function(sim, model, asset = "stocks") {
  sim@global_market_models[[asset]] <- model
  sim
}
```

### Market Models

```r
# Create GBM model
gbm_model <- function(mean_return = 0.07,
                     sd_return = 0.18,
                     return_period = "annual") {
  GBMModel(
    mean_return = mean_return,
    sd_return = sd_return,
    return_period = return_period
  )
}

# Generic for generating returns
method(generate_returns, MarketModel) <- function(model, n_sims, n_periods, periods_per_year) {
  S7_dispatch()
}

# GBM implementation
method(generate_returns, GBMModel) <- function(model, n_sims, n_periods, periods_per_year) {
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

# Historical bootstrap model
HistoricalModel <- new_class("HistoricalModel",
  parent = MarketModel,
  properties = list(
    historical_returns = new_property(class_numeric),
    block_size = new_property(class_numeric, default = 12)
  )
)

historical_model <- function(returns, block_size = 12) {
  HistoricalModel(
    historical_returns = returns,
    block_size = block_size
  )
}

method(generate_returns, HistoricalModel) <- function(model, n_sims, n_periods, periods_per_year) {
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
```

### Cash Flows

```r
# Contribution flow
contribution_flow <- function(amount, growth_rate = 0.03, frequency = NULL) {
  ContributionFlow(
    amount = amount,
    growth_rate = growth_rate,
    periods = frequency
  )
}

# Withdrawal flow
WithdrawalFlow <- new_class("WithdrawalFlow",
  parent = CashFlow,
  properties = list(
    amount = new_property(class_numeric),
    inflation_adjusted = new_property(class_logical, default = TRUE),
    rule = new_property(class_character, default = "constant")
  )
)

withdrawal_flow <- function(amount, inflation_adjusted = TRUE, rule = "constant") {
  WithdrawalFlow(
    amount = amount,
    inflation_adjusted = inflation_adjusted,
    rule = rule
  )
}

# Generate cash flow amounts for all periods
method(generate_cash_flows, CashFlow) <- function(flow, n_periods, periods_per_year) {
  S7_dispatch()
}

method(generate_cash_flows, ContributionFlow) <- function(flow, n_periods, periods_per_year) {
  amounts <- numeric(n_periods)

  # Calculate which periods get contributions
  if (is.null(flow@periods)) {
    contribution_periods <- 1:n_periods
  } else {
    contribution_periods <- flow@periods
  }

  # Apply growth rate
  for (i in contribution_periods) {
    year_fraction <- (i - 1) / periods_per_year
    amounts[i] <- flow@amount * (1 + flow@growth_rate)^year_fraction
  }

  amounts
}
```

### Simulation Engine

```r
# Main simulation runner
sim_run <- function(sim) {
  set.seed(sim@seed)

  n_periods <- (sim@end_age - sim@start_age) * sim@periods_per_year

  # Initialize portfolio matrix
  portfolios <- matrix(0, nrow = sim@n_simulations, ncol = n_periods + 1)

  # Generate returns for all assets upfront
  all_returns <- list()
  for (asset_name in names(sim@global_market_models)) {
    model <- sim@global_market_models[[asset_name]]
    all_returns[[asset_name]] <- generate_returns(
      model,
      sim@n_simulations,
      n_periods,
      sim@periods_per_year
    )
  }

  # Process each phase
  for (phase in sim@phases) {
    phase_start_period <- age_to_period(phase@start_age, sim@start_age, sim@periods_per_year)
    phase_end_period <- age_to_period(phase@end_age, sim@start_age, sim@periods_per_year)
    phase_periods <- phase_end_period - phase_start_period + 1

    # Get phase-specific returns (subset of global returns)
    phase_returns <- lapply(all_returns, function(r) {
      r[, phase_start_period:phase_end_period, drop = FALSE]
    })

    # Process cash flows for this phase
    for (flow in phase@cash_flows) {
      flow_amounts <- generate_cash_flows(flow, phase_periods, sim@periods_per_year)

      # Apply cash flows to portfolio
      for (period in 1:phase_periods) {
        global_period <- phase_start_period + period - 1

        if (inherits(flow, "ContributionFlow")) {
          portfolios[, global_period] <- portfolios[, global_period] + flow_amounts[period]
        } else if (inherits(flow, "WithdrawalFlow")) {
          portfolios[, global_period] <- pmax(portfolios[, global_period] - flow_amounts[period], 0)
        }
      }
    }

    # Apply returns (simplified - single asset for now)
    if (length(phase_returns) > 0) {
      returns_to_apply <- phase_returns[[1]]  # Just use first asset for now

      for (period in 1:phase_periods) {
        global_period <- phase_start_period + period - 1
        portfolios[, global_period + 1] <- portfolios[, global_period] * returns_to_apply[, period]
      }
    }
  }

  SimResults(
    trajectories = portfolios,
    periods_per_year = sim@periods_per_year,
    start_age = sim@start_age,
    metadata = list(
      n_simulations = sim@n_simulations,
      phases = sim@phases
    )
  )
}

# Helper function for age to period conversion
age_to_period <- function(age, start_age, periods_per_year) {
  (age - start_age) * periods_per_year + 1
}
```

### Analysis Functions

```r
# Calculate success rate
sim_success_rate <- function(results, threshold = 0) {
  final_values <- results@trajectories[, ncol(results@trajectories)]
  mean(final_values > threshold)
}

# Get percentiles
sim_percentiles <- function(results, probs = c(0.1, 0.25, 0.5, 0.75, 0.9)) {
  apply(results@trajectories, 2, quantile, probs = probs)
}

# Create summary
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

# Plotting
plot.SimResults <- function(results, n_paths = 100, ...) {
  library(ggplot2)

  # Sample paths for visualization
  n_sims <- nrow(results@trajectories)
  sample_idx <- sample(1:n_sims, min(n_paths, n_sims))

  # Convert to long format
  periods <- ncol(results@trajectories)
  ages <- results@start_age + (0:(periods-1)) / results@periods_per_year

  plot_data <- data.frame(
    age = rep(ages, each = length(sample_idx)),
    value = as.vector(t(results@trajectories[sample_idx, ])),
    sim = rep(1:length(sample_idx), periods)
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
    geom_line(data = plot_data, aes(x = age, y = value, group = sim),
              alpha = 0.1, color = "gray40") +
    geom_ribbon(data = percentile_data, aes(x = age, ymin = p10, ymax = p90),
                fill = "blue", alpha = 0.2) +
    geom_line(data = percentile_data, aes(x = age, y = p50),
              color = "darkblue", size = 1.5) +
    scale_y_continuous(labels = scales::dollar) +
    labs(x = "Age", y = "Portfolio Value",
         title = "Retirement Simulation Results") +
    theme_minimal()
}
```

## Example Usage

```r
library(retirementsim)

# Basic retirement simulation
sim <- sim_define(
  n_simulations = 10000,
  periods_per_year = 12,  # Monthly
  start_age = 30,
  end_age = 95,
  seed = 42
) %>%
  add_market_model(
    gbm_model(mean_return = 0.07, sd_return = 0.18),
    asset = "stocks"
  ) %>%
  add_phase(
    accumulation_phase(
      from_age = 30,
      to_age = 65,
      contribution = contribution_flow(
        amount = 1000,  # Monthly contribution
        growth_rate = 0.03
      )
    )
  ) %>%
  add_phase(
    distribution_phase(
      from_age = 65,
      to_age = 95,
      withdrawal = withdrawal_flow(
        amount = 5000,  # Monthly withdrawal
        inflation_adjusted = TRUE
      )
    )
  )

# Run simulation
results <- sim_run(sim)

# Analyze
sim_summary(results)
plot(results)
```

## Package Structure

```
retirementsim/
├── DESCRIPTION
├── NAMESPACE
├── R/
│   ├── classes.R           # S7 class definitions
│   ├── simulation.R        # sim_define, sim_run
│   ├── market-models.R     # GBM, historical, etc.
│   ├── cash-flows.R        # Contributions, withdrawals
│   ├── strategies.R        # Allocation strategies
│   ├── analysis.R          # Success rate, percentiles
│   ├── plotting.R          # Visualization functions
│   └── utils.R             # Helper functions
├── man/                    # Documentation (roxygen2)
├── tests/
│   └── testthat/
│       ├── test-simulation.R
│       ├── test-models.R
│       └── test-cashflows.R
└── vignettes/
    └── getting-started.Rmd
```

## Implementation Roadmap

### Phase 1: Core Engine (v0.1.0)
- [ ] S7 class structure
- [ ] Basic simulation engine
- [ ] GBM market model
- [ ] Simple contributions/withdrawals
- [ ] Success rate calculation
- [ ] Basic plotting

### Phase 2: Enhanced Models (v0.2.0)
- [ ] Historical bootstrap model
- [ ] Inflation modeling
- [ ] Multiple asset classes
- [ ] Asset correlation matrix
- [ ] Rebalancing strategies

### Phase 3: Advanced Cash Flows (v0.3.0)
- [ ] Social Security modeling
- [ ] Variable withdrawal rules (4% rule, etc.)
- [ ] Tax-aware cash flows
- [ ] Employer matching
- [ ] Lifecycle contribution patterns

### Phase 4: Strategies (v0.4.0)
- [ ] Glide path allocation
- [ ] Dynamic rebalancing
- [ ] CPPI and other protective strategies
- [ ] Tax loss harvesting
- [ ] Asset location optimization

### Phase 5: Analysis Tools (v0.5.0)
- [ ] Sensitivity analysis framework
- [ ] Scenario comparison
- [ ] Risk metrics (VaR, CVaR)
- [ ] Optimization tools
- [ ] Interactive Shiny app

### Phase 6: Performance & Polish (v1.0.0)
- [ ] C++ acceleration for critical paths
- [ ] Parallel simulation support
- [ ] Comprehensive documentation
- [ ] CRAN submission preparation
- [ ] Real-world validation cases

## Key Design Decisions

1. **S7 over S3**: Provides better type safety and cleaner inheritance while maintaining R-style simplicity
2. **Period-based from start**: Avoids painful refactoring when adding monthly/daily support
3. **Matrix operations**: 100x+ faster than loops for Monte Carlo simulations
4. **Minimal validation initially**: Focus on API design and features first
5. **Composable phases**: Allows complex lifecycle modeling without code complexity

## Next Steps for Implementation

1. Create package skeleton with `usethis::create_package("retirementsim")`
2. Add S7 dependency
3. Implement core classes in `R/classes.R`
4. Build simulation engine in `R/simulation.R`
5. Add basic tests to verify matrix operations
6. Create minimal working example
7. Iterate on API based on usage

This design provides a solid foundation that can grow from simple yearly simulations to complex sub-annual models with multiple assets, sophisticated strategies, and tax optimization.

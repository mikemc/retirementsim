# Instructions for Claude Code: Implement RetirementSim R Package

## Context
Please implement an R package called `retirementsim` based on the design document in `2025-09-14_design-doc.md`. This package will provide Monte Carlo simulation tools for retirement planning using S7 classes and matrix-based computations.

## Task 1: Create Package Structure

1. Create a new R package called `retirementsim` with the following structure:
```
retirementsim/
├── DESCRIPTION
├── NAMESPACE
├── R/
│   ├── classes.R
│   ├── simulation.R
│   ├── market-models.R
│   ├── cash-flows.R
│   ├── analysis.R
│   ├── plotting.R
│   └── utils.R
├── tests/
│   └── testthat/
│       └── test-simulation.R
└── README.md
```

2. Set up the DESCRIPTION file with:
   - Package name: retirementsim
   - Title: Monte Carlo Retirement Simulation Tools
   - Version: 0.1.0
   - Dependencies: S7, ggplot2, scales
   - Suggests: testthat

## Task 2: Implement Core Classes (R/classes.R)

Implement all S7 class definitions from the design document's "S7 Class Architecture" section:
- MarketModel (base class)
- GBMModel
- CashFlow (base class)
- ContributionFlow
- WithdrawalFlow
- Strategy (base class)
- FixedAllocation
- Phase
- Simulation
- SimResults

## Task 3: Implement Simulation Engine (R/simulation.R)

Implement the core simulation functions:
- `sim_define()` - Create new simulation
- `add_phase()` - Add phase to simulation
- `accumulation_phase()` - Create accumulation phase
- `distribution_phase()` - Create distribution phase
- `add_market_model()` - Add market model to simulation
- `sim_run()` - Main simulation engine (the most complex function)
- `age_to_period()` - Helper for age/period conversion

## Task 4: Implement Market Models (R/market-models.R)

Implement:
- `gbm_model()` - Create GBM model
- `generate_returns()` - S7 generic
- `generate_returns.GBMModel()` - GBM implementation
- `historical_model()` - Create historical bootstrap model
- `generate_returns.HistoricalModel()` - Historical bootstrap implementation

## Task 5: Implement Cash Flows (R/cash-flows.R)

Implement:
- `contribution_flow()` - Create contribution flow
- `withdrawal_flow()` - Create withdrawal flow
- `generate_cash_flows()` - S7 generic
- `generate_cash_flows.ContributionFlow()` - Implementation for contributions
- `generate_cash_flows.WithdrawalFlow()` - Implementation for withdrawals

## Task 6: Implement Analysis Functions (R/analysis.R)

Implement:
- `sim_success_rate()` - Calculate success rate
- `sim_percentiles()` - Get percentile trajectories
- `sim_summary()` - Create summary statistics

## Task 7: Implement Plotting (R/plotting.R)

Implement:
- `plot.SimResults()` - S3 method for plotting simulation results
  - Should show sample paths, percentile ribbons, and median path
  - Use ggplot2 for visualization

## Task 8: Create Helper Functions (R/utils.R)

Implement utility functions:
- `years_to_periods()`
- `periods_to_years()`
- `annualize_return()`
- `periodize_return()`

## Task 9: Create Working Example (README.md)

Create a README.md with:
1. Package description
2. Installation instructions
3. Complete working example showing:
   - Creating a simulation with monthly periods
   - Adding accumulation phase (age 30-65) with monthly $1000 contributions
   - Adding distribution phase (age 65-95) with monthly $5000 withdrawals
   - Running simulation with 10,000 scenarios
   - Displaying summary and plot

## Task 10: Create Basic Test (tests/testthat/test-simulation.R)

Create basic tests to verify:
1. Simulation object creation works
2. GBM model generates returns of correct dimensions
3. Cash flows are generated correctly
4. A simple simulation runs without errors

## Implementation Notes

1. **Start simple**: Focus on getting a working version first. Don't add extensive validation or error checking yet.

2. **Matrix operations**: Ensure all core computations use matrix operations, not loops over individual simulations.

3. **Period-agnostic**: All functions should work with abstract periods, not assume annual.

4. **S7 syntax**: Use the new S7 syntax from the design doc:
   - `new_class()` for class creation
   - `method()` for generic methods
   - `@` for property access

5. **Focus on Phase 1 features only**: Just implement what's needed for basic simulation with single asset class, simple contributions/withdrawals.

## Testing the Implementation

After implementing, test with this code:
```r
library(retirementsim)

sim <- sim_define(
  n_simulations = 1000,
  periods_per_year = 12,
  start_age = 30,
  end_age = 95
) %>%
  add_market_model(gbm_model(0.07, 0.18), "stocks") %>%
  add_phase(
    accumulation_phase(
      from_age = 30,
      to_age = 65,
      contribution = contribution_flow(1000, 0.03)
    )
  ) %>%
  add_phase(
    distribution_phase(
      from_age = 65,
      to_age = 95,
      withdrawal = withdrawal_flow(5000)
    )
  )

results <- sim_run(sim)
sim_summary(results)
plot(results)
```

This should run without errors and produce a meaningful visualization.

## Priority Order

1. Get classes.R working first (foundation)
2. Then simulation.R with minimal sim_run()
3. Then market-models.R with just GBM
4. Then cash-flows.R with basic flows
5. Then get a minimal example working
6. Finally add analysis and plotting

Focus on getting a minimal working version before adding features. The design doc has all the detailed code snippets you need.

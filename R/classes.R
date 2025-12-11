
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

# Historical bootstrap model
HistoricalModel <- new_class("HistoricalModel",
  parent = MarketModel,
  properties = list(
    historical_returns = new_property(class_numeric),
    block_size = new_property(class_numeric, default = 12)
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
    periods = new_property(class_numeric, default = NULL)
  )
)

# Withdrawal flow
WithdrawalFlow <- new_class("WithdrawalFlow",
  parent = CashFlow,
  properties = list(
    amount = new_property(class_numeric),
    inflation_adjusted = new_property(class_logical, default = TRUE),
    inflation_rate = new_property(class_numeric, default = 0.03)
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
    allocations = new_property(class_list)
  )
)

# Phase (accumulation, distribution, etc)
Phase <- new_class("Phase",
  properties = list(
    start_age = new_property(class_numeric),
    end_age = new_property(class_numeric),
    cash_flows = new_property(class_list, default = list()),
    strategy = new_property(class_any, default = NULL)
  )
)

# Main simulation object
Simulation <- new_class("Simulation",
  properties = list(
    n_simulations = new_property(class_numeric, default = 1000),
    periods_per_year = new_property(class_numeric, default = 12),
    start_age = new_property(class_numeric),
    end_age = new_property(class_numeric),
    seed = new_property(class_numeric),
    initial_balance = new_property(class_numeric, default = 0),
    phases = new_property(class_list, default = list()),
    market_models = new_property(class_list, default = list())
  )
)

# Simulation results container
SimResults <- new_class("SimResults",
  properties = list(
    trajectories = new_property(class_any),
    periods_per_year = new_property(class_numeric),
    start_age = new_property(class_numeric),
    metadata = new_property(class_list)
  )
)

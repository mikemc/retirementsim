#' Create a new simulation
#'
#' @param n_simulations Number of Monte Carlo simulations to run
#' @param periods_per_year Number of periods per year (12 for monthly, 1 for
#'   annual)
#' @param start_age Starting age for simulation
#' @param end_age Ending age for simulation
#' @param initial_balance Starting portfolio balance (default 0)
#' @param seed Random seed for reproducibility
#' @return A Simulation object
#' @include classes.R
#' @export
sim_define <- function(
  n_simulations = 1000,
  periods_per_year = 12,
  start_age = 30,
  end_age = 95,
  initial_balance = 0,
  seed = as.numeric(Sys.time())
) {
  Simulation(
    n_simulations = n_simulations,
    periods_per_year = periods_per_year,
    start_age = start_age,
    end_age = end_age,
    initial_balance = initial_balance,
    seed = seed
  )
}

#' Add a phase to the simulation
#'
#' @param sim A Simulation object
#' @param phase A Phase object
#' @return Modified Simulation object
#' @export
add_phase <- function(sim, phase) {
  sim@phases <- append(sim@phases, list(phase))
  sim
}

#' Create accumulation phase
#'
#' @param from_age Starting age for phase
#' @param to_age Ending age for phase
#' @param contribution ContributionFlow object
#' @param strategy Strategy object
#' @return A Phase object
#' @export
accumulation_phase <- function(
  from_age,
  to_age,
  contribution = NULL,
  strategy = NULL
) {
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

#' Create distribution phase
#'
#' @param from_age Starting age for phase
#' @param to_age Ending age for phase
#' @param withdrawal WithdrawalFlow object
#' @param strategy Strategy object
#' @return A Phase object
#' @export
distribution_phase <- function(
  from_age,
  to_age,
  withdrawal = NULL,
  strategy = NULL
) {
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

#' Create holding phase
#'
#' @param from_age Starting age for phase
#' @param to_age Ending age for phase
#' @param strategy Strategy object
#' @return A Phase object
#' @export
holding_phase <- function(from_age, to_age, strategy = NULL) {
  Phase(
    start_age = from_age,
    end_age = to_age,
    strategy = strategy
  )
}

#' Add global market model
#'
#' @param sim A Simulation object
#' @param model A MarketModel object
#' @param asset Asset name
#' @return Modified Simulation object
#' @export
add_market_model <- function(sim, model, asset = "stocks") {
  sim@global_market_models[[asset]] <- model
  sim
}

#' Main simulation runner
#'
#' @param sim A Simulation object
#' @return SimResults object
#' @export
sim_run <- function(sim) {
  if (!is.null(sim@seed)) {
    set.seed(sim@seed)
  }

  n_periods <- (sim@end_age - sim@start_age) * sim@periods_per_year

  # Initialize portfolio matrix
  portfolios <- matrix(sim@initial_balance, nrow = sim@n_simulations, ncol = n_periods + 1)

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

  # Sort phases by start_age
  sim@phases <- sim@phases[order(sapply(sim@phases, function(p) p@start_age))]

  # Validate full coverage: no gaps, no overlaps
  if (length(sim@phases) > 0) {
    # Check first phase starts at simulation start
    if (sim@phases[[1]]@start_age != sim@start_age) {
      stop(sprintf("First phase starts at age %d, but simulation starts at age %d. Add a phase to cover the gap.",
                   sim@phases[[1]]@start_age, sim@start_age))
    }

    # Check last phase ends at simulation end
    if (sim@phases[[length(sim@phases)]]@end_age != sim@end_age) {
      stop(sprintf("Last phase ends at age %d, but simulation ends at age %d. Add a phase to cover the gap.",
                   sim@phases[[length(sim@phases)]]@end_age, sim@end_age))
    }

    # Check for gaps and overlaps between phases
    if (length(sim@phases) > 1) {
      for (i in 1:(length(sim@phases) - 1)) {
        current_end <- sim@phases[[i]]@end_age
        next_start <- sim@phases[[i + 1]]@start_age

        if (next_start > current_end) {
          stop(sprintf("Gap detected: phase ending at age %d is followed by phase starting at age %d. Add a holding_phase(%d, %d) to fill the gap.",
                       current_end, next_start, current_end, next_start))
        } else if (next_start < current_end) {
          stop(sprintf("Overlap detected: phase ending at age %d overlaps with phase starting at age %d.",
                       current_end, next_start))
        }
      }
    }
  } else {
    stop("No phases defined. Add at least one phase using add_phase().")
  }

  # Process each phase
  for (phase in sim@phases) {
    phase_start_period <- age_to_period(phase@start_age, sim@start_age, sim@periods_per_year)
    phase_end_period <- age_to_period(phase@end_age, sim@start_age, sim@periods_per_year)
    phase_periods <- phase_end_period - phase_start_period + 1

    # Get phase-specific returns (subset of global returns)
    phase_returns <- lapply(all_returns, function(r) {
      if (phase_end_period <= ncol(r)) {
        r[, phase_start_period:phase_end_period, drop = FALSE]
      } else {
        r[, phase_start_period:ncol(r), drop = FALSE]
      }
    })

    # Get returns to apply if available
    returns_to_apply <- NULL
    if (length(phase_returns) > 0) {
      returns_to_apply <- phase_returns[[1]]  # Just use first asset for now
    }

    # Pre-generate all cash flows for this phase
    phase_cash_flows <- list()
    for (flow in phase@cash_flows) {
      phase_cash_flows[[length(phase_cash_flows) + 1]] <- list(
        flow = flow,
        amounts = generate_cash_flows(flow, phase_periods, sim@periods_per_year)
      )
    }

    # Process each period: carry forward value, apply cash flows, apply returns
    for (period in 1:phase_periods) {
      global_period <- phase_start_period + period - 1

      if (global_period <= n_periods) {
        # Start with previous period's ending value
        portfolios[, global_period + 1] <- portfolios[, global_period]

        # Apply all cash flows for this period
        for (flow_data in phase_cash_flows) {
          if (S7_inherits(flow_data$flow, ContributionFlow)) {
            portfolios[, global_period + 1] <- portfolios[, global_period + 1] + flow_data$amounts[period]
          } else if (S7_inherits(flow_data$flow, WithdrawalFlow)) {
            portfolios[, global_period + 1] <- pmax(portfolios[, global_period + 1] - flow_data$amounts[period], 0)
          }
        }

        # Apply returns to get end-of-period value
        if (!is.null(returns_to_apply)) {
          portfolios[, global_period + 1] <- portfolios[, global_period + 1] * returns_to_apply[, period]
        }
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

#' Helper function for age to period conversion
#'
#' @param age Age to convert
#' @param start_age Starting age of simulation
#' @param periods_per_year Number of periods per year
#' @return Period number
#' @export
age_to_period <- function(age, start_age, periods_per_year) {
  (age - start_age) * periods_per_year + 1
}

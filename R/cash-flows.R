#' Generic for generating cash flow amounts
#'
#' Generate cash flows for contributions (from a ContributionFlow object) or
#' for withdrawals (from a WithdrawalFlow object).
#'
#' @param flow A CashFlow object
#' @param n_periods Number of periods
#' @param periods_per_year Number of periods per year
#' @return Numeric vector of cash flow amounts
#' @include classes.R
#' @name generate_cash_flows
#' @export
generate_cash_flows <- new_generic("generate_cash_flows", "flow")

#' @export
method(generate_cash_flows, ContributionFlow) <- function(flow, n_periods, periods_per_year) {
  amounts <- numeric(n_periods)

  # Calculate which periods get contributions
  if (length(flow@periods) == 0) {
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

#' @export
method(generate_cash_flows, WithdrawalFlow) <- function(flow, n_periods, periods_per_year) {
  amounts <- numeric(n_periods)

  # For now, implement simple constant withdrawal
  # TODO: Add inflation adjustment and other withdrawal rules
  for (i in 1:n_periods) {
    if (flow@inflation_adjusted) {
      # Simple inflation adjustment (could be enhanced)
      year_fraction <- (i - 1) / periods_per_year
      amounts[i] <- flow@amount * (1.03)^year_fraction  # Assume 3% inflation
    } else {
      amounts[i] <- flow@amount
    }
  }

  amounts
}

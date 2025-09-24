test_that("Simulation object creation works", {
  sim <- sim_define(
    n_simulations = 100,
    periods_per_year = 12,
    start_age = 30,
    end_age = 65
  )

  expect_s7_class(sim, Simulation)
  expect_equal(sim@n_simulations, 100)
  expect_equal(sim@periods_per_year, 12)
  expect_equal(sim@start_age, 30)
  expect_equal(sim@end_age, 65)
})

test_that("GBM model generates returns of correct dimensions", {
  model <- GBMModel(
    mean_return = 0.07,
    sd_return = 0.18,
    return_period = "annual"
  )

  n_sims <- 100
  n_periods <- 60
  periods_per_year <- 12

  returns <- generate_returns(model, n_sims, n_periods, periods_per_year)

  expect_equal(nrow(returns), n_sims)
  expect_equal(ncol(returns), n_periods)
  expect_true(all(returns > 0))  # GBM returns should be positive
})

test_that("Cash flows are generated correctly", {
  # Test contribution flow
  contribution <- ContributionFlow(
    amount = 1000,
    growth_rate = 0.03
  )

  n_periods <- 12
  periods_per_year <- 12

  amounts <- generate_cash_flows(contribution, n_periods, periods_per_year)

  expect_length(amounts, n_periods)
  expect_equal(amounts[1], 1000)
  # Check that amounts grow over time
  expect_true(amounts[12] > amounts[1])

  # Test withdrawal flow
  withdrawal <- WithdrawalFlow(
    amount = 5000,
    inflation_adjusted = FALSE
  )

  amounts <- generate_cash_flows(withdrawal, n_periods, periods_per_year)

  expect_length(amounts, n_periods)
  expect_true(all(amounts == 5000))  # Should be constant without inflation
})

test_that("A simple simulation runs without errors", {
  sim <- sim_define(
    n_simulations = 100,
    periods_per_year = 12,
    start_age = 30,
    end_age = 40,
    seed = 123
  )

  sim <- add_market_model(
    sim,
    GBMModel(mean_return = 0.07, sd_return = 0.18, return_period = "annual"),
    asset = "stocks"
  )

  sim <- add_phase(
    sim,
    accumulation_phase(
      from_age = 30,
      to_age = 35,
      contribution = ContributionFlow(amount = 1000, growth_rate = 0.03)
    )
  )

  sim <- add_phase(
    sim,
    distribution_phase(
      from_age = 35,
      to_age = 40,
      withdrawal = WithdrawalFlow(amount = 2000, inflation_adjusted = FALSE)
    )
  )

  expect_no_error({
    results <- sim_run(sim)
  })

  results <- sim_run(sim)

  expect_s7_class(results, SimResults)
  expect_equal(nrow(results@trajectories), 100)  # n_simulations
  expect_equal(ncol(results@trajectories), 121)  # (40-30)*12 + 1

  # Check that we can calculate summary statistics
  summary_stats <- sim_summary(results)
  expect_type(summary_stats, "list")
  expect_true("success_rate" %in% names(summary_stats))

  success_rate <- sim_success_rate(results)
  expect_true(success_rate >= 0 && success_rate <= 1)
})

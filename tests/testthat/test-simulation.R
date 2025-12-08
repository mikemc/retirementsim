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

test_that("initial_balance parameter works correctly", {
  # Test with initial_balance set
  sim <- sim_define(
    n_simulations = 10,
    start_age = 65,
    end_age = 70,
    initial_balance = 1000000,
    seed = 123
  ) |>
    add_market_model(GBMModel(mean_return = 0.07, sd_return = 0.18)) |>
    add_phase(distribution_phase(65, 70,
      withdrawal = WithdrawalFlow(amount = 4000)))

  results <- sim_run(sim)

  # Verify all simulations start with initial_balance
  expect_true(all(results@trajectories[, 1] == 1000000))

  # Test default behavior (initial_balance = 0)
  sim_default <- sim_define(
    n_simulations = 10,
    start_age = 30,
    end_age = 35,
    seed = 123
  ) |>
    add_market_model(GBMModel(mean_return = 0.07, sd_return = 0.18)) |>
    add_phase(holding_phase(30, 35))

  results_default <- sim_run(sim_default)
  expect_true(all(results_default@trajectories[, 1] == 0))
})

test_that("WithdrawalFlow respects inflation_rate parameter", {
  n_periods <- 12
  periods_per_year <- 12

  # Test 1: Default 3% inflation
  withdrawal_default <- WithdrawalFlow(amount = 1000, inflation_adjusted = TRUE)
  amounts_default <- generate_cash_flows(withdrawal_default, n_periods, periods_per_year)

  expect_length(amounts_default, n_periods)
  expect_equal(amounts_default[1], 1000)
  # After 1 year (12 periods), should be approximately 1000 * 1.03
  expect_equal(amounts_default[12], 1000 * (1.03)^((12-1)/12), tolerance = 0.01)

  # Test 2: Custom 0% inflation (should be constant)
  withdrawal_0 <- WithdrawalFlow(amount = 1000, inflation_adjusted = TRUE, inflation_rate = 0)
  amounts_0 <- generate_cash_flows(withdrawal_0, n_periods, periods_per_year)

  expect_length(amounts_0, n_periods)
  expect_true(all(amounts_0 == 1000))

  # Test 3: Custom 2% inflation
  withdrawal_2 <- WithdrawalFlow(amount = 1000, inflation_adjusted = TRUE, inflation_rate = 0.02)
  amounts_2 <- generate_cash_flows(withdrawal_2, n_periods, periods_per_year)

  expect_length(amounts_2, n_periods)
  expect_equal(amounts_2[1], 1000)
  # After 1 year, should be approximately 1000 * 1.02
  expect_equal(amounts_2[12], 1000 * (1.02)^((12-1)/12), tolerance = 0.01)

  # Test 4: Custom 5% inflation
  withdrawal_5 <- WithdrawalFlow(amount = 1000, inflation_adjusted = TRUE, inflation_rate = 0.05)
  amounts_5 <- generate_cash_flows(withdrawal_5, n_periods, periods_per_year)

  expect_length(amounts_5, n_periods)
  expect_equal(amounts_5[1], 1000)
  # After 1 year, should be approximately 1000 * 1.05
  expect_equal(amounts_5[12], 1000 * (1.05)^((12-1)/12), tolerance = 0.01)

  # Verify that higher inflation rates result in higher amounts
  expect_true(amounts_5[12] > amounts_2[12])
  expect_true(amounts_2[12] > amounts_0[12])
})

test_that("WithdrawalFlow inflation_rate works with different period frequencies", {
  # Test with yearly periods
  n_periods_yearly <- 10
  periods_per_year_yearly <- 1

  withdrawal_yearly <- WithdrawalFlow(amount = 1000, inflation_adjusted = TRUE, inflation_rate = 0.03)
  amounts_yearly <- generate_cash_flows(withdrawal_yearly, n_periods_yearly, periods_per_year_yearly)

  expect_length(amounts_yearly, n_periods_yearly)
  expect_equal(amounts_yearly[1], 1000)
  # After 1 year (period 2), should be 1000 * 1.03
  expect_equal(amounts_yearly[2], 1000 * 1.03, tolerance = 0.01)
  # After 9 years (period 10), should be 1000 * 1.03^9
  expect_equal(amounts_yearly[10], 1000 * (1.03)^9, tolerance = 0.01)

  # Test with monthly periods
  n_periods_monthly <- 24
  periods_per_year_monthly <- 12

  withdrawal_monthly <- WithdrawalFlow(amount = 1000, inflation_adjusted = TRUE, inflation_rate = 0.03)
  amounts_monthly <- generate_cash_flows(withdrawal_monthly, n_periods_monthly, periods_per_year_monthly)

  expect_length(amounts_monthly, n_periods_monthly)
  expect_equal(amounts_monthly[1], 1000)
  # After 1 year (period 13), should be approximately 1000 * 1.03
  expect_equal(amounts_monthly[13], 1000 * (1.03)^(12/12), tolerance = 0.01)
  # After 2 years (period 24), should be approximately 1000 * 1.03^2
  expect_equal(amounts_monthly[24], 1000 * (1.03)^(23/12), tolerance = 0.01)
})

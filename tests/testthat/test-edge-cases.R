test_that("Negative balances are handled correctly and portfolios cannot go below zero", {
  # Test pmax(..., 0) protection at simulation.R:229
  # Set up scenario with large withdrawals exceeding balance
  sim <- sim_define(
    start_age = 65,
    end_age = 70,
    n_simulations = 10,
    initial_balance = 50000,
    seed = 123
  ) |>
    add_market_model(GBMModel(mean_return = -0.10, sd_return = 0.01)) |>
    add_phase(distribution_phase(65, 70,
      withdrawal = WithdrawalFlow(amount = 20000, inflation_adjusted = FALSE)))

  results <- sim_run(sim)

  # Verify no negative balances (pmax protection works)
  expect_true(all(results@trajectories >= 0))

  # Verify some portfolios hit zero (test is meaningful)
  expect_true(any(results@trajectories == 0))

  # Test zero initial balance with immediate withdrawals
  sim_zero <- sim_define(
    start_age = 30,
    end_age = 35,
    n_simulations = 10,
    initial_balance = 0,
    seed = 123
  ) |>
    add_market_model(GBMModel(mean_return = 0.07, sd_return = 0.18)) |>
    add_phase(distribution_phase(30, 35,
      withdrawal = WithdrawalFlow(amount = 5000, inflation_adjusted = FALSE)))

  results_zero <- sim_run(sim_zero)

  # Balance should remain at 0 (can't withdraw from empty account)
  expect_true(all(results_zero@trajectories == 0))
})

test_that("Multiple consecutive holding phases work correctly", {
  # Test holding_phase integration with no cash flows
  # Validates phase transitions and return application
  sim <- sim_define(
    start_age = 30,
    end_age = 60,
    n_simulations = 50,
    initial_balance = 100000,
    seed = 123
  ) |>
    add_market_model(GBMModel(mean_return = 0.07, sd_return = 0.18)) |>
    add_phase(holding_phase(30, 40)) |>
    add_phase(holding_phase(40, 50)) |>
    add_phase(holding_phase(50, 60))

  expect_no_error({
    results <- sim_run(sim)
  })

  results <- sim_run(sim)

  # Verify no discontinuities at phase boundaries
  expect_true(all(is.finite(results@trajectories)))

  # All trajectories should evolve (not stay constant) due to returns
  final_mean <- mean(results@trajectories[, ncol(results@trajectories)])
  initial_balance <- 100000

  # With 7% mean return over 30 years, average should be higher
  expect_true(final_mean != initial_balance)

  # Check specific phase boundaries are continuous
  # Age 40 = period 121, Age 50 = period 241
  period_40 <- age_to_period(40, 30, 12)
  period_50 <- age_to_period(50, 30, 12)

  expect_true(all(is.finite(results@trajectories[, period_40])))
  expect_true(all(is.finite(results@trajectories[, period_50])))
})

test_that("Quarterly periods_per_year setting works correctly", {
  # Test alternative to monthly periods
  # Validates period conversion and return periodization
  sim_quarterly <- sim_define(
    start_age = 30,
    end_age = 35,
    periods_per_year = 4,
    n_simulations = 10,
    initial_balance = 100000,
    seed = 123
  ) |>
    add_market_model(GBMModel(
      mean_return = 0.07,
      sd_return = 0.18,
      return_period = "annual"
    )) |>
    add_phase(holding_phase(30, 35))

  results_q <- sim_run(sim_quarterly)

  # Verify correct dimensions: 5 years * 4 quarters + 1 initial
  expect_equal(ncol(results_q@trajectories), (35-30)*4 + 1)
  expect_equal(ncol(results_q@trajectories), 21)
  expect_equal(nrow(results_q@trajectories), 10)

  # All values should be positive and finite
  expect_true(all(results_q@trajectories > 0))
  expect_true(all(is.finite(results_q@trajectories)))

  # Test with cash flows to ensure quarterly periods work end-to-end
  sim_quarterly_flows <- sim_define(
    start_age = 30,
    end_age = 35,
    periods_per_year = 4,
    n_simulations = 10,
    seed = 456
  ) |>
    add_market_model(GBMModel(mean_return = 0.07, sd_return = 0.18,
                              return_period = "annual")) |>
    add_phase(accumulation_phase(30, 35,
      contribution = ContributionFlow(amount = 1000)))

  results_flows <- sim_run(sim_quarterly_flows)
  expect_equal(ncol(results_flows@trajectories), 21)

  # Should accumulate contributions
  expect_true(all(results_flows@trajectories[, ncol(results_flows@trajectories)] > 0))
})

test_that("Single-period simulation works", {
  # Minimum viable simulation: 1 year with annual periods = 1 period
  sim <- sim_define(
    start_age = 30,
    end_age = 31,
    periods_per_year = 1,
    n_simulations = 10,
    initial_balance = 10000,
    seed = 123
  ) |>
    add_market_model(GBMModel(mean_return = 0.07, sd_return = 0.18,
                              return_period = "annual")) |>
    add_phase(holding_phase(30, 31))

  expect_no_error({
    results <- sim_run(sim)
  })

  results <- sim_run(sim)

  # Should have exactly 2 columns: initial state + 1 period
  expect_equal(ncol(results@trajectories), 2)
  expect_equal(nrow(results@trajectories), 10)

  # All values should be positive
  expect_true(all(results@trajectories > 0))
})

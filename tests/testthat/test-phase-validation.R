test_that("First phase must start at simulation start_age", {
  sim <- sim_define(start_age = 30, end_age = 65, n_simulations = 10) |>
    add_market_model(GBMModel(mean_return = 0.07, sd_return = 0.18)) |>
    add_phase(accumulation_phase(35, 65,
      contribution = ContributionFlow(amount = 1000)))

  expect_error(
    sim_run(sim),
    "First phase starts at age 35.*simulation starts at age 30"
  )
})

test_that("Last phase must end at simulation end_age", {
  sim <- sim_define(start_age = 30, end_age = 65, n_simulations = 10) |>
    add_market_model(GBMModel(mean_return = 0.07, sd_return = 0.18)) |>
    add_phase(accumulation_phase(30, 60,
      contribution = ContributionFlow(amount = 1000)))

  expect_error(
    sim_run(sim),
    "Last phase ends at age 60.*simulation ends at age 65"
  )
})

test_that("Gap between phases is detected", {
  sim <- sim_define(start_age = 30, end_age = 65, n_simulations = 10) |>
    add_market_model(GBMModel(mean_return = 0.07, sd_return = 0.18)) |>
    add_phase(accumulation_phase(30, 40,
      contribution = ContributionFlow(amount = 1000))) |>
    add_phase(distribution_phase(45, 65,
      withdrawal = WithdrawalFlow(amount = 2000)))

  expect_error(
    sim_run(sim),
    "Gap detected.*phase ending at age 40.*phase starting at age 45.*holding_phase\\(40, 45\\)"
  )
})

test_that("Overlap between phases is detected", {
  sim <- sim_define(start_age = 30, end_age = 65, n_simulations = 10) |>
    add_market_model(GBMModel(mean_return = 0.07, sd_return = 0.18)) |>
    add_phase(accumulation_phase(30, 45,
      contribution = ContributionFlow(amount = 1000))) |>
    add_phase(distribution_phase(40, 65,
      withdrawal = WithdrawalFlow(amount = 2000)))

  expect_error(
    sim_run(sim),
    "Overlap detected.*phase ending at age 45.*phase starting at age 40"
  )
})

test_that("Valid continuous phase sequence runs successfully", {
  sim <- sim_define(start_age = 30, end_age = 65, n_simulations = 10, seed = 123) |>
    add_market_model(GBMModel(mean_return = 0.07, sd_return = 0.18)) |>
    add_phase(accumulation_phase(30, 40,
      contribution = ContributionFlow(amount = 1000))) |>
    add_phase(holding_phase(40, 50)) |>
    add_phase(distribution_phase(50, 65,
      withdrawal = WithdrawalFlow(amount = 2000)))

  expect_no_error({
    results <- sim_run(sim)
  })

  results <- sim_run(sim)
  expect_s7_class(results, SimResults)
  expect_equal(nrow(results@trajectories), 10)
  expect_equal(ncol(results@trajectories), (65-30)*12 + 1)
})

test_that("No phases defined error", {
  sim <- sim_define(start_age = 30, end_age = 65, n_simulations = 10) |>
    add_market_model(GBMModel(mean_return = 0.07, sd_return = 0.18))

  expect_error(
    sim_run(sim),
    "No phases defined.*Add at least one phase"
  )
})

test_that("Phases are automatically sorted by start_age", {
  # Add phases in non-chronological order
  sim <- sim_define(start_age = 30, end_age = 65, n_simulations = 10, seed = 123) |>
    add_market_model(GBMModel(mean_return = 0.07, sd_return = 0.18)) |>
    add_phase(distribution_phase(50, 65,
      withdrawal = WithdrawalFlow(amount = 2000))) |>
    add_phase(holding_phase(40, 50)) |>
    add_phase(accumulation_phase(30, 40,
      contribution = ContributionFlow(amount = 1000)))

  expect_no_error({
    results <- sim_run(sim)
  })

  results <- sim_run(sim)
  expect_s7_class(results, SimResults)

  # Verify results are correct despite out-of-order phase addition
  expect_equal(nrow(results@trajectories), 10)
  expect_equal(ncol(results@trajectories), (65-30)*12 + 1)

  # All trajectories should have positive balances at some point
  # (accumulation phase should build up balance)
  expect_true(all(apply(results@trajectories, 1, max) > 0))
})

test_that("Single phase covering entire simulation period works", {
  sim <- sim_define(start_age = 30, end_age = 65, n_simulations = 10, seed = 123) |>
    add_market_model(GBMModel(mean_return = 0.07, sd_return = 0.18)) |>
    add_phase(accumulation_phase(30, 65,
      contribution = ContributionFlow(amount = 1000)))

  expect_no_error({
    results <- sim_run(sim)
  })

  results <- sim_run(sim)
  expect_s7_class(results, SimResults)
  expect_equal(nrow(results@trajectories), 10)
  expect_equal(ncol(results@trajectories), (65-30)*12 + 1)
})

test_that("Two phases with exact boundary alignment work", {
  sim <- sim_define(start_age = 30, end_age = 65, n_simulations = 10, seed = 123) |>
    add_market_model(GBMModel(mean_return = 0.07, sd_return = 0.18)) |>
    add_phase(accumulation_phase(30, 50,
      contribution = ContributionFlow(amount = 1000))) |>
    add_phase(distribution_phase(50, 65,
      withdrawal = WithdrawalFlow(amount = 2000)))

  expect_no_error({
    results <- sim_run(sim)
  })

  results <- sim_run(sim)
  expect_s7_class(results, SimResults)

  # Verify smooth transition at age 50
  period_50 <- age_to_period(50, 30, 12)
  expect_true(all(is.finite(results@trajectories[, period_50])))
  expect_true(all(results@trajectories[, period_50] >= 0))
})

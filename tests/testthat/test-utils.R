test_that("years_to_periods converts correctly", {
  # Monthly periods
  expect_equal(years_to_periods(1, 12), 12)
  expect_equal(years_to_periods(10, 12), 120)

  # Annual periods
  expect_equal(years_to_periods(5, 1), 5)
  expect_equal(years_to_periods(1, 1), 1)

  # Quarterly periods
  expect_equal(years_to_periods(2, 4), 8)

  # Fractional years
  expect_equal(years_to_periods(0.5, 12), 6)
  expect_equal(years_to_periods(2.5, 4), 10)

  # Zero edge case
  expect_equal(years_to_periods(0, 12), 0)
  expect_equal(years_to_periods(0, 1), 0)
})

test_that("periods_to_years converts correctly", {
  # Monthly periods
  expect_equal(periods_to_years(12, 12), 1)
  expect_equal(periods_to_years(120, 12), 10)

  # Annual periods
  expect_equal(periods_to_years(5, 1), 5)
  expect_equal(periods_to_years(1, 1), 1)

  # Non-integer results
  expect_equal(periods_to_years(6, 12), 0.5)
  expect_equal(periods_to_years(18, 12), 1.5)

  # Quarterly periods
  expect_equal(periods_to_years(8, 4), 2)

  # Zero edge case
  expect_equal(periods_to_years(0, 12), 0)

  # Test inverse relationship with years_to_periods
  expect_equal(periods_to_years(years_to_periods(5, 12), 12), 5)
  expect_equal(periods_to_years(years_to_periods(3.5, 4), 4), 3.5)
  expect_equal(periods_to_years(years_to_periods(10, 1), 1), 10)
})

test_that("annualize_return calculates correctly", {
  # 1% monthly return
  expect_equal(annualize_return(0.01, 12), (1.01)^12 - 1, tolerance = 0.0001)

  # 2% quarterly return
  expect_equal(annualize_return(0.02, 4), (1.02)^4 - 1, tolerance = 0.0001)

  # Annual return (identity)
  expect_equal(annualize_return(0.07, 1), 0.07)
  expect_equal(annualize_return(0.10, 1), 0.10)

  # Zero return
  expect_equal(annualize_return(0, 12), 0)
  expect_equal(annualize_return(0, 1), 0)

  # Negative return
  expect_equal(annualize_return(-0.01, 12), (0.99)^12 - 1, tolerance = 0.0001)
  expect_equal(annualize_return(-0.02, 4), (0.98)^4 - 1, tolerance = 0.0001)

  # Common scenario: ~0.58% monthly = ~7% annual
  monthly_ret <- 0.005654145
  annual_ret <- annualize_return(monthly_ret, 12)
  expect_equal(annual_ret, 0.07, tolerance = 0.001)
})

test_that("periodize_return calculates correctly", {
  # 7% annual to monthly
  expect_equal(periodize_return(0.07, 12), (1.07)^(1/12) - 1, tolerance = 0.0001)

  # 8% annual to quarterly
  expect_equal(periodize_return(0.08, 4), (1.08)^(1/4) - 1, tolerance = 0.0001)

  # Annual (identity)
  expect_equal(periodize_return(0.07, 1), 0.07)
  expect_equal(periodize_return(0.10, 1), 0.10)

  # Zero return
  expect_equal(periodize_return(0, 12), 0)
  expect_equal(periodize_return(0, 4), 0)

  # Negative return
  expect_equal(periodize_return(-0.05, 12), (0.95)^(1/12) - 1, tolerance = 0.0001)

  # Test inverse relationship with annualize_return
  annual_ret <- 0.07
  monthly_ret <- periodize_return(annual_ret, 12)
  expect_equal(annualize_return(monthly_ret, 12), annual_ret, tolerance = 0.0001)

  annual_ret2 <- 0.08
  quarterly_ret <- periodize_return(annual_ret2, 4)
  expect_equal(annualize_return(quarterly_ret, 4), annual_ret2, tolerance = 0.0001)

  # Test with various periods_per_year
  expect_equal(annualize_return(periodize_return(0.10, 1), 1), 0.10, tolerance = 0.0001)
  expect_equal(annualize_return(periodize_return(0.05, 6), 6), 0.05, tolerance = 0.0001)
})

test_that("age_to_period helper function works correctly", {
  # Start of simulation (monthly)
  expect_equal(age_to_period(30, 30, 12), 1)

  # One year later (monthly)
  expect_equal(age_to_period(31, 30, 12), 13)

  # Two years later (monthly)
  expect_equal(age_to_period(32, 30, 12), 25)

  # 10 years later (monthly)
  expect_equal(age_to_period(40, 30, 12), 121)

  # Annual periods
  expect_equal(age_to_period(30, 30, 1), 1)
  expect_equal(age_to_period(35, 30, 1), 6)
  expect_equal(age_to_period(65, 30, 1), 36)

  # Quarterly periods
  expect_equal(age_to_period(30, 30, 4), 1)
  expect_equal(age_to_period(31, 30, 4), 5)
  expect_equal(age_to_period(32, 30, 4), 9)

  # Edge case: retirement age scenario
  expect_equal(age_to_period(65, 30, 12), 421)
})

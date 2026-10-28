test_that("the same seed gives the same portfolio", {
  expect_identical(simulate_portfolio(500, seed = 1), simulate_portfolio(500, seed = 1))
})

test_that("charges equal the sum of claim amounts", {
  x <- simulate_portfolio(2000, seed = 2, claims = TRUE)
  expect_equal(sum(x$claims$Amount), sum(x$policies$Charges), tolerance = 1e-6)
  expect_equal(nrow(x$claims), sum(x$policies$ClaimCount))
  expect_true(all(x$policies$Charges[x$policies$ClaimCount == 0] == 0))
})

test_that("the base profile has the base frequency and severity", {
  base <- data.frame(Age = 40, Sex = "female", BMI = 25, Smoker = "no",
                     Province = "Gauteng", Plan = "Essential", Children = 0)
  expect_equal(true_frequency(base), 0.30)
  expect_equal(true_severity(base), 45000)
})

test_that("the simulated frequency is close to the truth", {
  p <- portfolio_params(frailty_var = 0)
  df <- simulate_portfolio(2e4, seed = 3, params = p)
  expected <- sum(true_frequency(df, p) * df$Exposure)
  expect_lt(abs(sum(df$ClaimCount) / expected - 1), 0.02)
})

test_that("parameters are checked", {
  expect_error(portfolio_params(freq_smokr = 1), "Unknown parameter")
  expect_error(portfolio_params(freq_plan = c(0, 1)), "length 3")
  expect_error(portfolio_params(large_alpha = 1.5), "large_alpha")
  expect_equal(portfolio_params(freq_smoker = 0.5)$freq_smoker, 0.5)
})

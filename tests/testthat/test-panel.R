pn <- simulate_panel(3000, years = 3, seed = 4, truth = TRUE)

test_that("each member appears once per year until they leave", {
  expect_false(anyDuplicated(pn[, c("Policy_ID", "Calendar_Year")]) > 0)
  exits <- pn[pn$Exit != "none", ]
  later <- merge(exits[, c("Policy_ID", "Calendar_Year")], pn, by = "Policy_ID")
  expect_false(any(later$Calendar_Year.y > later$Calendar_Year.x))
})

test_that("members age one year at a time", {
  a <- pn[pn$Calendar_Year == 2021, c("Policy_ID", "Age")]
  b <- pn[pn$Calendar_Year == 2022, c("Policy_ID", "Age")]
  m <- merge(a, b, by = "Policy_ID")
  expect_true(all(m$Age.y == m$Age.x + 1))
})

test_that("paid charges are capped at the limit", {
  expect_true(all(pn$Charges_Paid <= pn$Annual_Limit))
  expect_identical(pn$Limit_Reached, as.integer(pn$Charges > pn$Annual_Limit))
})

test_that("wearable data is only recorded for wellness members", {
  expect_true(all(is.na(pn$Avg_Daily_Steps[pn$Wellness == "no"])))
  expect_false(anyNA(pn$Avg_Daily_Steps[pn$Wellness == "yes"]))
})

test_that("truth columns are optional", {
  expect_false("Frailty" %in% names(simulate_panel(200, years = 2, seed = 5)))
  expect_true(all(c("True_Frequency", "Frailty") %in% names(pn)))
})

test_that("panel parameters are checked", {
  expect_error(panel_params(lapse_rate = 0.1), "Unknown parameter")
  expect_error(panel_params(copula_theta = 0), "copula_theta")
})

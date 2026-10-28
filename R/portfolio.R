#' Simulate a cross-sectional health insurance portfolio
#'
#' Simulates one year of a portfolio of principal members with their
#' dependent children. Claim counts are Poisson given a gamma frailty, so
#' negative binomial overall, and every claim has its own cost from a
#' lognormal-Pareto mixture. A policy's `Charges` are the sum of its claims.
#'
#' With the defaults, `simulate_portfolio(1e5, seed = 123)` gives the
#' Kaggle dataset "Health Insurance Actuarial Playground".
#'
#' @param n Number of policies.
#' @param seed Random seed, or `NULL` to use the current RNG state.
#' @param params Parameters from [portfolio_params()].
#' @param claims If `TRUE`, return a list with the policies and a claim-level
#'   table (one row per claim) instead of the policies alone.
#' @param id If `TRUE`, add a `Policy_ID` column.
#' @return A data frame of policies with columns `Age`, `Sex`, `BMI`,
#'   `Smoker`, `Province`, `Plan`, `Children`, `Exposure`, `ClaimCount` and
#'   `Charges` (rand), or a list of `policies` and `claims` when
#'   `claims = TRUE`.
#' @export
#' @examples
#' df <- simulate_portfolio(1000, seed = 1)
#' head(df)
#' sum(df$ClaimCount) / sum(df$Exposure)
#'
#' # A portfolio where smoking costs more
#' df2 <- simulate_portfolio(1000, seed = 1, portfolio_params(freq_smoker = 0.5))
simulate_portfolio <- function(n = 1e5, seed = 123, params = portfolio_params(),
                               claims = FALSE, id = FALSE) {
  check_portfolio_params(params)
  if (!is.null(seed)) set.seed(seed)
  p <- params

  df <- draw_members(n, p)
  # Exposure: most members are covered all year, the rest join or leave.
  df$Exposure <- ifelse(stats::runif(n) < p$full_year_share, 1, round(stats::runif(n, 0.1, 1), 2))

  frailty <- draw_frailty(n, p$frailty_var)
  df$ClaimCount <- stats::rpois(n, true_frequency(df, p) * df$Exposure * frailty)

  cl <- draw_claims(df$ClaimCount, sev_log_mean(df, p), p)
  df$Charges <- 0
  if (nrow(cl) > 0) df$Charges[unique(cl$row)] <- rowsum(cl$amount, cl$row)[, 1]
  df$Charges <- round(df$Charges, 2)

  if (id || claims) df <- cbind(Policy_ID = sprintf("P%07d", seq_len(n)), df)
  if (!claims) return(df)
  claims_df <- data.frame(
    Claim_ID = sprintf("C%08d", seq_len(nrow(cl))),
    Policy_ID = df$Policy_ID[cl$row],
    Amount = round(cl$amount, 2)
  )
  list(policies = df, claims = claims_df)
}

#' True expected frequency and severity
#'
#' The expected claims per policy year and the expected cost per claim under
#' the data-generating parameters, for comparing with fitted models.
#' `true_frequency()` is the mean over the frailty, so it is the target of a
#' Poisson or negative binomial GLM with log exposure as offset.
#'
#' @param df A data frame with the portfolio columns.
#' @param params Parameters from [portfolio_params()].
#' @return A numeric vector, one value per row of `df`.
#' @export
#' @examples
#' df <- simulate_portfolio(1000, seed = 1)
#' mean(true_frequency(df))
#' mean(true_severity(df))
true_frequency <- function(df, params = portfolio_params()) exp(freq_log_rate(df, params))

#' @rdname true_frequency
#' @export
true_severity <- function(df, params = portfolio_params()) exp(sev_log_mean(df, params))

#' Moments of the claim-size multiplier
#'
#' `z_mean()` is the mean of the multiplier Z, used to scale claims to the
#' severity means. `z_cv2()` is the squared coefficient of variation of a
#' single claim, which is the true dispersion of a Gamma severity GLM.
#'
#' @param params Parameters from [portfolio_params()].
#' @return A number.
#' @export
#' @examples
#' z_cv2()
z_mean <- function(params = portfolio_params()) {
  p <- params
  (1 - p$large_prob) * exp(p$sev_sdlog^2 / 2) +
    p$large_prob * p$large_scale * p$large_alpha / (p$large_alpha - 1)
}

#' @rdname z_mean
#' @export
z_cv2 <- function(params = portfolio_params()) {
  p <- params
  a <- p$large_alpha
  m2 <- (1 - p$large_prob) * exp(2 * p$sev_sdlog^2) +
    p$large_prob * p$large_scale^2 * a / (a - 2)
  m2 / z_mean(p)^2 - 1
}

#' Set factor levels with the base profile first
#'
#' Converts `Sex`, `Smoker`, `Province` and `Plan` to factors whose first
#' level is the base profile, so GLM coefficients line up with the
#' parameters.
#'
#' @param df A data frame from [simulate_portfolio()] or [simulate_panel()].
#' @return `df` with factor columns.
#' @export
set_levels <- function(df) {
  df$Sex <- factor(df$Sex, levels = c("female", "male"))
  df$Smoker <- factor(df$Smoker, levels = c("no", "yes"))
  df$Province <- factor(df$Province, levels = hs_provinces())
  df$Plan <- factor(df$Plan, levels = hs_plans())
  df
}

freq_log_rate <- function(df, p) {
  female <- df$Sex == "female"
  p$freq_base +
    p$freq_age * (df$Age - 40) +
    p$freq_age_old * pmax(df$Age - 55, 0)^2 +
    p$freq_maternity * female * exp(-((df$Age - 31) / 6)^2) -
    p$freq_maternity * exp(-((40 - 31) / 6)^2) +  # keeps the base profile at freq_base
    p$freq_bmi_high * pmax(df$BMI - 25, 0) +
    p$freq_bmi_low * pmax(20 - df$BMI, 0) +
    p$freq_smoker * (df$Smoker == "yes") +
    p$freq_child * df$Children +
    p$freq_province[match(df$Province, hs_provinces())] +
    p$freq_plan[match(df$Plan, hs_plans())]
}

sev_log_mean <- function(df, p) {
  log(p$sev_base_mean) +
    p$sev_age * (df$Age - 40) +
    p$sev_bmi_high * pmax(df$BMI - 25, 0) +
    p$sev_smoker * (df$Smoker == "yes") +
    p$sev_child * df$Children +
    p$sev_male * (df$Sex == "male") +
    p$sev_province[match(df$Province, hs_provinces())] +
    p$sev_plan[match(df$Plan, hs_plans())]
}

# Principal members and their cover. age_beta shapes the age distribution
# on 18 to 85; new business in the panel is younger than the in-force book.
draw_members <- function(n, p, age_beta = c(2.0, 2.8)) {
  Age <- 18 + round(67 * stats::rbeta(n, age_beta[1], age_beta[2]))
  Sex <- sample(c("female", "male"), n, replace = TRUE,
                prob = c(p$female_share, 1 - p$female_share))
  Province <- sample(hs_provinces(), n, replace = TRUE, prob = p$province_share)

  # Dependent children peak for members around 40.
  Children <- pmin(stats::rpois(n, 1.8 * exp(-((Age - 40) / 11)^2)), 5)

  # Smoking is more common in men and falls with age.
  Smoker <- ifelse(
    stats::runif(n) < stats::plogis(-1.9 + 1.2 * (Sex == "male") - 0.015 * (Age - 40)),
    "yes", "no"
  )

  # BMI: higher for women, rising with age to 60, slightly lower for smokers.
  BMI <- stats::rlnorm(
    n,
    meanlog = log(26) + 0.07 * (Sex == "female") + 0.004 * (pmin(Age, 60) - 40) -
      0.04 * (Smoker == "yes"),
    sdlog = 0.17
  )
  BMI <- round(pmin(pmax(BMI, 16), 50), 1)

  # Plan choice: older members buy richer cover, families lean to Standard.
  u <- cbind(
    0,
    0.2 + 0.25 * (Children > 0),
    -0.9 + 0.035 * (Age - 40) - 0.3 * (Smoker == "yes")
  )
  pr <- exp(u) / rowSums(exp(u))
  r <- stats::runif(n)
  Plan <- hs_plans()[1 + (r > pr[, 1]) + (r > pr[, 1] + pr[, 2])]

  data.frame(Age, Sex, BMI, Smoker, Province, Plan, Children)
}

# Gamma frailty with mean 1; a constant 1 when the variance is zero.
draw_frailty <- function(n, var) {
  if (var == 0) return(rep(1, n))
  stats::rgamma(n, shape = 1 / var, scale = var)
}

# One row per claim: the policy row it belongs to and its amount, given each
# policy's claim count and log severity mean.
draw_claims <- function(counts, log_mean, p) {
  row <- rep(seq_along(counts), counts)
  k <- length(row)
  large <- stats::runif(k) < p$large_prob
  z <- ifelse(
    large,
    p$large_scale * stats::runif(k)^(-1 / p$large_alpha),
    stats::rlnorm(k, 0, p$sev_sdlog)
  )
  data.frame(row = row, amount = exp(log_mean[row]) / z_mean(p) * z)
}

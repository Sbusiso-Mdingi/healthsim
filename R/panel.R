#' Simulate a multi-year health insurance panel
#'
#' Follows an open cohort of principal members for several calendar years,
#' one row per member per year. The book starts with `n` members in force,
#' and each year new members join, members age, change BMI, quit smoking,
#' have children, claim, and lapse or die.
#'
#' Claims follow the portfolio model ([simulate_portfolio()]) with three
#' additions:
#' * Frailty has a lasting part and a yearly part, so past claims predict
#'   future claims.
#' * Daily steps and sleep efficiency lower claim frequency for everyone,
#'   but are only recorded for members of the wellness programme. The two are
#'   joined by a Clayton copula, so low activity and poor sleep tend to come
#'   together. Active members join the programme more often, so the missing
#'   values are not missing at random.
#' * Cost per claim grows with `claims_inflation`, while annual benefit
#'   limits stay fixed in rand, so more members reach the limit each year.
#'
#' Lapses happen at the end of a year and are more likely in the first
#' policy year and for members who did not claim, so the remaining book gets
#' worse over time. Deaths happen during the year and end exposure early.
#'
#' @param n Members in force at the start.
#' @param years Number of calendar years.
#' @param seed Random seed, or `NULL` to use the current RNG state.
#' @param params Parameters from [panel_params()].
#' @param truth If `TRUE`, add the latent values used to simulate each row:
#'   `True_Frequency` (expected claims per year given the member's activity,
#'   without frailty), `True_Severity` (expected cost per claim before the
#'   limit), `Frailty`, and `Steps_Latent` and `Sleep_Latent` for all members.
#' @return A data frame with one row per member per calendar year:
#'   `Policy_ID`, `Calendar_Year`, `Entry_Year`, `Duration` (policy year,
#'   1 in the year of joining), `Age`, `Sex`, `BMI`, `Smoker`, `Province`,
#'   `Plan`, `Children`, `Wellness`, `Avg_Daily_Steps` and `Sleep_Efficiency`
#'   (`NA` outside the wellness programme), `Exposure`, `ClaimCount`,
#'   `Charges` (incurred, rand), `Annual_Limit`, `Charges_Paid` (capped at the
#'   limit), `Limit_Reached`, and `Exit` (`"none"`, `"lapse"` or `"death"`).
#'   Members with `Exit == "none"` in the last year are still in force.
#' @export
#' @examples
#' pn <- simulate_panel(2000, years = 3, seed = 1)
#' table(pn$Calendar_Year, pn$Exit)
#'
#' # Faster claims inflation
#' pp <- panel_params(claims_inflation = 0.12)
#' pn2 <- simulate_panel(2000, years = 3, seed = 1, params = pp)
simulate_panel <- function(n = 1e5, years = 5, seed = 123, params = panel_params(),
                           truth = FALSE) {
  check_panel_params(params)
  if (!is.null(seed)) set.seed(seed)
  p <- params
  pf <- p$portfolio

  # Split the frailty variance V into a lasting part and a yearly part with
  # (1 + v_lasting) * (1 + v_yearly) = 1 + V.
  v_lasting <- p$frailty_persistent_share * pf$frailty_var
  v_yearly <- (1 + pf$frailty_var) / (1 + v_lasting) - 1

  # The book in force at the start joined over the previous years.
  st <- new_members(n, p, age_beta = c(2.0, 2.8), v_lasting = v_lasting)
  st$Entry_Year <- p$start_year - pmin(floor(stats::rexp(n, 1 / 4)), 15)
  st$Join_Fraction <- 0
  next_id <- n

  out <- vector("list", years)
  for (k in seq_len(years)) {
    year <- p$start_year + k - 1
    m <- nrow(st)
    st$Duration <- year - st$Entry_Year + 1

    # Activity and sleep from each member's fixed habits and current profile.
    st$Steps_Latent <- steps_from_u(st$u_steps, st, p)
    st$Sleep_Latent <- sleep_from_u(st$u_sleep, st, p)

    # Deaths during the year end exposure early.
    dies <- stats::runif(m) < pmin(p$death_a * exp(p$death_b * st$Age), 1)
    death_time <- stats::runif(m)
    exposure <- 1 - st$Join_Fraction
    exposure[dies] <- pmax(death_time[dies] - st$Join_Fraction[dies], 0.01)
    exposure <- round(exposure, 3)

    freq <- true_frequency(st, pf) *
      exp(p$freq_steps * (st$Steps_Latent - p$steps_mean) / 1000 +
            p$freq_sleep * (st$Sleep_Latent - p$sleep_mean))
    frailty <- st$Frailty_Lasting * draw_frailty(m, v_yearly)
    count <- stats::rpois(m, freq * exposure * frailty)

    log_sev <- sev_log_mean(st, pf) + (k - 1) * log(1 + p$claims_inflation)
    cl <- draw_claims(count, log_sev, pf)
    charges <- numeric(m)
    if (nrow(cl) > 0) charges[unique(cl$row)] <- rowsum(cl$amount, cl$row)[, 1]
    charges <- round(charges, 2)
    limit <- p$annual_limit[match(st$Plan, hs_plans())]

    # Lapses at the end of the year, among members still alive.
    lapse_lo <- stats::qlogis(p$lapse_base) +
      p$lapse_first_year * (st$Duration == 1) +
      p$lapse_age * (st$Age - 40) +
      p$lapse_plan[match(st$Plan, hs_plans())] +
      p$lapse_no_claims * (count == 0)
    lapses <- !dies & stats::runif(m) < stats::plogis(lapse_lo)

    rows <- data.frame(
      Policy_ID = st$Policy_ID,
      Calendar_Year = year,
      Entry_Year = st$Entry_Year,
      Duration = st$Duration,
      Age = st$Age, Sex = st$Sex, BMI = st$BMI, Smoker = st$Smoker,
      Province = st$Province, Plan = st$Plan, Children = st$Children,
      Wellness = ifelse(st$Wellness, "yes", "no"),
      Avg_Daily_Steps = ifelse(st$Wellness, round(st$Steps_Latent), NA),
      Sleep_Efficiency = ifelse(st$Wellness, round(st$Sleep_Latent, 3), NA),
      Exposure = exposure,
      ClaimCount = count,
      Charges = charges,
      Annual_Limit = limit,
      Charges_Paid = pmin(charges, limit),
      Limit_Reached = as.integer(charges > limit),
      Exit = ifelse(dies, "death", ifelse(lapses, "lapse", "none"))
    )
    if (truth) {
      rows$True_Frequency <- freq
      rows$True_Severity <- exp(log_sev)
      rows$Frailty <- frailty
      rows$Steps_Latent <- st$Steps_Latent
      rows$Sleep_Latent <- st$Sleep_Latent
    }
    out[[k]] <- rows
    if (k == years) break

    # Survivors move to next year.
    st <- st[!dies & !lapses, , drop = FALSE]
    st <- age_members(st, p)
    st$Join_Fraction <- 0

    # New business, younger than the in-force book, joining during the year.
    n_new <- round(p$new_business_rate * nrow(st))
    if (n_new > 0) {
      nb <- new_members(n_new, p, age_beta = c(1.6, 3.2), v_lasting = v_lasting,
                        first_id = next_id + 1)
      nb$Entry_Year <- year + 1
      nb$Join_Fraction <- stats::runif(n_new)
      next_id <- next_id + n_new
      st <- rbind(st[, names(nb)], nb)
    }
  }
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out
}

# Members with their lasting traits: frailty, habits and wellness programme.
new_members <- function(n, p, age_beta, v_lasting, first_id = 1) {
  st <- draw_members(n, p$portfolio, age_beta = age_beta)
  st$Policy_ID <- sprintf("H%07d", first_id + seq_len(n) - 1)
  st$Frailty_Lasting <- draw_frailty(n, v_lasting)
  uv <- rclayton(n, p$copula_theta)
  st$u_steps <- uv[, 1]
  st$u_sleep <- uv[, 2]
  join_lo <- stats::qlogis(p$wellness_base) - 0.02 * (st$Age - 40) -
    0.05 * (st$BMI - 26) - 0.6 * (st$Smoker == "yes") + 1.2 * (st$u_steps - 0.5)
  st$Wellness <- stats::runif(n) < stats::plogis(join_lo)
  st
}

# One year on: older, heavier, some smokers quit, families change.
age_members <- function(st, p) {
  m <- nrow(st)
  st$Age <- st$Age + 1
  st$BMI <- round(pmin(pmax(st$BMI + p$bmi_drift + stats::rnorm(m, 0, p$bmi_sd), 16), 50), 1)
  quits <- st$Smoker == "yes" & stats::runif(m) < p$quit_rate
  st$Smoker[quits] <- "no"
  births <- stats::runif(m) < p$birth_peak * exp(-((st$Age - 32) / 6)^2)
  leaves <- st$Age >= 48 & st$Children > 0 & stats::runif(m) < p$child_leave_rate
  st$Children <- pmin(st$Children + births - leaves, 5)
  st
}

# Clayton copula by the conditional method.
rclayton <- function(n, theta) {
  u <- stats::runif(n)
  w <- stats::runif(n)
  v <- (u^(-theta) * (w^(-theta / (1 + theta)) - 1) + 1)^(-1 / theta)
  cbind(u, v)
}

steps_from_u <- function(u, st, p) {
  mu <- p$steps_mean * exp(-0.006 * (st$Age - 40) - 0.02 * (st$BMI - 26) -
                             0.08 * (st$Smoker == "yes"))
  shape <- 1 / p$steps_cv^2
  stats::qgamma(u, shape = shape, scale = mu / shape)
}

sleep_from_u <- function(v, st, p) {
  # Beta margin with the given mean and standard deviation, so values stay
  # inside (0, 1) without piling up at a cap.
  mu <- p$sleep_mean - 0.001 * (st$Age - 40) - 0.002 * (st$BMI - 26)
  k <- mu * (1 - mu) / p$sleep_sd^2 - 1
  stats::qbeta(v, mu * k, (1 - mu) * k)
}

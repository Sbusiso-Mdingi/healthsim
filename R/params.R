#' Provinces and benefit tiers
#'
#' The nine South African provinces and the three benefit tiers used by the
#' simulators, in the order that parameter vectors follow.
#'
#' Essential covers in-hospital treatment; Standard adds out-of-hospital
#' specialists, scans and tests; Comprehensive adds GP visits, prescribed
#' medicines, dental and optical.
#'
#' @return A character vector.
#' @export
#' @examples
#' hs_provinces()
#' hs_plans()
hs_provinces <- function() {
  c(
    "Gauteng", "Western Cape", "KwaZulu-Natal", "Eastern Cape", "Free State",
    "Limpopo", "Mpumalanga", "North West", "Northern Cape"
  )
}

#' @rdname hs_provinces
#' @export
hs_plans <- function() c("Essential", "Standard", "Comprehensive")

#' Parameters of the cross-sectional portfolio
#'
#' Returns the default data-generating parameters of [simulate_portfolio()],
#' with any named argument replacing its default. Unknown names are an error,
#' so a typo cannot silently leave a default in place.
#'
#' Frequency terms are on the log scale of claims per policy year, relative
#' to a base profile: a 40-year-old woman in Gauteng on the Essential plan,
#' non-smoker, BMI 25, no children. Severity terms are on the log scale of
#' the expected cost per claim, in rand. Each claim is the severity scale
#' times Z, where Z is lognormal with probability `1 - large_prob` and a
#' Pareto large claim otherwise.
#'
#' The defaults are illustrative assumptions, not estimates from real data.
#'
#' @param ... Parameters to change, for example `freq_smoker = 0.4`.
#' @return A named list.
#' @export
#' @examples
#' p <- portfolio_params(freq_smoker = 0.4, frailty_var = 0)
#' p$freq_smoker
portfolio_params <- function(...) {
  defaults <- list(
    # Membership is concentrated in the metros.
    province_share = c(0.38, 0.17, 0.15, 0.07, 0.05, 0.05, 0.06, 0.05, 0.02),
    female_share = 0.52,

    # Claim frequency
    freq_base = log(0.30),
    freq_age = 0.025,              # per year of age
    freq_age_old = 0.0006,         # per squared year above 55
    freq_maternity = 0.45,         # peak extra log rate for women, at age 31
    freq_bmi_high = 0.035,         # per BMI point above 25
    freq_bmi_low = 0.05,           # per BMI point below 20
    freq_smoker = 0.26,
    freq_child = 0.22,             # per dependent child
    freq_province = c(0, -0.03, -0.05, -0.10, -0.08, -0.15, -0.10, -0.10, -0.12),
    freq_plan = c(0, log(4), log(10)),
    frailty_var = 0.3,             # gamma frailty: negative binomial with theta = 1 / frailty_var

    # Claim severity
    sev_base_mean = 45000,
    sev_age = 0.008,
    sev_bmi_high = 0.015,
    sev_smoker = 0.22,
    sev_child = log(0.93),
    sev_male = log(1.05),
    sev_province = c(0, 0.02, -0.04, -0.08, -0.07, -0.10, -0.06, -0.08, -0.09),
    sev_plan = c(0, log(0.45), log(0.25)),
    sev_sdlog = 1.1,
    large_prob = 0.03,
    large_scale = 6,
    large_alpha = 2.3,

    # Share of members covered for the whole year
    full_year_share = 0.8
  )
  p <- set_params(defaults, list(...))
  check_portfolio_params(p)
  p
}

#' Parameters of the panel
#'
#' Returns the default parameters of [simulate_panel()]. Claims follow
#' [portfolio_params()], passed as `portfolio`, plus the panel terms below.
#'
#' @param ... Panel parameters to change, for example `lapse_base = 0.15`.
#' @param portfolio Claims parameters, from [portfolio_params()].
#' @return A named list.
#' @export
#' @examples
#' p <- panel_params(claims_inflation = 0.10,
#'                   portfolio = portfolio_params(freq_smoker = 0.4))
panel_params <- function(..., portfolio = portfolio_params()) {
  defaults <- list(
    start_year = 2021,
    new_business_rate = 0.12,      # new members each year, as a share of those in force

    # Frailty is split into a lasting part, which makes claims history
    # predictive, and a part redrawn every year. Together they match
    # portfolio$frailty_var.
    frailty_persistent_share = 0.6,

    # Member changes from one year to the next
    bmi_drift = 0.1,               # BMI points per year
    bmi_sd = 0.6,
    quit_rate = 0.06,              # smokers who quit each year
    birth_peak = 0.10,             # yearly chance of a new child, peaking at age 32
    child_leave_rate = 0.15,       # yearly chance one child leaves cover, from age 48

    # Wearable programme: members who join share daily steps and sleep
    # efficiency. Joining is more likely for younger, leaner non-smokers.
    wellness_base = 0.35,
    steps_mean = 7000,
    steps_cv = 0.45,
    sleep_mean = 0.84,
    sleep_sd = 0.07,
    copula_theta = 2,              # Clayton: steps and sleep are joined in the lower tail
    freq_steps = -0.07,            # per 1,000 steps above steps_mean
    freq_sleep = -1.5,             # per unit of sleep efficiency above sleep_mean

    # Money
    claims_inflation = 0.075,      # yearly growth in cost per claim
    annual_limit = c(250000, 500000, 1000000),  # rand per member per year, by plan

    # Exits
    lapse_base = 0.10,
    lapse_first_year = 0.6,        # extra log odds in the first policy year
    lapse_age = -0.02,             # per year of age above 40
    lapse_plan = c(0.3, 0, -0.3),
    lapse_no_claims = 0.4,         # members with no claims that year lapse more
    death_a = 2e-5,                # Gompertz: yearly death probability a * exp(b * age)
    death_b = 0.095
  )
  p <- set_params(defaults, list(...))
  p$portfolio <- portfolio
  check_panel_params(p)
  p
}

set_params <- function(defaults, changes) {
  if (length(changes) == 0) return(defaults)
  if (is.null(names(changes)) || any(names(changes) == "")) {
    stop("All parameters must be named.", call. = FALSE)
  }
  unknown <- setdiff(names(changes), names(defaults))
  if (length(unknown) > 0) {
    stop("Unknown parameter(s): ", paste(unknown, collapse = ", "), call. = FALSE)
  }
  defaults[names(changes)] <- changes
  defaults
}

check_length <- function(p, name, n) {
  if (length(p[[name]]) != n) {
    stop(sprintf("`%s` must have length %d.", name, n), call. = FALSE)
  }
}

check_portfolio_params <- function(p) {
  for (nm in c("province_share", "freq_province", "sev_province")) check_length(p, nm, 9)
  for (nm in c("freq_plan", "sev_plan")) check_length(p, nm, 3)
  if (any(p$province_share < 0) || sum(p$province_share) <= 0) {
    stop("`province_share` must be non-negative with a positive sum.", call. = FALSE)
  }
  if (p$frailty_var < 0) stop("`frailty_var` must be zero or more.", call. = FALSE)
  if (p$large_alpha <= 2) {
    stop("`large_alpha` must exceed 2 so claim variance is finite.", call. = FALSE)
  }
  for (nm in c("large_prob", "female_share", "full_year_share")) {
    if (p[[nm]] < 0 || p[[nm]] > 1) stop(sprintf("`%s` must be in [0, 1].", nm), call. = FALSE)
  }
  invisible(p)
}

check_panel_params <- function(p) {
  check_portfolio_params(p$portfolio)
  check_length(p, "annual_limit", 3)
  check_length(p, "lapse_plan", 3)
  if (p$copula_theta <= 0) stop("`copula_theta` must be positive.", call. = FALSE)
  for (nm in c("frailty_persistent_share", "quit_rate", "lapse_base", "wellness_base")) {
    if (p[[nm]] < 0 || p[[nm]] > 1) stop(sprintf("`%s` must be in [0, 1].", nm), call. = FALSE)
  }
  invisible(p)
}

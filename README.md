# healthsim

Synthetic South African health insurance data with a known data-generating
process, for learning and testing pricing models. Every parameter can be
changed, and the true expected frequency and severity are returned, so you
can check how close a fitted model gets to the truth.

The two Kaggle datasets below are built with this package
(`data-raw/kaggle.R`):

* [SA Health Insurance Pricing Data (Synthetic)](https://www.kaggle.com/datasets/sbusisomdingiayesbu/synthetic-health-insurance-data):
  `simulate_portfolio(1e5, seed = 123, claims = TRUE)`
* [SA Health Insurance Panel 2021-2025 (Synthetic)](https://www.kaggle.com/datasets/sbusisomdingiayesbu/health-insurance-panel-claims-and-telematics):
  `simulate_panel(2e5, years = 5, seed = 2026)`

## Install

```r
# install.packages("remotes")
remotes::install_github("Sbusiso-Mdingi/healthsim")
```

## Use

```r
library(healthsim)

# One year of 100,000 policies
pf <- simulate_portfolio(1e5, seed = 123)

# The same portfolio design with a heavier smoking loading and no frailty
pf2 <- simulate_portfolio(1e5, seed = 1,
                          params = portfolio_params(freq_smoker = 0.5, frailty_var = 0))

# Policies plus one row per claim
x <- simulate_portfolio(1e4, seed = 1, claims = TRUE)
head(x$claims)

# Five years of an open cohort starting with 50,000 members
pn <- simulate_panel(5e4, years = 5, seed = 1)

# Faster claims inflation and higher lapses
pp <- panel_params(claims_inflation = 0.10, lapse_base = 0.15)
pn2 <- simulate_panel(5e4, years = 5, seed = 1, params = pp)

# Compare a fitted model with the truth
fit <- glm(ClaimCount ~ Age + Smoker + Plan + offset(log(Exposure)),
           family = poisson, data = set_levels(pf))
truth <- true_frequency(pf)
```

`portfolio_params()` and `panel_params()` list every parameter with its
default; see `?portfolio_params` and `?panel_params`.

## The portfolio

Principal members aged 18 to 85 in the nine provinces, on three benefit
tiers (Essential, Standard, Comprehensive). Claim counts are Poisson given a
gamma frailty (negative binomial overall), with log-linear effects of age,
maternity, BMI, smoking, children, province and plan. Each claim's cost is a
lognormal-Pareto mixture scaled by a log-linear severity mean. Amounts are
in rand.

## The panel

An open cohort followed year by year. Members age, gain weight, quit
smoking and have children; new, younger members join; members lapse at the
end of a year (more in the first year and among non-claimants) or die during
it. Frailty has a lasting part, so claims history predicts future claims.
Daily steps and sleep efficiency, joined by a Clayton copula, lower claim
frequency, but they are only recorded for wellness programme members, who
are more active than the rest. Cost per claim inflates each year while
annual limits stay fixed, so more members reach the limit over time.

All data is artificial. The parameters are illustrative assumptions, not
estimates from any insurer's data.

## Citation

```r
citation("healthsim")
```

# Builds the files of the two Kaggle datasets:
#   sbusisomdingiayesbu/synthetic-health-insurance-data
#   sbusisomdingiayesbu/health-insurance-panel-claims-and-telematics
#
# Run from the package root:  Rscript data-raw/kaggle.R <output dir>

devtools::load_all(quiet = TRUE)

args <- commandArgs(trailingOnly = TRUE)
out <- if (length(args) > 0) args[1] else "kaggle"
dir.create(file.path(out, "portfolio"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(out, "panel"), recursive = TRUE, showWarnings = FALSE)

# Cross-sectional portfolio: the same draw as the pricing model repository
# (n = 100,000, seed 123), with policy IDs and a claim-level table added.
pf <- simulate_portfolio(n = 1e5, seed = 123, claims = TRUE)
write.csv(pf$policies, file.path(out, "portfolio", "synthetic_health_insurance_data.csv"),
          row.names = FALSE)
write.csv(pf$claims, file.path(out, "portfolio", "synthetic_health_insurance_claims.csv"),
          row.names = FALSE)

# Panel: 200,000 members in force at the start of 2021, followed to 2025.
pn <- simulate_panel(n = 2e5, years = 5, seed = 2026)
write.csv(pn, file.path(out, "panel", "synthetic_health_insurance_panel.csv"),
          row.names = FALSE, na = "")

cat("Portfolio:", nrow(pf$policies), "policies,", nrow(pf$claims), "claims\n")
cat("Panel:", nrow(pn), "member-years,", length(unique(pn$Policy_ID)), "members\n")

# 07_alt_estimators.R
# Alternative estimators: Synthetic Difference-in-Differences (Arkhangelsky
# et al. 2021) and Augmented SCM (Ben-Michael, Feller & Rothstein 2021).
#
#   - SDID is the headline COVID-robust estimate: unlike classic SCM it admits
#     a common time shock via time weights, so a nationwide pandemic effect is
#     differenced out rather than loaded onto the treatment.
#   - Augmented SCM ridge-corrects any residual pre-treatment imbalance and
#     returns a jackknife confidence interval. That CI is the paper's implicit
#     POWER statement: it names the effect sizes the design can rule out, which
#     is what gives the null result content.
#
# Requires GitHub packages: synthdid, augsynth (see README). Treatment is
# Oregon (fips 41) in 2020-2023; outcome is the state-average graduation rate.

SYNTH_LIB_ONLY <- TRUE
source("R/03_synth.R")
library(synthdid)
library(augsynth)

# synthdid's placebo variance uses randomized placebo assignments, so its SE
# is not deterministic; fix a seed for reproducible confidence intervals.
set.seed(2024)

fmt <- function(x, d = 3) formatC(x, format = "f", digits = d)

# Balanced state-year panel with a block-treatment indicator for a subsample.
prep_panel <- function(df, weight = NULL) {
  p <- make_panel(df, weight = weight)
  p <- p[stats::complete.cases(p[, c("fips", "year", "grrttot")]), ]
  # keep a balanced panel over all YEARS
  keep <- p %>% group_by(fips) %>%
    summarise(ok = all(YEARS %in% year) & sum(!is.na(grrttot)) == length(YEARS)) %>%
    filter(ok) %>% pull(fips)
  p <- p[p$fips %in% keep & p$year %in% YEARS, ]
  p$trt <- as.integer(p$fips == TREATED & p$year >= 2020)
  p[order(p$fips, p$year), c("fips", "year", "grrttot", "trt")]
}

sdid_one <- function(df, weight = NULL) {
  p <- prep_panel(df, weight = weight)
  setup <- panel.matrices(p, unit = "fips", time = "year",
                          outcome = "grrttot", treatment = "trt")
  est <- synthdid_estimate(setup$Y, setup$N0, setup$T0)
  se  <- sqrt(vcov(est, method = "placebo"))
  c(att = as.numeric(est), se = se,
    lo = as.numeric(est) - 1.96 * se, hi = as.numeric(est) + 1.96 * se)
}

ascm_one <- function(df) {
  p <- prep_panel(df)
  a <- augsynth(grrttot ~ trt, unit = fips, time = year, data = p,
                progfunc = "ridge", scm = TRUE)
  # jackknife+ gives an (asymmetric) CI; conformal gives only a p-value.
  att <- summary(a, inf_type = "jackknife+")$average_att
  c(att = att$Estimate, lo = att$lower_bound, hi = att$upper_bound)
}

samples <- list(
  pooled  = master,
  public  = filter(master, public == 1),
  private = filter(master, public == 0),
  urban   = filter(master, locale %in% c(11, 12, 13))
)
spec_labels <- c(pooled = "All institutions", public = "Public institutions",
                 private = "Private institutions", urban = "Urban institutions")

sdid <- t(sapply(samples, sdid_one))
ascm <- t(sapply(samples, function(d) tryCatch(ascm_one(d),
                 error = function(e) c(att = NA, lo = NA, hi = NA))))

# Consolidated table: average post-period ATT with 95% CI, both estimators.
tbl <- data.frame(
  sample = spec_labels[rownames(sdid)],
  sdid_att = sdid[, "att"], sdid_lo = sdid[, "lo"], sdid_hi = sdid[, "hi"],
  ascm_att = ascm[, "att"], ascm_lo = ascm[, "lo"], ascm_hi = ascm[, "hi"]
)
writeLines(c(
  "\\begin{tabular}{lcc}", "\\hline",
  " & Synthetic DiD & Augmented SCM \\\\",
  " & ATT [95\\% CI] & ATT [95\\% CI] \\\\", "\\hline",
  sprintf("%s & %s [%s, %s] & %s [%s, %s] \\\\", tbl$sample,
          fmt(tbl$sdid_att, 2), fmt(tbl$sdid_lo, 2), fmt(tbl$sdid_hi, 2),
          fmt(tbl$ascm_att, 2), fmt(tbl$ascm_lo, 2), fmt(tbl$ascm_hi, 2)),
  "\\hline", "\\end{tabular}"), "output/tables/table_alt_estimators.tex")

saveRDS(list(sdid = sdid, ascm = ascm, table = tbl), "output/alt_estimator_results.rds")

cat("=== Synthetic DiD (avg post-period ATT, placebo SE) ===\n")
print(round(sdid, 3))
cat("\n=== Augmented SCM (ridge, jackknife CI) ===\n")
print(round(ascm, 3))

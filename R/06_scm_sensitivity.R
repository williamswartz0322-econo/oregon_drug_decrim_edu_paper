# 06_scm_sensitivity.R
# Within-SCM inference and specification robustness.
#
#   (a) MSPE-ratio rank test (Abadie permutation p-value) per subsample -- a
#       single p-value complementing the per-period placebo p-values. This is
#       the test the original do-file sketched but left commented out.
#   (b) Predictor-specification sensitivity: all-lags (main) vs few-lags +
#       covariates vs covariates-only. Addresses Kaul et al. (2018): with all
#       pre-treatment outcomes as predictors the covariates carry ~no weight.
#   (c) Enrollment-weighted state collapse (vs the unweighted institution mean).
#   (d) V-selection sensitivity: nested optimization (Synth default) vs Stata's
#       regression-heuristic V, pinned via custom.v.
#
# Reuses functions from 03_synth.R in library-only mode.

SYNTH_LIB_ONLY <- TRUE
source("R/03_synth.R")

fmt <- function(x, d = 3) formatC(x, format = "f", digits = d)
results <- readRDS("output/synth_results.rds")

## ---- (a) MSPE-ratio rank test -------------------------------------------
# ratio = post-period MSPE / pre-period MSPE; rank Oregon among all units
# (treated + placebos); p = rank-based share >= Oregon's ratio.
mspe_ratio <- function(gap) {
  pre  <- mean(gap[as.character(PRE)]^2)
  post <- mean(gap[as.character(POST)]^2)
  post / pre
}
rank_test <- function(spec) {
  or_ratio <- mspe_ratio(spec$main$gap)
  pl_ratio <- vapply(spec$placebos, function(p) mspe_ratio(p$gap), numeric(1))
  all_ratio <- c(OR = or_ratio, pl_ratio)
  data.frame(mspe_ratio = or_ratio,
             rank = sum(all_ratio >= or_ratio),
             n = length(all_ratio),
             pval = mean(all_ratio >= or_ratio))
}
rank_tbl <- do.call(rbind, lapply(names(results), function(s)
  cbind(spec = s, rank_test(results[[s]]))))
row.names(rank_tbl) <- NULL

spec_labels <- c(pooled = "All institutions", public = "Public institutions",
                 private = "Private institutions", urban = "Urban institutions")
writeLines(c(
  "\\begin{tabular}{lccc}", "\\hline",
  "Sample & MSPE ratio & Rank & P-value \\\\", "\\hline",
  sprintf("%s & %s & %d/%d & %s \\\\", spec_labels[rank_tbl$spec],
          fmt(rank_tbl$mspe_ratio, 2), rank_tbl$rank, rank_tbl$n,
          fmt(rank_tbl$pval, 3)),
  "\\hline", "\\end{tabular}"), "output/tables/table_rank_test.tex")

## ---- (b) Predictor-specification sensitivity (pooled) --------------------
preds_alllags <- special_preds                                  # main spec
preds_fewlags <- c(                                             # 3 lags + covariates
  lapply(2017:2019, function(y) list("grrttot", y, "mean")),
  lapply(c(2012, 2016), function(y) list("unemprate",   y, "mean")),
  lapply(c(2012, 2016), function(y) list("medinc",      y, "mean")),
  lapply(c(2012, 2016), function(y) list("pct_hs_grad", y, "mean")),
  lapply(c(2012, 2016), function(y) list("enrtotdrvef", y, "mean")))
preds_covonly <- c(                                             # covariates only
  lapply(c(2012, 2016), function(y) list("unemprate",   y, "mean")),
  lapply(c(2012, 2016), function(y) list("medinc",      y, "mean")),
  lapply(c(2012, 2016), function(y) list("pct_hs_grad", y, "mean")),
  lapply(c(2012, 2016), function(y) list("enrtotdrvef", y, "mean")))

pooled_panel <- make_panel(master)
donors <- complete_donors(pooled_panel)
spec_variants <- list(
  `All lags (main)`   = run_one(pooled_panel, TREATED, donors, preds = preds_alllags),
  `3 lags + covars`   = run_one(pooled_panel, TREATED, donors, preds = preds_fewlags),
  `Covariates only`   = run_one(pooled_panel, TREATED, donors, preds = preds_covonly)
)
# enrollment-weighted collapse (main predictor set)
pooled_panel_w <- make_panel(master, weight = "enrtotdrvef")
donors_w <- complete_donors(pooled_panel_w)
spec_variants[["Enrollment-weighted"]] <-
  run_one(pooled_panel_w, TREATED, donors_w, preds = preds_alllags)

## ---- (d) V-selection sensitivity (pooled, main predictor set) ------------
# Stata regression-heuristic V (from V_matrix.rtf): weight on the 8 lags, ~0
# on covariates.
stata_v <- c(.143, .135, .124, .126, .126, .111, .116, .119, rep(0, 8))
spec_variants[["Stata heuristic V"]] <-
  run_one(pooled_panel, TREATED, donors, preds = preds_alllags, custom_v = stata_v)

# Consolidated sensitivity table: 2023 ATT (and 2021/2022) across variants
sens_tbl <- do.call(rbind, lapply(names(spec_variants), function(nm) {
  g <- spec_variants[[nm]]$gap
  data.frame(variant = nm, att2021 = g["2021"], att2022 = g["2022"], att2023 = g["2023"])
}))
row.names(sens_tbl) <- NULL
writeLines(c(
  "\\begin{tabular}{lccc}", "\\hline",
  "Specification & 2021 & 2022 & 2023 \\\\", "\\hline",
  sprintf("%s & %s & %s & %s \\\\", sens_tbl$variant,
          fmt(sens_tbl$att2021, 2), fmt(sens_tbl$att2022, 2), fmt(sens_tbl$att2023, 2)),
  "\\hline", "\\end{tabular}"), "output/tables/table_spec_sensitivity.tex")

saveRDS(list(rank = rank_tbl, sensitivity = sens_tbl), "output/scm_sensitivity_results.rds")

## ---- Console summary ----
cat("=== MSPE-ratio rank test ===\n"); print(rank_tbl, row.names = FALSE, digits = 4)
cat("\n=== Specification & V sensitivity (pooled ATT) ===\n")
print(sens_tbl, row.names = FALSE, digits = 4)

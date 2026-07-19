# 03_synth.R
# Synthetic control analysis of Measure 110 and college graduation rates.
#
# Replicates the Stata `synth` / `synth_runner` analysis in CollegeV2.do:
#   - outcome: state-average total graduation rate (grrttot)
#   - treated unit: Oregon (fips 41), treatment period 2020
#   - predictors: grrttot 2012-2019; unemprate, medinc, pct_hs_grad,
#     enrtotdrvef at 2012 and 2016
#   - inference: in-space placebos; per-period p-value is the share of
#     placebo effects at least as large (in absolute value) as Oregon's,
#     both raw and standardized by pre-treatment RMSPE (Galiani &
#     Quistorff 2017)
# Four specifications: pooled, public, private, urban institutions.

library(dplyr)
library(tidyr)
library(ggplot2)
library(Synth)
library(parallel)

master  <- readRDS("data/master.rds")
TREATED <- 41L          # Oregon
PRE     <- 2012:2019
POST    <- 2020:2023
YEARS   <- 2012:2023

special_preds <- c(
  lapply(PRE, function(y) list("grrttot", y, "mean")),
  lapply(c(2012, 2016), function(y) list("unemprate",   y, "mean")),
  lapply(c(2012, 2016), function(y) list("medinc",      y, "mean")),
  lapply(c(2012, 2016), function(y) list("pct_hs_grad", y, "mean")),
  lapply(c(2012, 2016), function(y) list("enrtotdrvef", y, "mean"))
)

# State-year panel for a subsample (Stata: collapse ..., by(year fips stabbr))
make_panel <- function(df) {
  df %>%
    group_by(year, fips, stabbr) %>%
    summarise(across(c(grrttot, unemprate, medinc, pct_hs_grad, enrtotdrvef),
                     ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
    mutate(across(where(is.numeric), ~ ifelse(is.nan(.x), NA, .x))) %>%
    as.data.frame()
}

# One synth run: returns the treated-minus-synthetic gap over YEARS
run_one <- function(panel, treated_id, control_ids) {
  dp <- dataprep(
    foo = panel,
    special.predictors   = special_preds,
    time.predictors.prior = PRE,
    dependent            = "grrttot",
    unit.variable        = "fips",
    unit.names.variable  = "stabbr",
    time.variable        = "year",
    treatment.identifier = treated_id,
    controls.identifier  = control_ids,
    time.optimize.ssr    = PRE,
    time.plot            = YEARS
  )
  fit <- synth(dp)
  gap <- as.numeric(dp$Y1plot - dp$Y0plot %*% fit$solution.w)
  list(gap = setNames(gap, YEARS),
       y1 = setNames(as.numeric(dp$Y1plot), YEARS),
       y0 = setNames(as.numeric(dp$Y0plot %*% fit$solution.w), YEARS),
       w  = setNames(as.numeric(fit$solution.w), rownames(fit$solution.w)),
       v  = setNames(as.numeric(fit$solution.v),
                     rownames(dp$X0) %||% names(fit$solution.v)))
}
`%||%` <- function(a, b) if (is.null(a)) b else a

pre_rmspe <- function(gap) sqrt(mean(gap[as.character(PRE)]^2))

run_spec <- function(df, spec_name) {
  panel <- make_panel(df)

  # Synth needs complete outcome/predictor data; drop donors with gaps
  needed <- panel %>%
    filter(year %in% YEARS) %>%
    group_by(fips) %>%
    summarise(ok = sum(!is.na(grrttot)) == length(YEARS) &
                   all(!is.na(unemprate[year %in% c(2012, 2016)])) &
                   all(!is.na(medinc[year %in% c(2012, 2016)])) &
                   all(!is.na(pct_hs_grad[year %in% c(2012, 2016)])) &
                   all(!is.na(enrtotdrvef[year %in% c(2012, 2016)])))
  units  <- sort(needed$fips[needed$ok])
  donors <- setdiff(units, TREATED)
  dropped <- setdiff(unique(panel$fips), units)
  if (length(dropped)) message(spec_name, ": dropped units with missing data: ",
                               paste(dropped, collapse = ", "))

  main <- run_one(panel, TREATED, donors)

  # In-space placebos: each donor treated in turn, Oregon excluded
  placebos <- mclapply(donors, function(d) {
    tryCatch(run_one(panel, d, setdiff(donors, d)), error = function(e) NULL)
  }, mc.cores = max(1, detectCores() - 2))
  names(placebos) <- donors
  placebos <- placebos[!vapply(placebos, is.null, logical(1))]

  # Per-period p-values a la synth_runner
  gap_or   <- main$gap
  rmspe_or <- pre_rmspe(gap_or)
  pl_gaps  <- vapply(placebos, `[[`, numeric(length(YEARS)), "gap")
  pl_rmspe <- vapply(placebos, function(p) pre_rmspe(p$gap), numeric(1))

  res <- data.frame(
    year      = POST,
    estimate  = gap_or[as.character(POST)],
    pval      = sapply(as.character(POST), function(t)
                  mean(abs(pl_gaps[t, ]) >= abs(gap_or[t]))),
    pval_std  = sapply(as.character(POST), function(t)
                  mean(abs(pl_gaps[t, ] / pl_rmspe) >= abs(gap_or[t] / rmspe_or)))
  )

  list(name = spec_name, panel = panel, main = main, placebos = placebos,
       results = res, n_placebo = length(placebos))
}

specs <- list(
  pooled  = run_spec(master, "pooled"),
  public  = run_spec(filter(master, public == 1), "public"),
  private = run_spec(filter(master, public == 0), "private"),
  urban   = run_spec(filter(master, locale %in% c(11, 12, 13)), "urban")
)
saveRDS(specs, "output/synth_results.rds")

## ---- LaTeX tables (Tables 2-5) ----
fmt <- function(x, d = 3) formatC(x, format = "f", digits = d)
spec_labels <- c(pooled = "All institutions", public = "Public institutions",
                 private = "Private institutions", urban = "Urban institutions")
for (s in names(specs)) {
  r <- specs[[s]]$results
  lines <- c(
    "\\begin{tabular}{lccc}",
    "\\hline",
    " & Estimate & P-value & Standardized P-value \\\\",
    "\\hline",
    sprintf("%d & %s & %s & %s \\\\", r$year, fmt(r$estimate),
            fmt(r$pval, 2), fmt(r$pval_std, 3)),
    "\\hline",
    "\\end{tabular}"
  )
  writeLines(lines, sprintf("output/tables/table_att_%s.tex", s))
}

## ---- Figures ----
theme_set(theme_minimal(base_size = 12))
pooled <- specs$pooled

# Figure 5: Oregon vs synthetic Oregon
path_dat <- data.frame(year = YEARS,
                       Oregon = pooled$main$y1,
                       `Synthetic Oregon` = pooled$main$y0,
                       check.names = FALSE) %>%
  pivot_longer(-year)
fig5 <- ggplot(path_dat, aes(year, value, linetype = name)) +
  geom_line(linewidth = 0.8) +
  geom_vline(xintercept = 2020, linetype = "dotted") +
  scale_linetype_manual(values = c(Oregon = "solid", `Synthetic Oregon` = "dashed")) +
  scale_x_continuous(breaks = seq(2012, 2023, 2)) +
  labs(title = "Oregon vs Synthetic Oregon", x = "Year",
       y = "Graduation Rate", linetype = NULL)
ggsave("output/figures/fig5_synth_path.png", fig5, width = 7, height = 4.5, dpi = 300)

# Figure 6: in-space placebo effects
pl_long <- do.call(rbind, lapply(names(pooled$placebos), function(d)
  data.frame(fips = d, year = YEARS, effect = pooled$placebos[[d]]$gap)))
or_long <- data.frame(fips = "OR", year = YEARS, effect = pooled$main$gap)
fig6 <- ggplot() +
  geom_line(data = pl_long, aes(year, effect, group = fips),
            color = "grey70", linewidth = 0.3) +
  geom_line(data = or_long, aes(year, effect), color = "black", linewidth = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 2020, linetype = "dotted") +
  scale_x_continuous(breaks = seq(2012, 2023, 2)) +
  labs(title = "Placebo Tests of Oregon vs Other States",
       x = "Year", y = "Estimated Effect")
ggsave("output/figures/fig6_placebo.png", fig6, width = 7, height = 4.5, dpi = 300)

# Effect path (gap) plot, pooled
gap_dat <- data.frame(year = YEARS, effect = pooled$main$gap)
fig_eff <- ggplot(gap_dat, aes(year, effect)) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 2020, linetype = "dotted") +
  scale_x_continuous(breaks = seq(2012, 2023, 2)) +
  labs(title = "Estimated Effect of Measure 110 on Graduation Rates",
       x = "Year", y = "Graduation Rate Gap")
ggsave("output/figures/fig_effects_pooled.png", fig_eff, width = 7, height = 4.5, dpi = 300)

## ---- Console summary ----
for (s in names(specs)) {
  cat("\n==", spec_labels[s], "(", specs[[s]]$n_placebo, "placebos ) ==\n")
  print(specs[[s]]$results, row.names = FALSE, digits = 4)
}
cat("\nPooled donor weights (>0.001):\n")
w <- sort(pooled$main$w[pooled$main$w > 0.001], decreasing = TRUE)
print(round(w, 3))

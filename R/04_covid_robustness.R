# 04_covid_robustness.R
# COVID-19 confounding (priority threat).
#
# Measure 110 took effect Feb 2021, squarely inside the pandemic's disruption
# of higher education. The concern is that any post-2020 Oregon-vs-synthetic
# gap reflects state-specific COVID effects, not the policy.
#
# The clean Synth-only diagnostic is an IN-TIME PLACEBO (backdating): assign a
# *fake* treatment year that precedes both the policy and COVID, then look for
# a spurious "effect" once the pandemic hits. Backdating to 2018 and 2019 asks:
# does the method attribute the 2020-2021 common shock to a treatment that
# never happened? If the fake-treatment gaps in 2020-2021 are comparable in
# size to Oregon's real post-2020 gap, the headline gap is pandemic noise.
#
# (Synthetic DiD -- which absorbs a common time shock directly -- is the
# complementary headline estimator and lives in 07_alt_estimators.R.)

SYNTH_LIB_ONLY <- TRUE
source("R/03_synth.R")

fmt <- function(x, d = 3) formatC(x, format = "f", digits = d)

# Predictor set for a backdated placebo: outcome lags strictly before the fake
# treatment year, plus the same covariates at the two earliest available years.
backdate_preds <- function(t_treat) {
  lags <- 2012:(t_treat - 1)
  cov_years <- sort(lags)[1:2]
  c(lapply(lags, function(y) list("grrttot", y, "mean")),
    lapply(cov_years, function(y) list("unemprate",   y, "mean")),
    lapply(cov_years, function(y) list("medinc",      y, "mean")),
    lapply(cov_years, function(y) list("pct_hs_grad", y, "mean")),
    lapply(cov_years, function(y) list("enrtotdrvef", y, "mean")))
}

pooled_panel <- make_panel(master)
donors <- setdiff(sort(unique(pooled_panel$fips)), TREATED)

# Real treatment (2020) plus two pre-policy, pre-COVID fake treatments.
runs <- list(
  `Actual (2020)` = run_one(pooled_panel, TREATED, donors,
                            preds = special_preds, t_treat = 2020),
  `Backdated 2019` = run_one(pooled_panel, TREATED, donors,
                             preds = backdate_preds(2019), t_treat = 2019),
  `Backdated 2018` = run_one(pooled_panel, TREATED, donors,
                             preds = backdate_preds(2018), t_treat = 2018)
)

# Gap paths for the figure
gap_long <- do.call(rbind, lapply(names(runs), function(nm)
  data.frame(spec = nm, year = YEARS, gap = runs[[nm]]$gap)))
gap_long$spec <- factor(gap_long$spec, levels = names(runs))

covid_fig <- ggplot(gap_long, aes(year, gap, color = spec, linetype = spec)) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_line(linewidth = 0.8) +
  annotate("rect", xmin = 2020, xmax = 2021.4, ymin = -Inf, ymax = Inf,
           alpha = 0.08, fill = "red") +
  scale_x_continuous(breaks = seq(2012, 2023, 2)) +
  scale_color_manual(values = c("black", "#1b7837", "#762a83")) +
  labs(title = "In-Time Placebo: Backdated Treatment vs COVID Window",
       subtitle = "Shaded = 2020-2021 pandemic disruption",
       x = "Year", y = "Estimated gap (Oregon - synthetic)",
       color = NULL, linetype = NULL) +
  theme_minimal(base_size = 12)
ggsave("output/figures/fig_intime_placebo.png", covid_fig,
       width = 7, height = 4.5, dpi = 300)

# Table: the 2020 and 2021 gap under each (fake) treatment date
intime_tbl <- do.call(rbind, lapply(names(runs), function(nm) {
  g <- runs[[nm]]$gap
  data.frame(spec = nm, gap2020 = g["2020"], gap2021 = g["2021"])
}))
row.names(intime_tbl) <- NULL
writeLines(c(
  "\\begin{tabular}{lcc}", "\\hline",
  "Treatment year & 2020 gap & 2021 gap \\\\", "\\hline",
  sprintf("%s & %s & %s \\\\", intime_tbl$spec,
          fmt(intime_tbl$gap2020, 2), fmt(intime_tbl$gap2021, 2)),
  "\\hline", "\\end{tabular}"), "output/tables/table_intime_placebo.tex")

saveRDS(list(runs = runs, intime = intime_tbl), "output/covid_results.rds")

cat("=== In-time placebo: 2020 & 2021 gaps by (fake) treatment year ===\n")
print(intime_tbl, row.names = FALSE, digits = 4)
cat("\nInterpretation: sizable 2020-2021 gaps under a pre-COVID fake treatment\n",
    "indicate the post-2020 gap reflects the pandemic, not Measure 110.\n")

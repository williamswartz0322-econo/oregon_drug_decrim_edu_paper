# 05_donor_pool.R
# Donor-pool robustness (priority threat: control-group contamination).
#
#   (a) Drop Oregon's border states (CA, ID, NV, WA) from the donor pool.
#       Motivated by cross-border SUTVA and by the fact that Nevada -- a
#       border state -- receives the largest weight in the main pooled fit,
#       contradicting the paper's original claim.
#   (b) Drop states that changed their own drug / recreational-marijuana law
#       in 2020-2023, guarding against a broader "harm-reduction wave"
#       confound in the donor pool.
#   (c) Leave-one-out: drop each positive-weight donor in turn and track the
#       2023 ATT, showing no single donor drives the result.
#
# Reuses run_spec()/run_one()/make_panel() from 03_synth.R (library-only mode).
# Requires output/synth_results.rds from a prior 03 run for the baseline weights.

SYNTH_LIB_ONLY <- TRUE
source("R/03_synth.R")

BORDER <- c(6, 16, 32, 53)                              # CA, ID, NV, WA
POLICY <- c(4, 9, 24, 29, 30, 34, 35, 36, 44, 51)       # AZ CT MD MO MT NJ NM NY RI VA

fmt <- function(x, d = 3) formatC(x, format = "f", digits = d)

att_table <- function(res, note_cols = TRUE) {
  c("\\begin{tabular}{lccc}", "\\hline",
    " & Estimate & P-value & Standardized P-value \\\\", "\\hline",
    sprintf("%d & %s & %s & %s \\\\", res$year, fmt(res$estimate),
            fmt(res$pval, 2), fmt(res$pval_std, 3)),
    "\\hline", "\\end{tabular}")
}

## ---- (a) Drop border states, all four subsamples ----
message("== Donor pool: dropping border states (CA, ID, NV, WA) ==")
border_specs <- list(
  pooled  = run_spec(master, "pooled_noborder",  exclude_donors = BORDER),
  public  = run_spec(filter(master, public == 1), "public_noborder",  exclude_donors = BORDER),
  private = run_spec(filter(master, public == 0), "private_noborder", exclude_donors = BORDER),
  urban   = run_spec(filter(master, locale %in% c(11, 12, 13)), "urban_noborder", exclude_donors = BORDER)
)
for (s in names(border_specs))
  writeLines(att_table(border_specs[[s]]$results),
             sprintf("output/tables/table_att_%s_noborder.tex", s))

## ---- (b) Drop concurrent drug-policy states (pooled) ----
message("== Donor pool: dropping concurrent-policy states ==")
nopolicy <- run_spec(master, "pooled_nopolicy", exclude_donors = POLICY)
writeLines(att_table(nopolicy$results), "output/tables/table_att_pooled_nopolicy.tex")

## ---- (c) Leave-one-out over positive-weight donors (pooled) ----
message("== Donor pool: leave-one-out on positive-weight donors ==")
base_pooled <- readRDS("output/synth_results.rds")$pooled
posw <- names(sort(base_pooled$main$w[base_pooled$main$w > 0.01], decreasing = TRUE))
loo <- lapply(as.integer(posw), function(d)
  run_spec(master, paste0("loo_", d), exclude_donors = d))
names(loo) <- posw

loo_2023 <- data.frame(
  dropped = sapply(posw, function(f) unique(master$stabbr[master$fips == as.integer(f)])),
  att2023 = sapply(loo, function(x) x$results$estimate[x$results$year == 2023]),
  pval2023 = sapply(loo, function(x) x$results$pval[x$results$year == 2023])
)
row.names(loo_2023) <- NULL

# Leave-one-out figure: 2023 ATT envelope vs the full-sample baseline
base2023 <- base_pooled$results$estimate[base_pooled$results$year == 2023]
loo_fig <- ggplot(loo_2023, aes(reorder(dropped, att2023), att2023)) +
  geom_hline(yintercept = base2023, linetype = "dashed", color = "grey40") +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_point(size = 2) +
  coord_flip() +
  labs(title = "Leave-One-Out: 2023 ATT with each positive-weight donor removed",
       subtitle = paste0("Dashed line = full-sample 2023 ATT (", fmt(base2023, 2), ")"),
       x = "Donor removed", y = "2023 ATT (graduation-rate gap)") +
  theme_minimal(base_size = 12)
ggsave("output/figures/fig_loo_pooled.png", loo_fig, width = 7, height = 4.5, dpi = 300)

saveRDS(list(border = border_specs, nopolicy = nopolicy, loo = loo,
             loo_2023 = loo_2023), "output/donor_pool_results.rds")

## ---- Console summary ----
cat("\n=== Border states dropped (was NV top weight) ===\n")
for (s in names(border_specs)) {
  cat("\n--", s, "--\n"); print(border_specs[[s]]$results, row.names = FALSE, digits = 4)
  w <- border_specs[[s]]$main$w
  cat("top weights:", paste(names(sort(w, decreasing = TRUE))[1:4],
      round(sort(w, decreasing = TRUE)[1:4], 3), collapse = "  "), "\n")
}
cat("\n=== Concurrent-policy states dropped (pooled) ===\n")
print(nopolicy$results, row.names = FALSE, digits = 4)
cat("\n=== Leave-one-out, 2023 ATT ===\n")
print(loo_2023, row.names = FALSE, digits = 4)

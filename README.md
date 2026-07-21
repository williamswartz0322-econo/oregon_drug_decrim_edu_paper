# Drug Decriminalization and College Graduation: Evidence from Oregon's Measure 110

Replication package for "Drug Decriminalization and College Graduation:
Evidence from Oregon's Measure 110" (William Swartz, ECON 740, San Diego
State University, December 2024).

The paper estimates the effect of Oregon's Measure 110 (all-drug
decriminalization, effective February 2021) on college graduation rates
using the synthetic control method (Abadie, Diamond & Hainmueller 2010),
with in-space placebo inference in the style of Stata's `synth_runner`
(Galiani & Quistorff 2017).

The analysis was originally written in Stata (`CollegeV2.do`, kept for
reference) and has been ported to R. The R pipeline reproduces the Stata
master dataset cell-for-cell; synthetic control estimates differ slightly
from the published Stata numbers because R's `Synth` package uses fully
nested optimization for the predictor-weight matrix V, while Stata's
`synth` default uses a regression-based V.

## Data

- `data-raw/Data_12-19-2024---668.csv` — IPEDS custom data file
  (institution-level graduation rates, enrollment, retention, and
  institutional characteristics, 2012–2023).
- `data-raw/demographic_census_data.dta` — ACS 1-year state-level
  estimates (median income, unemployment rate, educational attainment),
  2012–2023; 2020 is missing because the ACS 1-year estimates were not
  released for 2020.

## Pipeline

```sh
Rscript R/01_build_data.R        # build institution-year master panel -> data/master.rds
Rscript R/02_descriptives.R      # Table 1, Figures 1-4
Rscript R/03_synth.R             # synthetic control + placebo inference, Tables 2-5, Figures 5-6
Rscript R/04_covid_robustness.R  # in-time placebo (backdating) COVID diagnostic
Rscript R/05_donor_pool.R        # border/policy donor exclusions, leave-one-out
Rscript R/06_scm_sensitivity.R   # MSPE-ratio rank test, predictor/V/weighting sensitivity
Rscript R/07_alt_estimators.R    # Synthetic DiD + Augmented SCM (needs synthdid, augsynth)
cd paper && latexmk -pdf paper.tex   # compile the paper
```

Scripts `04`-`07` reuse the functions in `03_synth.R` (sourced in library-only
mode) and require `03` to have been run once for the baseline
`output/synth_results.rds`.

R packages required: `dplyr`, `tidyr`, `readr`, `stringr`, `haven`,
`ggplot2`, `Synth`. The alternative estimators in `07` additionally need two
GitHub-only packages plus `glmnet`:

```r
install.packages(c("glmnet", "LowRankQP"))
remotes::install_github("synth-inference/synthdid")
remotes::install_github("ebenmichael/augsynth")
```

## Robustness

`R/04`-`R/07` implement the robustness analysis (see the paper's Robustness
section): a COVID-19 in-time placebo, donor-pool restrictions (dropping
Oregon's border states and states with concurrent drug-policy changes) plus a
leave-one-out check, an MSPE-ratio permutation test, predictor/`V`/weighting
sensitivity, and two alternative estimators (Synthetic DiD and Augmented SCM
with confidence intervals). The null result holds throughout.

## Layout

- `R/` — analysis scripts (run in order)
- `data-raw/` — raw input data (checked in)
- `data/` — built datasets (generated)
- `output/figures/`, `output/tables/` — generated figures and LaTeX tables
- `paper/` — LaTeX source of the paper
- `CollegeV2.do` — original Stata code (reference only)

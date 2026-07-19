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
Rscript R/01_build_data.R      # build institution-year master panel -> data/master.rds
Rscript R/02_descriptives.R    # Table 1, Figures 1-4
Rscript R/03_synth.R           # synthetic control + placebo inference, Tables 2-5, Figures 5-6
cd paper && latexmk -pdf paper.tex   # compile the paper
```

R packages required: `dplyr`, `tidyr`, `readr`, `stringr`, `haven`,
`ggplot2`, `Synth`.

## Layout

- `R/` — analysis scripts (run in order)
- `data-raw/` — raw input data (checked in)
- `data/` — built datasets (generated)
- `output/figures/`, `output/tables/` — generated figures and LaTeX tables
- `paper/` — LaTeX source of the paper
- `CollegeV2.do` — original Stata code (reference only)

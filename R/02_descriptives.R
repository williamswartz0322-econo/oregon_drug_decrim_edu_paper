# 02_descriptives.R
# Table 1 (descriptive statistics) and Figures 1-4 (graduation rate trends)

library(dplyr)
library(tidyr)
library(ggplot2)

master <- readRDS("data/master.rds")

## ---- Table 1: descriptive statistics on the state-year panel ----
state_year <- master %>%
  group_by(year, fips) %>%
  summarise(across(c(grrttot, enrtotdrvef, pct_hs_grad, unemprate, medinc),
                   ~ mean(.x, na.rm = TRUE)),
            .groups = "drop") %>%
  mutate(across(everything(), ~ ifelse(is.nan(.x), NA, .x)))

sum_stats <- state_year %>%
  pivot_longer(-c(year, fips)) %>%
  group_by(name) %>%
  summarise(
    Obs  = sum(!is.na(value)),
    Mean = mean(value, na.rm = TRUE),
    `Std. Dev.` = sd(value, na.rm = TRUE),
    Min  = min(value, na.rm = TRUE),
    Max  = max(value, na.rm = TRUE)
  ) %>%
  mutate(name = recode(name,
    grrttot = "Graduation Rate", enrtotdrvef = "Enrollment Total",
    pct_hs_grad = "Grad Degree", unemprate = "Unemp. Rate",
    medinc = "Median Income")) %>%
  arrange(match(name, c("Graduation Rate", "Enrollment Total", "Grad Degree",
                        "Unemp. Rate", "Median Income")))

fmt <- function(x) formatC(x, format = "f", digits = 3, big.mark = "")
tab1 <- c(
  "\\begin{tabular}{lrrrrr}",
  "\\hline",
  "Variable & Obs & Mean & Std. Dev. & Min & Max \\\\",
  "\\hline",
  sprintf("%s & %d & %s & %s & %s & %s \\\\",
          sum_stats$name, sum_stats$Obs, fmt(sum_stats$Mean),
          fmt(sum_stats$`Std. Dev.`), fmt(sum_stats$Min), fmt(sum_stats$Max)),
  "\\hline",
  "\\end{tabular}"
)
writeLines(tab1, "output/tables/table1_descriptives.tex")
print(as.data.frame(sum_stats), digits = 6)

## ---- Figures ----
theme_set(theme_minimal(base_size = 12))

or_vs_us <- function(df) {
  df %>%
    mutate(group = ifelse(stabbr == "OR", "Oregon", "Other States")) %>%
    group_by(year, group) %>%
    summarise(grrttot = mean(grrttot, na.rm = TRUE), .groups = "drop")
}

plot_or_vs_us <- function(df, title) {
  ggplot(or_vs_us(df), aes(year, grrttot, color = group, linetype = group)) +
    geom_line(linewidth = 0.8) +
    geom_vline(xintercept = 2020, linetype = "dotted") +
    scale_color_manual(values = c(Oregon = "blue", `Other States` = "red")) +
    scale_linetype_manual(values = c(Oregon = "solid", `Other States` = "dashed")) +
    scale_x_continuous(breaks = seq(2012, 2023, 2)) +
    labs(title = title, subtitle = "Oregon vs Other States",
         x = "Year", y = "Graduation Rate", color = NULL, linetype = NULL)
}

# Figure 1: country-wide average graduation rate
fig1_dat <- master %>%
  group_by(year) %>%
  summarise(grrttot = mean(grrttot, na.rm = TRUE))
fig1 <- ggplot(fig1_dat, aes(year, grrttot)) +
  geom_line(linewidth = 0.8) +
  geom_vline(xintercept = 2020, linetype = "dotted") +
  scale_x_continuous(breaks = seq(2012, 2023, 2)) +
  labs(title = "Country Wide Average Graduation Rate",
       x = "Year", y = "Graduation Rate")
ggsave("output/figures/fig1_countrywide_grad_rate.png", fig1,
       width = 7, height = 4.5, dpi = 300)

# Figures 2-4: Oregon vs other states (all / public / private)
ggsave("output/figures/fig2_oregon_vs_other.png",
       plot_or_vs_us(master, "Average Graduation Rate"),
       width = 7, height = 4.5, dpi = 300)
ggsave("output/figures/fig3_public.png",
       plot_or_vs_us(filter(master, public == 1),
                     "Average Graduation Rate in Public Universities"),
       width = 7, height = 4.5, dpi = 300)
ggsave("output/figures/fig4_private.png",
       plot_or_vs_us(filter(master, public == 0),
                     "Average Graduation Rate in Private Universities"),
       width = 7, height = 4.5, dpi = 300)

cat("Descriptives written to output/\n")

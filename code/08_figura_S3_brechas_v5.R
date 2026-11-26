# 08_figura_S3_brechas_v5.R — Supplementary Figure S3 (interactive)
#   Absolute participation gaps by component (education vs. country of birth)
#   7 survey waves: EESE 2009, ENSE 2011, EESE 2014, ENSE 2017, EESE 2020,
#                   ESdE 2023, ESdE 2025
#
# Input:  output/tabla_blups_prob_v5.csv
# Output: output/tabla_S3_brechas_v5.csv    (data for the figure)
#         Supplementary_Figure_S3_7olas_v5.html  (copy in project root)
#         suplementos_git2/Supplementary_Figure_S3_gaps_decomposition.html
#
# Gap calculation (delta method corrected, v2):
#   For each stratum i, dev_i = 100 * [expit(β₀ + uᵢ) - expit(β₀)]
#   Gradient wrt uᵢ (logit scale):
#     ∂dev_i/∂uᵢ = 100 * expit(β₀ + uᵢ) * [1 - expit(β₀ + uᵢ)]
#   Var(dev_i) ≈ [∂dev_i/∂uᵢ]² * SE(uᵢ)²
#
#   edu_gap  = (1/n_high) Σ dev_i[high] - (1/n_low) Σ dev_i[low]
#   Var(edu_gap) = (1/n_high²) Σ Var(dev_i[high]) + (1/n_low²) Σ Var(dev_i[low])
#   [Independence across strata assumed; BLUPs from same model share β₀
#    but are conditionally independent given the data — standard MAIHDA assumption]
#
#   Same for orig_gap. Total gap CI uses Var(total) = Var(edu) + Var(orig)
#   [edu and orig components computed on the same strata → they are correlated;
#    correct propagation: Var(total) includes Cov(edu_gap, orig_gap).
#    Here we propagate directly from the stratum-level variances.]
#
# Panels:
#   "CRC (men)"   = soh_2a, sex == "Men"
#   "CRC (women)" = soh_2a, sex == "Women"
#   "Breast"      = mama_2a (all female)
#   "Cervical"    = cito_3a (all female)

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tidyr)
  library(purrr)
})

# ── Paths ──────────────────────────────────────────────────────────────────
DIR_PROJ <- dirname(dirname(normalizePath(sys.frame(0)$ofile %||%
                                           commandArgs(FALSE)[grepl("--file", commandArgs(FALSE))][1] %||%
                                           getwd())))
if (!dir.exists(file.path(DIR_PROJ, "output"))) DIR_PROJ <- getwd()

DIR_OUT <- file.path(DIR_PROJ, "output")
BLUPS_PROB  <- file.path(DIR_OUT, "tabla_blups_prob_v5.csv")
OUT_CSV     <- file.path(DIR_OUT, "tabla_figS3_brechas_v5.csv")
S3_TEMPLATE <- file.path(DIR_PROJ, "suplementos_git2",
                           "Supplementary_Figure_S3_gaps_decomposition.html")
OUT_HTML_LOCAL <- file.path(DIR_PROJ, "Supplementary_Figure_S3_7olas_v5.html")
OUT_HTML_REPO  <- S3_TEMPLATE

message("Reading: ", BLUPS_PROB)
blups <- read_csv(BLUPS_PROB, show_col_types = FALSE)

# ── Delta-method variance for each stratum's dev ────────────────────────────
# dev_i = 100 * [expit(β₀ + uᵢ) - expit(β₀)]
# ∂dev_i/∂uᵢ = 100 * p_i * (1 - p_i)   where p_i = expit(β₀ + uᵢ)
# Var(dev_i) = [100 * p_i * (1 - p_i)]² * se_i²

expit <- function(x) 1 / (1 + exp(-x))

blups <- blups |>
  mutate(
    p_i    = expit(beta0 + blup),          # predicted prob for stratum i
    grad_i = 100 * p_i * (1 - p_i),       # ∂dev/∂u in pp per logit unit
    var_dev = grad_i^2 * se^2              # Var(dev_i) via delta method
  )

# ── Parse strata labels ─────────────────────────────────────────────────────
blups <- blups |>
  mutate(
    sex  = if_else(str_starts(lab, "Men,"), "Men", "Women"),
    edu  = case_when(
      str_detect(lab, "low educ")    ~ "low",
      str_detect(lab, "medium educ") ~ "medium",
      str_detect(lab, "high educ")   ~ "high",
      TRUE ~ NA_character_
    ),
    orig = if_else(str_detect(lab, "foreign"), "foreign", "spain")
  )

# ── Gap computation (delta method correct) ───────────────────────────────────
# For a mean of k deviations: Var(mean) = (1/k²) * Σ Var(dev_i)
# Gap = mean_A - mean_B  →  Var(gap) = Var(mean_A) + Var(mean_B)
# SE(gap) = sqrt(Var(gap));  CI = gap ± 1.96 * SE(gap)

compute_gap <- function(df) {

  # Education gap: high vs low (medium excluded from gap definition)
  edu_data <- df |> filter(edu %in% c("high", "low"))

  edu_stats <- edu_data |>
    group_by(edu) |>
    summarise(
      m_dev   = mean(dev),
      var_mean = sum(var_dev) / n()^2,   # Var(mean) = Σ Var(dev_i) / k²
      .groups = "drop"
    ) |>
    pivot_wider(names_from = edu, values_from = c(m_dev, var_mean))

  edu_gap  <- edu_stats$m_dev_high   - edu_stats$m_dev_low
  edu_se   <- sqrt(edu_stats$var_mean_high + edu_stats$var_mean_low)
  edu_lo   <- edu_gap - 1.96 * edu_se
  edu_hi   <- edu_gap + 1.96 * edu_se

  # Origin gap: spain vs foreign
  orig_stats <- df |>
    group_by(orig) |>
    summarise(
      m_dev    = mean(dev),
      var_mean = sum(var_dev) / n()^2,
      .groups  = "drop"
    ) |>
    pivot_wider(names_from = orig, values_from = c(m_dev, var_mean))

  orig_gap <- orig_stats$m_dev_spain   - orig_stats$m_dev_foreign
  orig_se  <- sqrt(orig_stats$var_mean_spain + orig_stats$var_mean_foreign)
  orig_lo  <- orig_gap - 1.96 * orig_se
  orig_hi  <- orig_gap + 1.96 * orig_se

  # Total gap = edu_gap + orig_gap
  # Cov(edu_gap, orig_gap) ≠ 0 because both are computed on the same strata.
  # edu_gap = Σ wᵢ dev_i (wᵢ = ±1/k), orig_gap = Σ vᵢ dev_i (vᵢ = ±1/k)
  # Cov = Σ wᵢ vᵢ Var(dev_i)   (independence across strata, same model)

  # Build weight vectors aligned to df rows
  n_high    <- sum(df$edu == "high",    na.rm = TRUE)
  n_low     <- sum(df$edu == "low",     na.rm = TRUE)
  n_spain   <- sum(df$orig == "spain",  na.rm = TRUE)
  n_foreign <- sum(df$orig == "foreign",na.rm = TRUE)

  w_edu  <- case_when(
    df$edu == "high" ~ 1  / n_high,
    df$edu == "low"  ~ -1 / n_low,
    TRUE             ~ 0
  )
  v_orig <- case_when(
    df$orig == "spain"   ~  1 / n_spain,
    df$orig == "foreign" ~ -1 / n_foreign,
    TRUE                 ~  0
  )

  cov_eg_og  <- sum(w_edu * v_orig * df$var_dev)
  total_gap  <- edu_gap + orig_gap
  total_var  <- edu_se^2 + orig_se^2 + 2 * cov_eg_og
  total_se   <- sqrt(max(total_var, 0))   # guard against tiny negative rounding
  total_lo   <- total_gap - 1.96 * total_se
  total_hi   <- total_gap + 1.96 * total_se

  tibble(
    edu_gap, edu_lo, edu_hi,
    orig_gap, orig_lo, orig_hi,
    total = total_gap, total_lo, total_hi
  )
}

panels <- tribble(
  ~panel,          ~variable,  ~sex_filter,
  "CRC (men)",     "soh_2a",   "Men",
  "CRC (women)",   "soh_2a",   "Women",
  "Breast",        "mama_2a",  "Women",
  "Cervical",      "cito_3a",  "Women"
)

gaps_all <- panels |>
  pmap_dfr(function(panel, variable, sex_filter) {
    blups |>
      filter(variable == !!variable, sex == !!sex_filter) |>
      group_by(anyo) |>
      group_modify(~ compute_gap(.x)) |>
      ungroup() |>
      mutate(panel = panel)
  }) |>
  select(panel, anyo, edu_gap, edu_lo, edu_hi, orig_gap, orig_lo, orig_hi,
         total, total_lo, total_hi) |>
  mutate(across(where(is.numeric), ~ round(.x, 1)))

message("Gaps computed for ", nrow(gaps_all), " panel x wave combinations")
print(gaps_all, n = 50)

# ── Save CSV ─────────────────────────────────────────────────────────────────
write_csv(gaps_all, OUT_CSV)
message("Saved: ", OUT_CSV)

# ── Build JS DATA string ──────────────────────────────────────────────────────
row_to_js <- function(r) {
  sprintf(
    "    {anyo:%d,edu_gap:%.1f,edu_lo:%.1f,edu_hi:%.1f,orig_gap:%.1f,orig_lo:%.1f,orig_hi:%.1f,total:%.1f,total_lo:%.1f,total_hi:%.1f}",
    as.integer(r$anyo),
    as.numeric(r$edu_gap), as.numeric(r$edu_lo), as.numeric(r$edu_hi),
    as.numeric(r$orig_gap), as.numeric(r$orig_lo), as.numeric(r$orig_hi),
    as.numeric(r$total), as.numeric(r$total_lo), as.numeric(r$total_hi)
  )
}

panel_to_js <- function(panel_name, df) {
  rows <- df |> filter(panel == panel_name)
  row_strs <- pmap_chr(rows, function(...) row_to_js(list(...)))
  sprintf('  "%s": [\n%s\n  ]', panel_name, paste(row_strs, collapse = ",\n"))
}

panel_names <- c("CRC (men)", "CRC (women)", "Breast", "Cervical")
data_js <- paste0(
  "const DATA = {\n",
  paste(sapply(panel_names, panel_to_js, df = gaps_all), collapse = ",\n"),
  "\n};"
)

waves_js <- 'const WAVES = [2009,2011,2014,2017,2020,2023,2025];'
ticks_js <- 'const WAVE_TICKS = ["EESE<br>2009","ENSE<br>2011","EESE<br>2014","ENSE<br>2017","EESE<br>2020","ESdE<br>2023","ESdE<br>2025"];'

# ── Rebuild HTML from template ────────────────────────────────────────────────
message("Reading template: ", S3_TEMPLATE)
html <- paste(readLines(S3_TEMPLATE, warn = FALSE), collapse = "\n")

script_marker   <- "\n<script>\nconst DATA = {"
script_pos      <- regexpr(script_marker, html, fixed = TRUE)[[1]]
stopifnot("Custom script section not found in template" = script_pos > 0)

ticks_pos    <- regexpr("\nconst WAVE_TICKS = \\[", html)[[1]]
ticks_end    <- regexpr("\n", substr(html, ticks_pos + 1, nchar(html)))[[1]] + ticks_pos

rest_of_script <- substr(html, ticks_end, nchar(html))

new_script <- paste0(
  "\n<script>\n",
  data_js, "\n\n",
  waves_js, "\n",
  ticks_js,
  rest_of_script
)

new_html <- paste0(substr(html, 1, script_pos - 1), new_script)

new_html <- gsub(
  "Supplementary Figure S3 — Absolute participation gaps by component</title>",
  "Supplementary Figure S3 — Absolute participation gaps by component (Spain, 2009–2025)</title>",
  new_html, fixed = TRUE)
new_html <- gsub(
  "EESE 2009, ENSE 2011, EESE 2014, ENSE 2017, EESE 2020, ESdE 2023\\.",
  "EESE 2009, ENSE 2011, EESE 2014, ENSE 2017, EESE 2020, ESdE 2023, ESdE 2025.",
  new_html)
new_html <- gsub("Spain, 2009–2023", "Spain, 2009–2025", new_html, fixed = TRUE)

writeLines(new_html, OUT_HTML_LOCAL)
message("Saved local copy: ", OUT_HTML_LOCAL)
writeLines(new_html, OUT_HTML_REPO)
message("Updated repo:     ", OUT_HTML_REPO)

message("\n✓ Supplementary Figure S3 updated — delta method corrected (v2, 2026-11-26)")

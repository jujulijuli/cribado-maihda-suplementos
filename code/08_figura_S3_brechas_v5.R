# 08_figura_S3_brechas_v5.R — Supplementary Figure S3 (interactive)
#   Absolute participation gaps by component (education vs. country of birth)
#   7 survey waves: EESE 2009, ENSE 2011, EESE 2014, ENSE 2017, EESE 2020,
#                   ESdE 2023, ESdE 2025
#
# Input:  output/tabla_blups_prob_v5.csv
# Output: output/tabla_S3_brechas_v5.csv    (data for the figure)
#         Supplementary_Figure_S3_7olas_v5.html  (copy in project root)
#         suplementos_git2/Supplementary_Figure_S3_brechas_componentes.html
#
# Gap calculation:
#   edu_gap  = mean(dev | high edu) - mean(dev | low edu)   [pp, unweighted mean across strata]
#   orig_gap = mean(dev | Spain-born) - mean(dev | foreign-born)
#   total    = edu_gap + orig_gap
#   CIs propagated as: lo = lo_A - hi_B, hi = hi_A - lo_B
#   where dev, lo, hi are the probability-scale deviations from tabla_blups_prob_v5.csv:
#     dev = 100 * (expit(beta0 + blup) - expit(beta0))
#     lo  = 100 * (expit(beta0 + blup - 1.96*se) - expit(beta0))
#     hi  = 100 * (expit(beta0 + blup + 1.96*se) - expit(beta0))
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
# Assumes script is in <project>/R/, data in <project>/output/
DIR_PROJ <- dirname(dirname(normalizePath(sys.frame(0)$ofile %||%
                                           commandArgs(FALSE)[grepl("--file", commandArgs(FALSE))][1] %||%
                                           getwd())))
# Fallback for running interactively in RStudio from project root
if (!dir.exists(file.path(DIR_PROJ, "output"))) DIR_PROJ <- getwd()

DIR_OUT <- file.path(DIR_PROJ, "output")
BLUPS_PROB  <- file.path(DIR_OUT, "tabla_blups_prob_v5.csv")
OUT_CSV     <- file.path(DIR_OUT, "tabla_S3_brechas_v5.csv")
# S3 HTML template: start from the existing S3 (which embeds Plotly library)
S3_TEMPLATE <- file.path(DIR_PROJ, "suplementos_git2",
                           "Supplementary_Figure_S3_brechas_componentes.html")
OUT_HTML_LOCAL <- file.path(DIR_PROJ, "Supplementary_Figure_S3_7olas_v5.html")
OUT_HTML_REPO  <- S3_TEMPLATE  # update repo copy in-place

message("Reading: ", BLUPS_PROB)
blups <- read_csv(BLUPS_PROB, show_col_types = FALSE)

# ── Parse strata labels ─────────────────────────────────────────────────────
# lab: "Women, low educ., Spain-born, 50-54 (n=474)"
#   or "Men, low educ., foreign-born, 50-54 (n=42)"
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

# ── Gap computation ──────────────────────────────────────────────────────────
compute_gap <- function(df) {
  # Education gap: high vs low (medium excluded from gap definition)
  g_edu <- df |>
    filter(edu %in% c("high", "low")) |>
    group_by(edu) |>
    summarise(m_dev = mean(dev), m_lo = mean(lo), m_hi = mean(hi), .groups = "drop") |>
    pivot_wider(names_from = edu, values_from = c(m_dev, m_lo, m_hi)) |>
    transmute(
      edu_gap = m_dev_high - m_dev_low,
      edu_lo  = m_lo_high  - m_hi_low,
      edu_hi  = m_hi_high  - m_lo_low
    )

  # Origin gap: spain vs foreign
  g_orig <- df |>
    group_by(orig) |>
    summarise(m_dev = mean(dev), m_lo = mean(lo), m_hi = mean(hi), .groups = "drop") |>
    pivot_wider(names_from = orig, values_from = c(m_dev, m_lo, m_hi)) |>
    transmute(
      orig_gap = m_dev_spain   - m_dev_foreign,
      orig_lo  = m_lo_spain    - m_hi_foreign,
      orig_hi  = m_hi_spain    - m_lo_foreign
    )

  bind_cols(g_edu, g_orig) |>
    mutate(
      total    = edu_gap + orig_gap,
      total_lo = edu_lo  + orig_lo,
      total_hi = edu_hi  + orig_hi
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
    r$anyo, r$edu_gap, r$edu_lo, r$edu_hi,
    r$orig_gap, r$orig_lo, r$orig_hi,
    r$total, r$total_lo, r$total_hi
  )
}

panel_to_js <- function(panel_name, df) {
  rows <- df |> filter(panel == panel_name)
  row_strs <- apply(rows, 1, function(r) row_to_js(as.list(type.convert(r, as.is = TRUE))))
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

# Find the start of the custom script section
script_marker   <- "\n<script>\nconst DATA = {"
script_pos      <- regexpr(script_marker, html, fixed = TRUE)[[1]]
stopifnot("Custom script section not found in template" = script_pos > 0)

# Find WAVE_TICKS line end (everything after it stays the same)
ticks_pos    <- regexpr("\nconst WAVE_TICKS = \\[", html)[[1]]
ticks_end    <- regexpr("\n", substr(html, ticks_pos + 1, nchar(html)))[[1]] + ticks_pos

rest_of_script <- substr(html, ticks_end, nchar(html))

# Assemble new script block
new_script <- paste0(
  "\n<script>\n",
  data_js, "\n\n",
  waves_js, "\n",
  ticks_js,
  rest_of_script
)

# Replace from script_pos onwards
new_html <- paste0(substr(html, 1, script_pos - 1), new_script)

# Update text strings
new_html <- gsub(
  "Supplementary Figure S3 — Absolute participation gaps by component</title>",
  "Supplementary Figure S3 — Absolute participation gaps by component (Spain, 2009–2025)</title>",
  new_html, fixed = TRUE)
new_html <- gsub(
  "EESE 2009, ENSE 2011, EESE 2014, ENSE 2017, EESE 2020, ESdE 2023\\.",
  "EESE 2009, ENSE 2011, EESE 2014, ENSE 2017, EESE 2020, ESdE 2023, ESdE 2025.",
  new_html)
new_html <- gsub("Spain, 2009–2023", "Spain, 2009–2025", new_html, fixed = TRUE)

# Save both copies
writeLines(new_html, OUT_HTML_LOCAL)
message("Saved local copy: ", OUT_HTML_LOCAL)
writeLines(new_html, OUT_HTML_REPO)
message("Updated repo:     ", OUT_HTML_REPO)

message("\n✓ Supplementary Figure S3 updated to 7 waves (2009-2025)")

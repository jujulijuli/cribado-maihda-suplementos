# 08_figura_S3_brechas_v5.R — Supplementary Figure S3 (interactive)
#   Absolute participation gaps decomposed by component
#   (education gap, country-of-birth gap, total gap)
#   7 survey waves: EESE 2009, ENSE 2011, EESE 2014, ENSE 2017,
#                   EESE 2020, ESdE 2023, ESdE 2025
#
# Method: separate logistic model per wave and programme (replicating
#   Raquel's Stata approach with margins/lincom). For each wave:
#
#   logit(y) ~ educ3 * extranjero + grupo_edad   [same as Stata do file]
#
#   Three reference points (average marginal probabilities):
#     pA = P(y=1 | educ3=3, extranjero=0, age averaged over observed dist.)
#     pB = P(y=1 | educ3=1, extranjero=0, age averaged over observed dist.)
#     pC = P(y=1 | educ3=1, extranjero=1, age averaged over observed dist.)
#
#   Gaps (in percentage points):
#     edu_gap  = 100*(pA - pB)   [Education: high vs low, Spain-born]
#     orig_gap = 100*(pB - pC)   [Origin: Spain-born vs foreign, low educ]
#     total    = 100*(pA - pC)   [Total = edu_gap + orig_gap]
#
#   95% CI via delta method on the linear combination of predicted
#   probabilities (gradient propagation through the logit link),
#   replicating Stata's lincom approach.
#
# Input:   data/cribado_crc.rds, data/cribado_mama.rds, data/cribado_cervix.rds
#          Expected columns: y, educ3, extranjero, grupo_edad, ola, estrato
#          CRC file also needs: mujer (0=men, 1=women)
#          educ3: 1=low, 2=medium, 3=high  (matches Stata coding)
#          extranjero: 0=Spain-born, 1=foreign-born
#
# Output:
#   output/tabla_figS3_brechas_v5.csv
#   suplementos_git2/Supplementary_Figure_S3_gaps_decomposition.html
#   Supplementary_Figure_S3_7olas_v5.html  (local copy in project root)
#
# Correspondence with Stata do file (FIGs3.do):
#   Stata:  logit y i.educ3##i.extranjero i.grupo_edad
#   R:      glm(y ~ factor(educ3)*factor(extranjero) + factor(grupo_edad), ...)
#   Stata:  margins, at(educ3=3 extranjero=0) at(educ3=1 extranjero=0)
#                    at(educ3=1 extranjero=1) post
#   R:      predict_at_ref() evaluated at three fixed covariate combinations,
#           averaged over observed age distribution (average marginal predictions)
#   Stata:  lincom 100*(_b[1._at] - _b[2._at])  → edu_gap  (CI from Stata delta method)
#   Stata:  lincom 100*(_b[2._at] - _b[3._at])  → orig_gap
#   Stata:  lincom 100*(_b[1._at] - _b[3._at])  → total
#   R:      delta method via gradient * vcov * gradient

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tidyr)
  library(purrr)
})

# ── Paths ──────────────────────────────────────────────────────────────────────
this_file <- tryCatch(
  normalizePath(sys.frame(0)$ofile %||%
    commandArgs(FALSE)[grepl("--file", commandArgs(FALSE))][1]),
  error = function(e) NULL
)
DIR_PROJ <- if (!is.null(this_file) && !is.na(this_file) && file.exists(this_file)) {
  dirname(dirname(this_file))
} else {
  getwd()
}
if (!dir.exists(file.path(DIR_PROJ, "output"))) DIR_PROJ <- getwd()

DIR_OUT    <- file.path(DIR_PROJ, "output")
DIR_DATA   <- file.path(DIR_PROJ, "data")
OUT_CSV    <- file.path(DIR_OUT,  "tabla_figS3_brechas_v5.csv")
S3_TEMPLATE <- file.path(DIR_PROJ, "suplementos_git2",
                           "Supplementary_Figure_S3_gaps_decomposition.html")
OUT_HTML_LOCAL <- file.path(DIR_PROJ, "Supplementary_Figure_S3_7olas_v5.html")
OUT_HTML_REPO  <- S3_TEMPLATE

# ── Data loading ───────────────────────────────────────────────────────────────
load_data <- function(nombre) {
  rds <- file.path(DIR_DATA, paste0(nombre, ".rds"))
  csv <- file.path(DIR_DATA, paste0(nombre, ".csv"))
  if (file.exists(rds)) {
    readRDS(rds)
  } else if (file.exists(csv)) {
    read_csv(csv, show_col_types = FALSE)
  } else {
    stop("Data file not found: ", rds, " or ", csv)
  }
}

# ── Core function: average marginal predictions at three reference points ──────
#
# Replicates Stata's:
#   margins, at(educ3=3 extranjero=0)   → pA (high educ, Spain-born)
#            at(educ3=1 extranjero=0)   → pB (low educ, Spain-born)
#            at(educ3=1 extranjero=1)   → pC (low educ, foreign-born)
#   post
#   lincom 100*(_b[1._at] - _b[2._at])  → edu_gap with 95% CI
#   lincom 100*(_b[2._at] - _b[3._at])  → orig_gap with 95% CI
#   lincom 100*(_b[1._at] - _b[3._at])  → total with 95% CI
#
# Method: for each reference point, replace educ3 and extranjero with fixed
# values in ALL observations (keeping observed age distribution), predict
# P(y=1) for each row, and average. This is Stata's "margins, at()" semantics.
# CIs via delta method: gradient of mean(p) w.r.t. β, then g'Vg.

margins_lincom <- function(mod, data_obs) {
  data_obs <- data_obs |> filter(!is.na(estrato))
  if (nrow(data_obs) == 0) return(NULL)

  vcov_mat   <- vcov(mod)
  coef_names <- names(coef(mod))
  form       <- mod$formula

  # Build prediction for a fixed (educ_val, extran_val) over observed age dist.
  pred_ref <- function(educ_val, extran_val) {
    nd            <- data_obs
    nd$educ3      <- educ_val
    nd$extranjero <- extran_val
    mm  <- model.matrix(form, data = nd)
    # Align to fitted coefficients (handles dropped factor levels)
    mm  <- mm[, intersect(coef_names, colnames(mm)), drop = FALSE]
    # Add any missing columns (from dropped levels) as 0
    missing_cols <- setdiff(coef_names, colnames(mm))
    if (length(missing_cols) > 0) {
      extra <- matrix(0, nrow = nrow(mm), ncol = length(missing_cols),
                      dimnames = list(NULL, missing_cols))
      mm <- cbind(mm, extra)[, coef_names, drop = FALSE]
    }
    eta  <- as.numeric(mm %*% coef(mod))
    p    <- 1 / (1 + exp(-eta))
    w    <- p * (1 - p)
    grad <- colMeans(mm * w)   # (1/n) Σ p_k(1-p_k) x_ik
    list(p_mean = mean(p), grad = grad)
  }

  rA <- pred_ref(3, 0)   # high educ, Spain-born   → Stata's _b[1._at]
  rB <- pred_ref(1, 0)   # low educ,  Spain-born   → Stata's _b[2._at]
  rC <- pred_ref(1, 1)   # low educ,  foreign-born → Stata's _b[3._at]

  # Delta-method variances and covariances: Cov(pX,pY) = gX' V gY
  cov_AA <- as.numeric(rA$grad %*% vcov_mat %*% rA$grad)
  cov_BB <- as.numeric(rB$grad %*% vcov_mat %*% rB$grad)
  cov_CC <- as.numeric(rC$grad %*% vcov_mat %*% rC$grad)
  cov_AB <- as.numeric(rA$grad %*% vcov_mat %*% rB$grad)
  cov_BC <- as.numeric(rB$grad %*% vcov_mat %*% rC$grad)
  cov_AC <- as.numeric(rA$grad %*% vcov_mat %*% rC$grad)

  # edu_gap = 100*(pA - pB)
  edu_gap <- 100 * (rA$p_mean - rB$p_mean)
  edu_se  <- 100 * sqrt(max(cov_AA + cov_BB - 2 * cov_AB, 0))

  # orig_gap = 100*(pB - pC)
  orig_gap <- 100 * (rB$p_mean - rC$p_mean)
  orig_se  <- 100 * sqrt(max(cov_BB + cov_CC - 2 * cov_BC, 0))

  # total = 100*(pA - pC)  [direct, not sum — same as Stata's lincom _b[1._at]-_b[3._at]]
  total_gap <- 100 * (rA$p_mean - rC$p_mean)
  total_se  <- 100 * sqrt(max(cov_AA + cov_CC - 2 * cov_AC, 0))

  tibble(
    edu_gap  = round(edu_gap,  1),
    edu_lo   = round(edu_gap  - 1.96 * edu_se,  1),
    edu_hi   = round(edu_gap  + 1.96 * edu_se,  1),
    orig_gap = round(orig_gap, 1),
    orig_lo  = round(orig_gap - 1.96 * orig_se, 1),
    orig_hi  = round(orig_gap + 1.96 * orig_se, 1),
    total    = round(total_gap, 1),
    total_lo = round(total_gap - 1.96 * total_se, 1),
    total_hi = round(total_gap + 1.96 * total_se, 1)
  )
}

# ── Fit one model for a wave/programme ────────────────────────────────────────
compute_wave_gaps <- function(df_wave) {
  df_wave <- df_wave |> filter(!is.na(estrato))
  if (nrow(df_wave) < 50) {
    warning("Too few observations (", nrow(df_wave), "); skipping")
    return(NULL)
  }
  form <- y ~ factor(educ3) * factor(extranjero) + factor(grupo_edad)
  mod  <- tryCatch(
    glm(form, data = df_wave, family = binomial(link = "logit")),
    error = function(e) { warning("Model failed: ", conditionMessage(e)); NULL }
  )
  if (is.null(mod)) return(NULL)
  mod$formula <- form
  margins_lincom(mod, df_wave)
}

# ── Loop over programmes and waves ────────────────────────────────────────────
WAVES <- c(2009, 2011, 2014, 2017, 2020, 2023, 2025)

panels <- list(
  list(panel = "CRC (men)",   dataset = "cribado_crc",    sex_filter = 0),
  list(panel = "CRC (women)", dataset = "cribado_crc",    sex_filter = 1),
  list(panel = "Breast",      dataset = "cribado_mama",   sex_filter = NA),
  list(panel = "Cervical",    dataset = "cribado_cervix", sex_filter = NA)
)

gaps_all <- map_dfr(panels, function(p) {
  message("\n── Panel: ", p$panel, " ──")
  df <- tryCatch(load_data(p$dataset), error = function(e) {
    stop("Cannot load '", p$dataset, "': ", conditionMessage(e))
  })
  if (!is.na(p$sex_filter)) df <- df |> filter(mujer == p$sex_filter)

  map_dfr(WAVES, function(w) {
    df_w <- df |> filter(ola == w)
    if (nrow(df_w) == 0) { message("  ola=", w, ": no data, skipping"); return(NULL) }
    message("  ola=", w, " n=", nrow(df_w))
    res <- compute_wave_gaps(df_w)
    if (!is.null(res)) mutate(res, panel = p$panel, anyo = w)
  })
}) |>
  select(panel, anyo, edu_gap, edu_lo, edu_hi,
         orig_gap, orig_lo, orig_hi, total, total_lo, total_hi)

message("\nGaps computed for ", nrow(gaps_all), " panel × wave combinations")
print(gaps_all, n = 50)

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
data_js <- paste0("const DATA = {\n",
  paste(sapply(panel_names, panel_to_js, df = gaps_all), collapse = ",\n"),
  "\n};")
waves_js <- 'const WAVES = [2009,2011,2014,2017,2020,2023,2025];'
ticks_js <- 'const WAVE_TICKS = ["EESE<br>2009","ENSE<br>2011","EESE<br>2014","ENSE<br>2017","EESE<br>2020","ESdE<br>2023","ESdE<br>2025"];'

# ── Rebuild HTML from template ────────────────────────────────────────────────
message("Reading template: ", S3_TEMPLATE)
html          <- paste(readLines(S3_TEMPLATE, warn = FALSE), collapse = "\n")
script_marker <- "\n<script>\nconst DATA = {"
script_pos    <- regexpr(script_marker, html, fixed = TRUE)[[1]]
stopifnot("Custom script section not found in template" = script_pos > 0)
ticks_pos     <- regexpr("\nconst WAVE_TICKS = \\[", html)[[1]]
ticks_end     <- regexpr("\n", substr(html, ticks_pos + 1, nchar(html)))[[1]] + ticks_pos
rest_of_script <- substr(html, ticks_end, nchar(html))
new_script <- paste0("\n<script>\n", data_js, "\n\n", waves_js, "\n", ticks_js, rest_of_script)
new_html   <- paste0(substr(html, 1, script_pos - 1), new_script)
new_html <- gsub("Supplementary Figure S3 — Absolute participation gaps by component</title>",
  "Supplementary Figure S3 — Absolute participation gaps by component (Spain, 2009–2025)</title>",
  new_html, fixed = TRUE)
new_html <- gsub(
  "EESE 2009, ENSE 2011, EESE 2014, ENSE 2017, EESE 2020, ESdE 2023\\.",
  "EESE 2009, ENSE 2011, EESE 2014, ENSE 2017, EESE 2020, ESdE 2023, ESdE 2025.", new_html)
new_html <- gsub("Spain, 2009–2023", "Spain, 2009–2025", new_html, fixed = TRUE)

writeLines(new_html, OUT_HTML_LOCAL)
message("Saved local copy: ", OUT_HTML_LOCAL)
writeLines(new_html, OUT_HTML_REPO)
message("Updated repo:     ", OUT_HTML_REPO)
message("\n✓ Supplementary Figure S3 updated — margins/lincom method (v5, 2026-11-26)")

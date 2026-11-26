# 08_figura_S3_brechas_v5.R — Supplementary Figure S3 (interactive)
#   Absolute participation gaps decomposed by component
#   (education gap, country-of-birth gap, total gap)
#   7 survey waves: EESE 2009, ENSE 2011, EESE 2014, ENSE 2017,
#                   EESE 2020, ESdE 2023, ESdE 2025
#
# Method: separate logistic model per wave and programme (replicating
#   Raquel's Stata approach with margins/lincom). For each wave:
#
#   logit(y) ~ isced_3 * pais_nac_f + edad_grupo   [equivalent to Stata do file]
#
#   Three reference points (average marginal probabilities):
#     pA = P(y=1 | isced_3="Alto",  pais_nac_f="España",     age averaged over obs. dist.)
#     pB = P(y=1 | isced_3="Bajo",  pais_nac_f="España",     age averaged over obs. dist.)
#     pC = P(y=1 | isced_3="Bajo",  pais_nac_f="Extranjero", age averaged over obs. dist.)
#
#   Gaps (in percentage points):
#     edu_gap  = 100*(pA - pB)   [Education: high vs low, Spain-born]
#     orig_gap = 100*(pB - pC)   [Origin: Spain-born vs foreign, low educ]
#     total    = 100*(pA - pC)   [Total = edu_gap + orig_gap; computed directly]
#
#   95% CI via delta method on the linear combination of predicted
#   probabilities (gradient propagation through the logit link),
#   replicating Stata's lincom approach.
#
# Correspondence with Stata do file (FIGs3.do):
#   Stata:  logit y i.educ3##i.extranjero i.grupo_edad
#           (educ3: 1=low, 3=high; extranjero: 0=Spain, 1=foreign)
#   R:      glm(y ~ isced_3 * pais_nac_f + edad_grupo, ...)
#           (isced_3: factor with "Bajo"/"Medio"/"Alto"; ref = "Alto"
#            pais_nac_f: factor with "España"/"Extranjero"; ref = "España")
#   Stata:  margins, at(educ3=3 extranjero=0)   → pA (high, Spain)
#            at(educ3=1 extranjero=0)            → pB (low, Spain)
#            at(educ3=1 extranjero=1)            → pC (low, foreign)
#   R:      pred_ref("Alto","España"), pred_ref("Bajo","España"), pred_ref("Bajo","Extranjero")
#   Stata:  lincom 100*(_b[1._at] - _b[2._at])  → edu_gap  (95% CI via delta method)
#   R:      100*(pA - pB) with CI from gradient' %*% vcov %*% gradient
#
# Pipeline variables (from 02_preparar_datos_v4.R / _targets.R):
#   isced_3     : "Bajo" / "Medio" / "Alto"       (collapsed ISCED)
#   pais_nac_f  : "España" / "Extranjero"
#   sexo_f      : "Hombre" / "Mujer"
#   edad_grupo  : "50-54" / "55-59" / "60-64" / "65-69"   (SOH, mama)
#   edad_grupo_cito: "25-29" / ... / "60-64"               (citología)
#   soh_2a      : integer 0/1  (CRC screening, within 2 years)
#   mama_2a     : integer 0/1  (mammography, within 2 years)
#   cito_3a     : integer 0/1  (cervical smear, within 3 years)
#   estrato_4   : stratum label (NA = exclude)
#   anyo        : survey year
#
# Input:   _targets/objects/prep_{survey}{year}  (one RDS per wave, all programmes)
#          or output/olas_preparadas_v5.rds       (combined 7-wave dataset)
#
# Output:
#   output/tabla_figS3_brechas_v5.csv
#   suplementos_git2/Supplementary_Figure_S3_gaps_decomposition.html
#   Supplementary_Figure_S3_7olas_v5.html  (local copy in project root)
#
# NOTA: el pipeline _targets.R actual cubre 2009-2023 (6 olas).
#   Si existe output/olas_preparadas_v5.rds (generado por 02_preparar_datos_v5.R)
#   se usa ese fichero, que incluye ESdE 2025.

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

DIR_OUT      <- file.path(DIR_PROJ, "output")
DIR_TARGETS  <- file.path(DIR_PROJ, "_targets", "objects")
OUT_CSV      <- file.path(DIR_OUT,  "tabla_figS3_brechas_v5.csv")
S3_TEMPLATE  <- file.path(DIR_PROJ, "suplementos_git2",
                           "Supplementary_Figure_S3_gaps_decomposition.html")
OUT_HTML_LOCAL <- file.path(DIR_PROJ, "Supplementary_Figure_S3_7olas_v5.html")
OUT_HTML_REPO  <- S3_TEMPLATE

# ── Data loading ───────────────────────────────────────────────────────────────
# Strategy:
#   1. If output/olas_preparadas_v5.rds exists (7 waves incl. 2025) → use it.
#   2. Else try output/olas_preparadas_v4.rds (6 waves 2009-2023).
#   3. Else load individual _targets/objects/prep_* files and bind rows.

load_all_waves <- function() {
  # Priority 1: pre-combined 7-wave file
  v5 <- file.path(DIR_OUT, "olas_preparadas_v5.rds")
  if (file.exists(v5)) {
    message("Loading combined dataset: ", v5)
    return(readRDS(v5))
  }
  # Priority 2: pre-combined 6-wave file
  v4 <- file.path(DIR_OUT, "olas_preparadas_v4.rds")
  if (file.exists(v4)) {
    message("Loading combined dataset: ", v4)
    return(readRDS(v4))
  }
  # Priority 3: individual _targets objects
  target_names <- c(
    "prep_eese2009", "prep_ense2011", "prep_eese2014",
    "prep_ense2017", "prep_eese2020", "prep_esde2023"
  )
  waves <- list()
  for (nm in target_names) {
    path <- file.path(DIR_TARGETS, nm)
    if (file.exists(path)) {
      message("Loading: ", nm)
      waves[[nm]] <- readRDS(path)
    } else {
      message("Not found (skipping): ", path)
    }
  }
  if (length(waves) == 0) stop("No wave data found.")
  bind_rows(waves)
}

OLAS <- load_all_waves()

# Variables needed
# isced_3, pais_nac_f, sexo_f, edad_grupo, edad_grupo_cito
# soh_2a, mama_2a, cito_3a, estrato_4, anyo

# Check key variables exist
required_vars <- c("isced_3", "pais_nac_f", "sexo_f", "edad_grupo", "anyo")
missing <- setdiff(required_vars, names(OLAS))
if (length(missing) > 0) stop("Missing columns: ", paste(missing, collapse = ", "))

message("Waves in dataset: ", paste(sort(unique(OLAS$anyo)), collapse = ", "))
message("Total rows: ", nrow(OLAS))

# ── Core function: average marginal predictions at three reference points ──────
#
# Replicates Stata's:
#   margins, at(educ3=3 extranjero=0)   → pA (high educ, Spain-born)
#            at(educ3=1 extranjero=0)   → pB (low educ,  Spain-born)
#            at(educ3=1 extranjero=1)   → pC (low educ,  foreign-born)
#   post
#   lincom 100*(_b[1._at] - _b[2._at])  → edu_gap with 95% CI
#   lincom 100*(_b[2._at] - _b[3._at])  → orig_gap with 95% CI
#   lincom 100*(_b[1._at] - _b[3._at])  → total with 95% CI
#
# Method: for each reference point, replace isced_3 and pais_nac_f with fixed
# values in ALL observations (keeping observed age distribution), predict
# P(y=1) for each row, and average. This matches Stata's "margins, at()" semantics.
# CIs via delta method: gradient of mean(p) w.r.t. β, then g'Vg.

margins_lincom <- function(mod, data_obs) {
  data_obs <- data_obs |> filter(!is.na(estrato_4))
  if (nrow(data_obs) == 0) return(NULL)

  vcov_mat   <- vcov(mod)
  coef_names <- names(coef(mod))
  form       <- mod$formula

  # Build prediction for a fixed (educ_val, pais_val) over observed age dist.
  # educ_val: "Alto" or "Bajo"  (matches isced_3 factor labels)
  # pais_val: "España" or "Extranjero"  (matches pais_nac_f factor labels)
  pred_ref <- function(educ_val, pais_val) {
    nd            <- data_obs
    nd$isced_3    <- educ_val
    nd$pais_nac_f <- pais_val
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

  rA <- pred_ref("Alto",  "España")     # high educ, Spain-born   → Stata's _b[1._at]
  rB <- pred_ref("Bajo",  "España")     # low educ,  Spain-born   → Stata's _b[2._at]
  rC <- pred_ref("Bajo",  "Extranjero") # low educ,  foreign-born → Stata's _b[3._at]

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

# ── Fit one model for a wave/programme subset ──────────────────────────────────
# df_sub: pre-filtered by programme (soh/mama/cito) and sex
# resp_var: "soh_2a", "mama_2a", or "cito_3a"
# age_var: "edad_grupo" (soh, mama) or "edad_grupo_cito" (cito)

compute_wave_gaps <- function(df_sub, resp_var, age_var = "edad_grupo") {
  df_sub <- df_sub |>
    filter(!is.na(estrato_4),
           !is.na(.data[[resp_var]]),
           !is.na(isced_3),
           !is.na(pais_nac_f),
           !is.na(.data[[age_var]])) |>
    # Only Alto and Bajo for education (exclude Medio from model,
    # matching Stata which uses educ3 as 1,2,3 but only contrasts 1 vs 3)
    # Actually Stata includes all levels in the model; we do too.
    mutate(
      y         = as.integer(.data[[resp_var]]),
      isced_3   = factor(isced_3, levels = c("Bajo", "Medio", "Alto")),
      pais_nac_f = factor(pais_nac_f, levels = c("España", "Extranjero")),
      edad_g    = factor(.data[[age_var]])
    )

  if (nrow(df_sub) < 50) {
    warning("Too few observations (", nrow(df_sub), "); skipping")
    return(NULL)
  }

  # Equivalent to Stata: logit y i.educ3##i.extranjero i.grupo_edad
  form <- y ~ isced_3 * pais_nac_f + edad_g
  mod  <- tryCatch(
    glm(form, data = df_sub, family = binomial(link = "logit")),
    error = function(e) { warning("Model failed: ", conditionMessage(e)); NULL }
  )
  if (is.null(mod)) return(NULL)
  mod$formula <- form
  margins_lincom(mod, df_sub)
}

# ── Loop over programmes and waves ────────────────────────────────────────────
WAVES_AVAILABLE <- sort(unique(OLAS$anyo))
message("Processing waves: ", paste(WAVES_AVAILABLE, collapse = ", "))

panels <- list(
  list(panel     = "CRC (men)",
       resp_var  = "soh_2a",
       age_var   = "edad_grupo",
       sex_filter = "Hombre",
       desc      = "SOH preventiva, hombres 50-69"),
  list(panel     = "CRC (women)",
       resp_var  = "soh_2a",
       age_var   = "edad_grupo",
       sex_filter = "Mujer",
       desc      = "SOH preventiva, mujeres 50-69"),
  list(panel     = "Breast",
       resp_var  = "mama_2a",
       age_var   = "edad_grupo",
       sex_filter = "Mujer",
       desc      = "Mamografía preventiva, mujeres 50-69"),
  list(panel     = "Cervical",
       resp_var  = "cito_3a",
       age_var   = "edad_grupo_cito",
       sex_filter = "Mujer",
       desc      = "Citología preventiva, mujeres 25-65")
)

gaps_all <- map_dfr(panels, function(p) {
  message("\n── Panel: ", p$panel, " (", p$desc, ") ──")

  # Check response variable exists
  if (!p$resp_var %in% names(OLAS)) {
    message("  Variable '", p$resp_var, "' not in dataset; skipping panel")
    return(NULL)
  }

  map_dfr(WAVES_AVAILABLE, function(w) {
    df_w <- OLAS |>
      filter(anyo == w, sexo_f == p$sex_filter)

    if (nrow(df_w) == 0) {
      message("  ola=", w, ": no data for sex='", p$sex_filter, "', skipping")
      return(NULL)
    }
    n_valid <- sum(!is.na(df_w[[p$resp_var]]) & !is.na(df_w$estrato_4), na.rm = TRUE)
    message("  ola=", w, " n_total=", nrow(df_w), " n_valid=", n_valid)

    if (n_valid < 50) {
      message("  → too few valid obs, skipping")
      return(NULL)
    }

    res <- compute_wave_gaps(df_w, resp_var = p$resp_var, age_var = p$age_var)
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
  if (nrow(rows) == 0) return(sprintf('  "%s": []', panel_name))
  row_strs <- pmap_chr(rows, function(...) row_to_js(list(...)))
  sprintf('  "%s": [\n%s\n  ]', panel_name, paste(row_strs, collapse = ",\n"))
}
panel_names <- c("CRC (men)", "CRC (women)", "Breast", "Cervical")
data_js <- paste0("const DATA = {\n",
  paste(sapply(panel_names, panel_to_js, df = gaps_all), collapse = ",\n"),
  "\n};")

waves_in_data <- sort(unique(gaps_all$anyo))
waves_js_vec  <- paste(waves_in_data, collapse = ",")
waves_js      <- paste0("const WAVES = [", waves_js_vec, "];")
tick_labels   <- sapply(waves_in_data, function(y) {
  enc <- dplyr::case_when(
    y %in% c(2011, 2017) ~ paste0("ENSE<br>", y),
    y %in% c(2023, 2025) ~ paste0("ESdE<br>", y),
    TRUE                 ~ paste0("EESE<br>", y)
  )
  paste0('"', enc, '"')
})
ticks_js <- paste0("const WAVE_TICKS = [", paste(tick_labels, collapse = ","), "];")

# ── Rebuild HTML from template ────────────────────────────────────────────────
if (!file.exists(S3_TEMPLATE)) {
  warning("Template not found: ", S3_TEMPLATE, "\nSkipping HTML generation.")
} else {
  message("Reading template: ", S3_TEMPLATE)
  html          <- paste(readLines(S3_TEMPLATE, warn = FALSE), collapse = "\n")
  script_marker <- "\n<script>\nconst DATA = {"
  script_pos    <- regexpr(script_marker, html, fixed = TRUE)[[1]]
  stopifnot("Custom script section not found in template" = script_pos > 0)
  ticks_pos     <- regexpr("\nconst WAVE_TICKS = \\[", html)[[1]]
  ticks_end     <- regexpr("\n", substr(html, ticks_pos + 1, nchar(html)))[[1]] + ticks_pos
  rest_of_script <- substr(html, ticks_end, nchar(html))
  new_script <- paste0("\n<script>\n", data_js, "\n\n", waves_js, "\n", ticks_js,
                        rest_of_script)
  new_html   <- paste0(substr(html, 1, script_pos - 1), new_script)
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
}

message("\n✓ Supplementary Figure S3 updated — margins/lincom method (v5, 2026-11-26)")

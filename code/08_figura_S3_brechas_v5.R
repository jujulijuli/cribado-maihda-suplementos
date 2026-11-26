# 08_figura_S3_brechas_v5.R — Supplementary Figure S3 (interactive)
#
# Absolute participation gaps decomposed by component
#   edu_gap  = 100*(pA − pB)   high educ / Spain-born vs low educ / Spain-born
#   orig_gap = 100*(pB − pC)   Spain-born vs foreign-born (low educ)
#   total    = 100*(pA − pC)   computed directly (= edu_gap + orig_gap)
#
# Method: separate logistic model per wave × programme, replicating
# Raquel's Stata do file (FIGs3.do) with margins / lincom.
#
#   Stata:  logit y i.educ3##i.extranjero i.grupo_edad
#           margins, at(educ3=3 extranjero=0)   → pA
#                    at(educ3=1 extranjero=0)   → pB
#                    at(educ3=1 extranjero=1)   → pC
#           lincom 100*(_b[1._at]-_b[2._at])   → edu_gap  [CI: delta method]
#           lincom 100*(_b[2._at]-_b[3._at])   → orig_gap
#           lincom 100*(_b[1._at]-_b[3._at])   → total
#
#   R:      glm(y ~ isced_3 * pais_nac_f + edad_g, family=binomial)
#           pA = mean(predict at isced_3="Alto",  pais_nac_f="España")
#           pB = mean(predict at isced_3="Bajo",  pais_nac_f="España")
#           pC = mean(predict at isced_3="Bajo",  pais_nac_f="Extranjero")
#           CIs via gradient-based delta method: SE² = g'·V·g
#
# Input:   output/olas_preparadas_v5.rds   (run 02_preparar_datos_v5.R first)
# Output:  output/tabla_figS3_brechas_v5.csv
#          suplementos_git2/Supplementary_Figure_S3_gaps_decomposition.html

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(readr)
})

# ── Paths ──────────────────────────────────────────────────────────────────────
DIR_PROJ <- tryCatch(
  dirname(dirname(normalizePath(sys.frame(0)$ofile))),
  error = function(e) getwd()
)
DIR_OUT     <- file.path(DIR_PROJ, "output")
HTML_TMPL   <- file.path(DIR_PROJ, "suplementos_git2",
                          "Supplementary_Figure_S3_gaps_decomposition.html")
OUT_CSV     <- file.path(DIR_OUT, "tabla_figS3_brechas_v5.csv")
OUT_HTML    <- HTML_TMPL   # updated in-place

# ── Load data ─────────────────────────────────────────────────────────────────
# Requires: 02_preparar_datos_v5.R has been run first (7 waves 2009-2025)
message("Loading: ", file.path(DIR_OUT, "olas_preparadas_v5.rds"))
OLAS <- readRDS(file.path(DIR_OUT, "olas_preparadas_v5.rds"))
message("Waves: ", paste(sort(unique(OLAS$anyo)), collapse = ", "),
        "  |  n = ", nrow(OLAS))

# ── Margins / lincom (delta method) ───────────────────────────────────────────
# Fits one logistic model and returns a 1-row tibble with edu_gap, orig_gap,
# total and their 95% CIs (percentage points).
gaps_one_wave <- function(df, resp, age_var) {
  d <- df |>
    filter(!is.na(estrato_4), !is.na(.data[[resp]]),
           !is.na(isced_3), !is.na(pais_nac_f), !is.na(.data[[age_var]])) |>
    mutate(
      y    = as.integer(.data[[resp]]),
      edu  = factor(isced_3,    levels = c("Alto", "Medio", "Bajo")),
      orig = factor(pais_nac_f, levels = c("España", "Extranjero")),
      age  = factor(.data[[age_var]])
    )
  if (nrow(d) < 50) { message("    skipped (n=", nrow(d), ")"); return(NULL) }

  mod <- tryCatch(
    glm(y ~ edu * orig + age, data = d, family = binomial),
    error = function(e) { message("    model error: ", e$message); NULL }
  )
  if (is.null(mod)) return(NULL)

  V  <- vcov(mod)
  b  <- coef(mod)

  # Average marginal prediction at fixed (edu, orig), observed age distribution
  avg_pred <- function(edu_lv, orig_lv) {
    nd      <- mutate(d, edu = factor(edu_lv, levels(d$edu)),
                        orig = factor(orig_lv, levels(d$orig)))
    mm      <- model.matrix(~ edu * orig + age, data = nd)
    # Keep only columns in the fitted model (handles perfect-separation drops)
    keep    <- intersect(names(b), colnames(mm))
    mm      <- mm[, keep, drop = FALSE]
    bk      <- b[keep]
    p       <- plogis(as.numeric(mm %*% bk))
    g       <- colMeans(mm * p * (1 - p))   # gradient of mean(p) w.r.t. β
    list(p = mean(p), g = g)
  }

  A <- avg_pred("Alto", "España")      # high educ, Spain-born
  B <- avg_pred("Bajo", "España")      # low  educ, Spain-born
  C <- avg_pred("Bajo", "Extranjero")  # low  educ, foreign-born

  cov2 <- function(ga, gb) as.numeric(ga %*% V[names(ga), names(gb)] %*% gb)

  se_diff <- function(ga, gb)
    100 * sqrt(max(cov2(ga, ga) + cov2(gb, gb) - 2 * cov2(ga, gb), 0))

  gap <- function(pa, pb, ga, gb) {
    d  <- 100 * (pa - pb)
    se <- se_diff(ga, gb)
    c(d, d - 1.96 * se, d + 1.96 * se)
  }

  eg  <- gap(A$p, B$p, A$g, B$g)
  og  <- gap(B$p, C$p, B$g, C$g)
  tot <- gap(A$p, C$p, A$g, C$g)

  tibble(
    edu_gap = eg[1],  edu_lo = eg[2],  edu_hi = eg[3],
    orig_gap = og[1], orig_lo = og[2], orig_hi = og[3],
    total    = tot[1],total_lo= tot[2],total_hi= tot[3]
  ) |> mutate(across(everything(), \(x) round(x, 1)))
}

# ── Panel definitions ─────────────────────────────────────────────────────────
PANELS <- tribble(
  ~panel,          ~resp,     ~age_var,          ~sex,
  "CRC (men)",     "soh_2a",  "edad_grupo",      "Hombre",
  "CRC (women)",   "soh_2a",  "edad_grupo",      "Mujer",
  "Breast",        "mama_2a", "edad_grupo",      "Mujer",
  "Cervical",      "cito_3a", "edad_grupo_cito", "Mujer"
)

WAVES <- c(2009, 2011, 2014, 2017, 2020, 2023, 2025)

# ── Compute all gaps ──────────────────────────────────────────────────────────
gaps_all <- PANELS |>
  pmap_dfr(function(panel, resp, age_var, sex) {
    message("\n── ", panel, " ──")
    purrr::map_dfr(WAVES, function(w) {
      df_w <- OLAS |> filter(anyo == w, sexo_f == sex)
      message("  ", w, "  n=", nrow(df_w))
      res  <- gaps_one_wave(df_w, resp, age_var)
      if (!is.null(res)) mutate(res, panel = panel, anyo = w)
    })
  }) |>
  select(panel, anyo, edu_gap, edu_lo, edu_hi,
         orig_gap, orig_lo, orig_hi, total, total_lo, total_hi)

message("\nResults: ", nrow(gaps_all), " panel × wave rows")
print(gaps_all, n = Inf)

write_csv(gaps_all, OUT_CSV)
message("Saved: ", OUT_CSV)

# ── Inject data into HTML ─────────────────────────────────────────────────────
row_js <- function(r)
  with(r, sprintf(
    "    {anyo:%d,edu_gap:%.1f,edu_lo:%.1f,edu_hi:%.1f,orig_gap:%.1f,orig_lo:%.1f,orig_hi:%.1f,total:%.1f,total_lo:%.1f,total_hi:%.1f}",
    anyo, edu_gap, edu_lo, edu_hi, orig_gap, orig_lo, orig_hi, total, total_lo, total_hi))

panel_js <- function(nm)
  sprintf('  "%s": [\n%s\n  ]', nm,
    paste(gaps_all |> filter(panel == nm) |> pmap_chr(\(...) row_js(list(...))) ,
          collapse = ",\n"))

data_js  <- paste0("const DATA = {\n",
                   paste(map_chr(unique(gaps_all$panel), panel_js), collapse = ",\n"),
                   "\n};")
waves_js <- 'const WAVES = [2009,2011,2014,2017,2020,2023,2025];'
ticks_js <- 'const WAVE_TICKS = ["EESE<br>2009","ENSE<br>2011","EESE<br>2014","ENSE<br>2017","EESE<br>2020","ESdE<br>2023","ESdE<br>2025"];'

html  <- paste(readLines(HTML_TMPL, warn = FALSE), collapse = "\n")
start <- regexpr("\n<script>\nconst DATA = {", html, fixed = TRUE)[[1]]
stopifnot(start > 0)
tp    <- regexpr("\nconst WAVE_TICKS = \\[", html)[[1]]
tend  <- tp + regexpr("\n", substring(html, tp + 1))[[1]]

new_html <- paste0(
  substring(html, 1, start - 1),
  "\n<script>\n", data_js, "\n\n", waves_js, "\n", ticks_js,
  substring(html, tend)
)

writeLines(new_html, OUT_HTML)
# Local copy in project root
writeLines(new_html,
  file.path(DIR_PROJ, "Supplementary_Figure_S3_7olas_v5.html"))

message("\n✓ Done — ", nrow(gaps_all), " gaps written to CSV and HTML")

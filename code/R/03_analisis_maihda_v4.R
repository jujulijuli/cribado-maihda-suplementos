# ============================================================
# 03_analisis_maihda_v4.R  —  Modelos MAIHDA CRUZADOS (cross-classified) — SIN PONDERACIÓN
# ============================================================
# Cambios respecto a v3:
#   - ANÁLISIS PRIMARIO: sin ponderación de encuesta (weights eliminados).
#     v3 usaba weights = peso_centrado (pesos de diseño escalados a media 1).
#     v4 omite el argumento weights → modelos de máxima verosimilitud estándar.
#   - Salidas con sufijo _v4 para no sobrescribir los resultados ponderados (_cc).
#
# Requiere output/olas_preparadas_v3.rds (02_preparar_datos_v4.R, o 02b_corregir_pais_nac_2009.R)
# ============================================================

if (file.exists("R/00_config.R")) source("R/00_config.R")
if (!exists("DIR_OUTPUT")) DIR_OUTPUT <- "output"
suppressPackageStartupMessages({ library(lme4); library(dplyr) })

# ── 1. Estratos (idéntico a v2) ─────────────────────────────
crear_estratos_soh_mama <- function(df) {
  df |> dplyr::mutate(
    estrato_id = interaction(sexo_f, isced_3, pais_nac_f, edad_grupo,
                             drop = TRUE, sep = "_") |> as.factor())
}
crear_estratos_cito <- function(df) {
  df |>
    dplyr::filter(sexo_f == "Mujer", edad <= 65) |>
    dplyr::mutate(
      estrato_id = interaction(isced_3, pais_nac_f, edad_grupo_cito,
                               drop = TRUE, sep = "_") |> as.factor())
}
preparar_df <- function(df, var_resp) {
  if (var_resp == "cito_3a") crear_estratos_cito(df)
  else if (var_resp == "mama_2a") df |> dplyr::filter(sexo_f == "Mujer") |> crear_estratos_soh_mama()
  else crear_estratos_soh_mama(df)
}
covs_para <- function(var_resp) {
  if (var_resp == "cito_3a") c("isced_3", "pais_nac_f", "edad_grupo_cito")
  else if (var_resp == "mama_2a") c("isced_3", "pais_nac_f", "edad_grupo")
  else c("sexo_f", "isced_3", "pais_nac_f", "edad_grupo")
}

# Muestra analítica común a nulo y ajustado (las filas con NA en estrato o
# CCAA se excluyen; estrato es NA fuera del rango de edad diana)
muestra_modelo <- function(df, var_resp, nivel3 = "ccaa_f") {
  df |>
    dplyr::filter(!is.na(.data[[var_resp]]), !is.na(estrato_id), !is.na(.data[[nivel3]])) |>
    dplyr::mutate(estrato_id = droplevels(estrato_id))
}

CTRL <- lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
RE_CC <- "(1 | estrato_id) + (1 | ccaa_f)"

# ── 2. Modelo nulo cruzado ──────────────────────────────────
ajustar_nulo <- function(df2, var_resp) {
  message("  Nulo cruzado '", var_resp, "' (", nrow(df2), " obs)...")
  lme4::glmer(as.formula(paste(var_resp, "~ 1 +", RE_CC)),
              data = df2, family = binomial("logit"),
              control = CTRL)
}

# ── 3. Modelo ajustado cruzado ──────────────────────────────
ajustar_completo <- function(df2, var_resp, covs) {
  covs_ok <- intersect(covs, names(df2))
  covs_ok <- covs_ok[sapply(covs_ok, function(v) {
    x <- df2[[v]]
    if (is.factor(x) || is.character(x)) length(unique(na.omit(x))) >= 2 else TRUE
  })]
  if (length(covs_ok) == 0) covs_ok <- "1"
  message("  Ajustado cruzado '", var_resp, "'...")
  lme4::glmer(as.formula(paste(var_resp, "~", paste(covs_ok, collapse = " + "), "+", RE_CC)),
              data = df2, family = binomial("logit"),
              control = CTRL)
}

# ── 4. Métricas MAIHDA ─────────────────────────────────────
var_comp <- function(m) {
  vc <- as.data.frame(lme4::VarCorr(m))
  c(est = vc$vcov[vc$grp == "estrato_id"], ccaa = vc$vcov[vc$grp == "ccaa_f"])
}
diag_conv <- function(m) {
  msg <- c(m@optinfo$conv$lme4$messages, m@optinfo$warnings)
  paste(unique(unlist(msg)), collapse = "; ")
}
metricas_maihda <- function(m0, m1 = NULL) {
  pi2_3 <- pi^2 / 3
  v0 <- var_comp(m0); den <- v0[["est"]] + v0[["ccaa"]] + pi2_3
  out <- tibble::tibble(
    sigma2_estrato_nulo = v0[["est"]], sigma2_ccaa_nulo = v0[["ccaa"]],
    VPC_estrato_pct = round(100 * v0[["est"]] / den, 2),
    VPC_ccaa_pct    = round(100 * v0[["ccaa"]] / den, 2),
    ICC_total_pct   = round(100 * (v0[["est"]] + v0[["ccaa"]]) / den, 2),
    singular_nulo = lme4::isSingular(m0), conv_nulo = diag_conv(m0))
  if (!is.null(m1)) {
    v1 <- var_comp(m1)
    out$sigma2_estrato_ajust <- v1[["est"]]; out$sigma2_ccaa_ajust <- v1[["ccaa"]]
    out$PCV_estrato_pct <- if (v0[["est"]]  == 0) NA_real_ else round(100 * (v0[["est"]]  - v1[["est"]])  / v0[["est"]],  2)
    out$PCV_ccaa_pct    <- if (v0[["ccaa"]] == 0) NA_real_ else round(100 * (v0[["ccaa"]] - v1[["ccaa"]]) / v0[["ccaa"]], 2)
    out$singular_ajust <- lme4::isSingular(m1); out$conv_ajust <- diag_conv(m1)
  }
  out
}

# AUC (discriminatory accuracy), modelo ajustado, sin ponderar:
#   AUC_fijos:          efectos fijos (ejes sociales aditivos)
#   AUC_fijos_estrato:  fijos + BLUP de estrato (posición interseccional completa)
#   AUC_completo:       fijos + estrato + CCAA
auc_modelo <- function(m1) {
  y <- lme4::getME(m1, "y")
  a <- function(p) as.numeric(pROC::auc(y, p, quiet = TRUE))
  tibble::tibble(
    AUC_fijos         = a(predict(m1, re.form = NA)),
    AUC_fijos_estrato = a(predict(m1, re.form = ~ (1 | estrato_id))),
    AUC_completo      = a(predict(m1)))
}

# OR de efectos fijos con IC de Wald (Tabla 3)
tabla_or <- function(m1) {
  co <- summary(m1)$coefficients
  co <- co[rownames(co) != "(Intercept)", , drop = FALSE]
  tibble::tibble(term = rownames(co), beta = co[, 1], se = co[, 2],
                 OR = exp(co[, 1]), IC_inf = exp(co[, 1] - 1.96 * co[, 2]),
                 IC_sup = exp(co[, 1] + 1.96 * co[, 2]), p = co[, 4])
}

# BLUPs de estrato del modelo nulo (Figura 3, S1)
blups_estrato <- function(m0, df2, var_resp) {
  re <- lme4::ranef(m0, condVar = TRUE)$estrato_id
  lv <- rownames(re)
  parts <- strsplit(lv, "_", fixed = TRUE)
  g <- function(i) vapply(parts, `[`, "", i)
  comp <- if (var_resp == "cito_3a")
    tibble::tibble(sexo = "Mujer", estudios = g(1), origen = g(2), edad = g(3))
  else tibble::tibble(sexo = g(1), estudios = g(2), origen = g(3), edad = g(4))
  n  <- as.integer(table(df2$estrato_id)[lv])
  np <- as.integer(tapply(df2[[var_resp]], df2$estrato_id, sum)[lv])
  dplyr::bind_cols(tibble::tibble(estrato_id = lv), comp,
    tibble::tibble(n = n, n_pos = np, blup = re[, 1],
                   se = sqrt(as.numeric(attr(re, "postVar")[1, 1, ])),
                   beta0 = unname(lme4::fixef(m0)[1])))
}

# ── 5. Pipeline por ola y variable ─────────────────────────
ajustar_maihda_v4 <- function(ola_df, var_resp, anyo) {
  message("\n── MAIHDA v4 (sin pesos) ", anyo, " | ", var_resp, " ──")
  df2 <- muestra_modelo(preparar_df(ola_df, var_resp), var_resp)
  n_pos <- sum(df2[[var_resp]] == 1)
  if (n_pos < 30) { message("  ⚠ Muy pocos positivos (", n_pos, ") — omitiendo"); return(NULL) }

  m0 <- tryCatch(ajustar_nulo(df2, var_resp), error = \(e) { message("  ✗ nulo: ", conditionMessage(e)); NULL })
  if (is.null(m0)) return(NULL)
  m1 <- tryCatch(ajustar_completo(df2, var_resp, covs_para(var_resp)),
                 error = \(e) { message("  ✗ ajustado: ", conditionMessage(e)); NULL })

  met <- metricas_maihda(m0, m1)
  met$anyo <- anyo; met$variable <- var_resp
  met$n_modelo <- nrow(df2); met$n_pos_modelo <- n_pos; met$n_estratos <- nlevels(df2$estrato_id)
  if (!is.null(m1)) met <- dplyr::bind_cols(met, auc_modelo(m1))

  list(m0 = m0, m1 = m1, metricas = met,
       or = if (!is.null(m1)) dplyr::mutate(tabla_or(m1), anyo = anyo, variable = var_resp),
       blups = dplyr::mutate(blups_estrato(m0, df2, var_resp), anyo = anyo, variable = var_resp))
}

# ── 6. Correr ───────────────────────────────────────────────
f_olas <- file.path(DIR_OUTPUT, "olas_preparadas_v3.rds")
message("\n── Cargando ", f_olas, " ──")
OLAS <- readRDS(f_olas)
VARS_RESP <- c("soh_2a", "mama_2a", "cito_3a")
jobs <- expand.grid(anyo = sort(unique(OLAS$anyo)), v = VARS_RESP, stringsAsFactors = FALSE)

n_cores <- if (.Platform$OS.type == "unix") max(1, parallel::detectCores() - 1) else 1
res_list <- parallel::mclapply(seq_len(nrow(jobs)), function(i)
  ajustar_maihda_v4(OLAS[OLAS$anyo == jobs$anyo[i], ], jobs$v[i], jobs$anyo[i]),
  mc.cores = n_cores)

# Misma estructura que resultados_maihda.rds: [[anyo]][[variable]]
resultados_v4 <- list()
for (i in seq_len(nrow(jobs)))
  resultados_v4[[as.character(jobs$anyo[i])]][[jobs$v[i]]] <- res_list[[i]]

pick <- function(nm) dplyr::bind_rows(lapply(res_list, `[[`, nm))
tabla_vpc_v4   <- pick("metricas") |> dplyr::arrange(variable, anyo)
tabla_or_v4    <- pick("or")       |> dplyr::arrange(variable, anyo)
tabla_blups_v4 <- pick("blups")    |> dplyr::arrange(variable, anyo, blup)

message("\n── Tabla VPC (cruzado) ──")
print(tabla_vpc_v4 |> dplyr::select(variable, anyo, n_modelo, VPC_estrato_pct, VPC_ccaa_pct,
                                    PCV_estrato_pct, PCV_ccaa_pct, AUC_fijos_estrato), n = Inf)

saveRDS(resultados_v4, file.path(DIR_OUTPUT, "resultados_maihda_v4.rds"))
readr::write_csv(tabla_vpc_v4,   file.path(DIR_OUTPUT, "tabla_vpc_v4.csv"))
readr::write_csv(tabla_or_v4,    file.path(DIR_OUTPUT, "tabla_or_v4.csv"))
readr::write_csv(tabla_blups_v4, file.path(DIR_OUTPUT, "tabla_blups_v4.csv"))
message("\n✓ Guardados en ", DIR_OUTPUT, ": resultados_maihda_v4.rds, tabla_vpc_v4.csv, tabla_or_v4.csv, tabla_blups_v4.csv")

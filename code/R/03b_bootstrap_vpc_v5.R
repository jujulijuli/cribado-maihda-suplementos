# ============================================================
# 03b_bootstrap_vpc_v5.R — IC 95 % del VPC por BOOTSTRAP PARAMÉTRICO — 7 olas
# Requiere: output/olas_preparadas_v5.rds, output/tabla_vpc_v5.csv (de 03)
# Salidas:  output/tabla_vpc_boot_v5.csv
#           output/tabla_vpc_v5.csv  (actualizado con columnas IC)
#           output/bootstrap_vpc_replicas_v5.rds  (todas las réplicas)
# ============================================================
# Cambios respecto a la versión del 13/11/2026 (archivada en R/_archivo/):
#   - Antes: bootstrap NO paramétrico (remuestreo de individuos dentro de cada CCAA),
#     aunque el artículo decía "parametric". Además, las réplicas con cualquier aviso
#     de glmer se descartaban (warning -> NA). Los avisos aparecen sobre todo cuando la
#     varianza está cerca de 0, así que se perdían justo las réplicas bajas y el IC
#     quedaba desplazado hacia arriba: en CRC 2009, 2011, 2014 y 2025 la estimación
#     puntual del VPC de estrato quedaba POR DEBAJO de su IC.
#   - Ahora: bootstrap paramétrico (lme4::bootMer): se simulan respuestas desde el
#     modelo nulo ajustado, con nuevos efectos aleatorios de estrato y de territorio en
#     cada réplica (use.u = FALSE), y se reajusta el mismo modelo. Así se propaga también
#     la incertidumbre de tener solo 19 unidades territoriales.
#   - No se descarta ninguna réplica por avisos: se guardan todas, se cuenta cuántas
#     acaban en el límite (varianza estimada = 0, ajuste singular) y cuántas fallan.
#   - IC = percentiles 2,5 y 97,5 de todas las réplicas válidas. Si muchas réplicas dan
#     varianza 0, el límite inferior es 0: es el resultado correcto, no un fallo.
#
# Modelo nulo: idéntico a 03_analisis_maihda_v5.R
#   y ~ 1 + (1 | estrato_id) + (1 | ccaa_f), binomial logit, sin pesos, bobyqa.
#   VPC_k = σ²_k / (σ²_estrato + σ²_ccaa + π²/3)
#
# Tiempo: 21 combinaciones × N_BOOT ajustes. Con N_BOOT = 200 y varios núcleos,
# del orden de 1–3 h (en paralelo con parallel = "multicore"; en Windows, secuencial).
# ============================================================

if (file.exists("R/00_config.R")) source("R/00_config.R")
if (!exists("DIR_OUTPUT")) DIR_OUTPUT <- "output"
suppressPackageStartupMessages({ library(lme4); library(dplyr); library(readr) })

N_BOOT <- if (exists("N_BOOT")) N_BOOT else 200
CTRL   <- lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
pi2_3  <- pi^2 / 3
NCPUS  <- if (.Platform$OS.type == "unix") max(1, parallel::detectCores() - 1) else 1

OLAS <- readRDS(file.path(DIR_OUTPUT, "olas_preparadas_v5.rds"))
VARS_RESP <- c("soh_2a", "mama_2a", "cito_3a")
jobs <- if (exists("JOBS_BOOT")) JOBS_BOOT else
  expand.grid(anyo = sort(unique(OLAS$anyo)), v = VARS_RESP, stringsAsFactors = FALSE)

# --- Muestra y estratos (idénticos a 03_analisis_maihda_v5.R) --------------
crear_estratos_soh_mama <- function(df) df |>
  dplyr::mutate(estrato_id = interaction(sexo_f, isced_3, pais_nac_f, edad_grupo, drop = TRUE, sep = "_") |> as.factor())
crear_estratos_cito <- function(df) df |>
  dplyr::filter(sexo_f == "Mujer", edad <= 65) |>
  dplyr::mutate(estrato_id = interaction(isced_3, pais_nac_f, edad_grupo_cito, drop = TRUE, sep = "_") |> as.factor())
preparar_df <- function(df, var_resp) {
  if (var_resp == "cito_3a") crear_estratos_cito(df)
  else if (var_resp == "mama_2a") df |> dplyr::filter(sexo_f == "Mujer") |> crear_estratos_soh_mama()
  else crear_estratos_soh_mama(df)
}
muestra_modelo <- function(df, var_resp) df |>
  dplyr::filter(!is.na(.data[[var_resp]]), !is.na(estrato_id), !is.na(ccaa_f)) |>
  dplyr::mutate(estrato_id = droplevels(estrato_id))

# --- Estadístico de cada réplica -------------------------------------------
vpc_de <- function(m) {
  vc <- as.data.frame(lme4::VarCorr(m))
  v_e <- vc$vcov[vc$grp == "estrato_id"]; v_c <- vc$vcov[vc$grp == "ccaa_f"]
  den <- v_e + v_c + pi2_3
  c(vpc_est = 100 * v_e / den, vpc_ccaa = 100 * v_c / den,
    s2_est = v_e, s2_ccaa = v_c, singular = as.numeric(lme4::isSingular(m, tol = 1e-4)))
}

boot_job <- function(anyo_i, var_resp_i) {
  t0 <- Sys.time()
  df2 <- muestra_modelo(preparar_df(OLAS[OLAS$anyo == anyo_i, ], var_resp_i), var_resp_i)
  f <- as.formula(paste(var_resp_i, "~ 1 + (1 | estrato_id) + (1 | ccaa_f)"))
  m0 <- lme4::glmer(f, data = df2, family = binomial("logit"), control = CTRL)
  est <- vpc_de(m0)
  b <- suppressWarnings(suppressMessages(
    lme4::bootMer(m0, FUN = vpc_de, nsim = N_BOOT, type = "parametric", use.u = FALSE,
                  parallel = if (NCPUS > 1) "multicore" else "no", ncpus = NCPUS)))
  r <- as.data.frame(b$t)
  ok <- stats::complete.cases(r)
  q <- function(x) unname(quantile(x[ok], c(.025, .975)))
  message(sprintf("  %s %s: %d/%d réplicas válidas, %.0f%% en el límite (σ²_estrato≈0)  [%.1f min]",
                  var_resp_i, anyo_i, sum(ok), N_BOOT, 100 * mean(r$s2_est[ok] < 1e-6),
                  as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  list(
    resumen = tibble::tibble(
      anyo = anyo_i, variable = var_resp_i, n_boot = N_BOOT, n_validas = sum(ok),
      vpc_est = est[["vpc_est"]], vpc_ccaa = est[["vpc_ccaa"]],
      vpc_est_lo = q(r$vpc_est)[1], vpc_est_hi = q(r$vpc_est)[2],
      vpc_ccaa_lo = q(r$vpc_ccaa)[1], vpc_ccaa_hi = q(r$vpc_ccaa)[2],
      pct_rep_s2est_0 = 100 * mean(r$s2_est[ok] < 1e-6),
      pct_rep_s2ccaa_0 = 100 * mean(r$s2_ccaa[ok] < 1e-6),
      pct_rep_singular = 100 * mean(r$singular[ok] == 1)),
    replicas = dplyr::mutate(r, anyo = anyo_i, variable = var_resp_i))
}

message("\n── Bootstrap paramétrico VPC (", N_BOOT, " réplicas × ", nrow(jobs), " combinaciones, ", NCPUS, " núcleos) ──")
set.seed(2025)
t0 <- proc.time()
res <- lapply(seq_len(nrow(jobs)), function(i) boot_job(jobs$anyo[i], jobs$v[i]))
message(sprintf("Tiempo total: %.1f min", (proc.time() - t0)[["elapsed"]] / 60))

tabla_vpc_boot <- dplyr::bind_rows(lapply(res, `[[`, "resumen"))
saveRDS(dplyr::bind_rows(lapply(res, `[[`, "replicas")), file.path(DIR_OUTPUT, "bootstrap_vpc_replicas_v5.rds"))

if (!exists("JOBS_BOOT")) {                       # solo con la ejecución completa se actualiza la tabla del artículo
  tabla_vpc_v5 <- readr::read_csv(file.path(DIR_OUTPUT, "tabla_vpc_v5.csv"), show_col_types = FALSE) |>
    dplyr::select(-dplyr::any_of(c("vpc_est_lo", "vpc_est_hi", "vpc_ccaa_lo", "vpc_ccaa_hi"))) |>
    dplyr::left_join(tabla_vpc_boot |> dplyr::select(anyo, variable, vpc_est_lo, vpc_est_hi, vpc_ccaa_lo, vpc_ccaa_hi),
                     by = c("anyo", "variable"))
  # Coherencia: la estimación de 03 y la reajustada aquí deben coincidir
  chk <- dplyr::inner_join(tabla_vpc_v5, tabla_vpc_boot, by = c("anyo", "variable"))
  if (max(abs(chk$VPC_estrato_pct - chk$vpc_est), na.rm = TRUE) > 0.01)
    warning("El VPC reajustado en 03b no coincide con tabla_vpc_v5.csv: ¿se ha ejecutado 03 con los mismos datos?")
  readr::write_csv(tabla_vpc_v5, file.path(DIR_OUTPUT, "tabla_vpc_v5.csv"))
  readr::write_csv(tabla_vpc_boot, file.path(DIR_OUTPUT, "tabla_vpc_boot_v5.csv"))
  message("✓ Guardado tabla_vpc_v5.csv y tabla_vpc_boot_v5.csv")
}
print(tabla_vpc_boot |> dplyr::mutate(dplyr::across(where(is.double), ~ round(.x, 2))), n = Inf, width = 200)

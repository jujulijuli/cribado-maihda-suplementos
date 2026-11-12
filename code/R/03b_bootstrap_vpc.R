# ============================================================
# 03b_bootstrap_vpc.R — Bootstrap IC para VPC (200 réplicas)
# Requiere: output/olas_preparadas_v3.rds, output/tabla_vpc_v4.csv
# Salidas:  output/tabla_vpc_boot_v4.csv
#           output/tabla_vpc_v4.csv (actualizado con IC)
# ============================================================

if (file.exists("R/00_config.R")) source("R/00_config.R")
if (!exists("DIR_OUTPUT")) DIR_OUTPUT <- "output"
suppressPackageStartupMessages({ library(lme4); library(dplyr); library(readr) })

CTRL  <- lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
pi2_3 <- pi^2 / 3

# Cargar datos y estructura de jobs
OLAS <- readRDS(file.path(DIR_OUTPUT, "olas_preparadas_v3.rds"))
VARS_RESP <- c("soh_2a", "mama_2a", "cito_3a")
jobs <- expand.grid(anyo = sort(unique(OLAS$anyo)), v = VARS_RESP, stringsAsFactors = FALSE)

# Funciones de estrato (mismas que 03_analisis_maihda_v4.R)
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
muestra_modelo <- function(df, var_resp) {
  df |>
    dplyr::filter(!is.na(.data[[var_resp]]), !is.na(estrato_id), !is.na(ccaa_f)) |>
    dplyr::mutate(estrato_id = droplevels(estrato_id))
}

# Una réplica bootstrap
boot_vpc_una_vez <- function(df2, var_resp) {
  df_b <- df2 |>
    dplyr::group_by(ccaa_f) |>
    dplyr::slice_sample(prop = 1, replace = TRUE) |>
    dplyr::ungroup() |>
    dplyr::mutate(estrato_id = droplevels(estrato_id))
  m <- tryCatch(
    lme4::glmer(as.formula(paste(var_resp, "~ 1 + (1 | estrato_id) + (1 | ccaa_f)")),
                data = df_b, family = binomial("logit"), control = CTRL),
    error = function(e) NULL, warning = function(w) NULL)
  if (is.null(m)) return(c(vpc_est = NA_real_, vpc_ccaa = NA_real_))
  vc  <- as.data.frame(lme4::VarCorr(m))
  v_e <- vc$vcov[vc$grp == "estrato_id"]
  v_c <- vc$vcov[vc$grp == "ccaa_f"]
  den <- v_e + v_c + pi2_3
  c(vpc_est = 100 * v_e / den, vpc_ccaa = 100 * v_c / den)
}

# Bootstrap para una combinación anyo × variable
boot_vpc_job <- function(anyo_i, var_resp_i, n_boot = 200) {
  message("  Bootstrap ", anyo_i, " | ", var_resp_i)
  df2 <- muestra_modelo(preparar_df(OLAS[OLAS$anyo == anyo_i, ], var_resp_i), var_resp_i)
  n_cores <- if (.Platform$OS.type == "unix") max(1, parallel::detectCores() - 1) else 1
  mat <- do.call(rbind, parallel::mclapply(seq_len(n_boot), function(b)
    boot_vpc_una_vez(df2, var_resp_i), mc.cores = n_cores))
  tibble::tibble(
    anyo = anyo_i, variable = var_resp_i,
    vpc_est_lo  = quantile(mat[, "vpc_est"],  0.025, na.rm = TRUE),
    vpc_est_hi  = quantile(mat[, "vpc_est"],  0.975, na.rm = TRUE),
    vpc_ccaa_lo = quantile(mat[, "vpc_ccaa"], 0.025, na.rm = TRUE),
    vpc_ccaa_hi = quantile(mat[, "vpc_ccaa"], 0.975, na.rm = TRUE))
}

# Correr las 18 combinaciones
message("\n── Bootstrap VPC (200 réplicas) ──")
set.seed(2024)
boot_list <- lapply(seq_len(nrow(jobs)), function(i)
  boot_vpc_job(jobs$anyo[i], jobs$v[i]))
tabla_vpc_boot <- dplyr::bind_rows(boot_list)

# Unir con tabla_vpc_v4 y guardar
tabla_vpc_v4 <- readr::read_csv(file.path(DIR_OUTPUT, "tabla_vpc_v4.csv"), show_col_types = FALSE) |>
  dplyr::select(-dplyr::any_of(c("vpc_est_lo","vpc_est_hi","vpc_ccaa_lo","vpc_ccaa_hi"))) |>
  dplyr::left_join(tabla_vpc_boot, by = c("anyo", "variable"))

readr::write_csv(tabla_vpc_v4,   file.path(DIR_OUTPUT, "tabla_vpc_v4.csv"))
readr::write_csv(tabla_vpc_boot, file.path(DIR_OUTPUT, "tabla_vpc_boot_v4.csv"))
message("✓ Bootstrap guardado en tabla_vpc_v4.csv y tabla_vpc_boot_v4.csv")
print(tabla_vpc_boot, n = Inf)

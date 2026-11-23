# ============================================================
# 03c_descomposicion_pesos_v5.R — 7 olas (2009–2025). Tabla suplementaria S2.
# Igual que 03b_analisis_descomposicion_pesos_v1.R (6 olas, olas_preparadas_v3.rds),
# pero lee olas_preparadas_v5.rds. Para 2009–2023 da los mismos VPC que v1.
# Descomposición del efecto del peso en modelos RE-MAIHDA
# ============================================================
# Objetivo: cuantificar qué componente del peso (entre-CCAA vs.
# dentro-CCAA) explica la discrepancia entre el análisis principal
# (sin pesos) y el análisis de sensibilidad (con pesos).
#
# Para cada ola × variable se ajustan 4 modelos:
#   M0: sin pesos            (= análisis principal)
#   M1: peso real            (= análisis de sensibilidad)
#   M2: solo componente entre-CCAA  (w_ccaa = media_ccaa / media_global)
#   M3: solo componente dentro-CCAA (w_dentro = peso_centrado / media_ccaa)
#
# Descomposición (Raquel Gómez, comunicación personal):
#   w_ccaa   = mean(peso | ccaa, ola) / mean(peso | ola)
#   w_dentro = peso / mean(peso | ccaa, ola)
#   => peso = w_ccaa * w_dentro  (multiplicativamente)
#
# Referencia metodológica: Rabe-Hesketh & Skrondal (2006)
#   J R Statist Soc A 169(4):805-827
#
# Requiere: output/olas_preparadas_v5.rds
# Salidas:  output/tabla_descomposicion_pesos_v5.csv
#           output/tabla_descomposicion_pesos_v5.rds
# ============================================================

if (file.exists("R/00_config.R")) source("R/00_config.R")
if (!exists("DIR_OUTPUT")) DIR_OUTPUT <- "output"
suppressPackageStartupMessages({ library(lme4); library(dplyr) })

# ── Funciones auxiliares (idénticas a 03_analisis_maihda_v4.R) ──

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

CTRL   <- lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
RE_CC  <- "(1 | estrato_id) + (1 | ccaa_f)"

# ── Descomposición del peso ──────────────────────────────────
# Genera w_ccaa y w_dentro dentro del df ya filtrado para la ola
descomponer_peso <- function(df2) {
  df2 |>
    dplyr::group_by(ccaa_f) |>
    dplyr::mutate(media_ccaa = mean(peso_centrado, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      media_global = mean(media_ccaa, na.rm = TRUE),   # media de medias por CCAA
      w_ccaa   = media_ccaa / media_global,
      w_dentro = peso_centrado / media_ccaa
    )
}

# ── Ajustar un modelo nulo con/sin pesos ─────────────────────
ajustar_nulo_w <- function(df2, var_resp, w_var = NULL, etiqueta = "") {
  message("    Nulo cruzado [", etiqueta, "] '", var_resp,
          "' (n=", nrow(df2), ")...")
  fml <- as.formula(paste(var_resp, "~ 1 +", RE_CC))

  if (is.null(w_var)) {
    m <- tryCatch(
      lme4::glmer(fml, data = df2, family = binomial("logit"), control = CTRL),
      error = \(e) { message("    ✗ ", etiqueta, ": ", conditionMessage(e)); NULL }
    )
  } else {
    # lme4 acepta weights= como vector numérico (frecuencia/importancia)
    # Centramos los pesos para que sumen n (conserva escala de la log-verosimilitud)
    w <- df2[[w_var]]
    w_cent <- w / mean(w, na.rm = TRUE) * 1   # media = 1 → Σw = n
    m <- tryCatch(
      lme4::glmer(fml, data = df2, family = binomial("logit"),
                  weights = w_cent, control = CTRL),
      error = \(e) { message("    ✗ ", etiqueta, ": ", conditionMessage(e)); NULL }
    )
  }
  m
}

# ── Extraer VPC del modelo nulo ──────────────────────────────
vpc_nulo <- function(m) {
  if (is.null(m)) return(c(VPC_estrato = NA_real_, VPC_ccaa = NA_real_,
                            sigma2_estrato = NA_real_, sigma2_ccaa = NA_real_,
                            singular = NA, convergencia = NA_character_))
  pi2_3 <- pi^2 / 3
  vc <- as.data.frame(lme4::VarCorr(m))
  s_e <- vc$vcov[vc$grp == "estrato_id"]
  s_c <- vc$vcov[vc$grp == "ccaa_f"]
  den <- s_e + s_c + pi2_3
  msg <- c(m@optinfo$conv$lme4$messages, m@optinfo$warnings)
  c(VPC_estrato   = round(100 * s_e / den, 2),
    VPC_ccaa      = round(100 * s_c / den, 2),
    sigma2_estrato = round(s_e, 6),
    sigma2_ccaa    = round(s_c, 6),
    singular       = as.numeric(lme4::isSingular(m)),
    convergencia   = paste(unique(unlist(msg)), collapse = "; "))
}

# ── Pipeline por ola × variable ──────────────────────────────
descomposicion_ola <- function(ola_df, var_resp, anyo) {
  message("\n── Descomposición pesos ", anyo, " | ", var_resp, " ──")

  df_base <- muestra_modelo(preparar_df(ola_df, var_resp), var_resp)
  n_pos   <- sum(df_base[[var_resp]] == 1, na.rm = TRUE)
  if (n_pos < 30) {
    message("  ⚠ Muy pocos positivos (", n_pos, ") — omitiendo")
    return(NULL)
  }
  if (!"peso_centrado" %in% names(df_base) || all(is.na(df_base$peso_centrado))) {
    message("  ⚠ Variable 'peso' no disponible — omitiendo")
    return(NULL)
  }

  # Añadir componentes del peso
  df2 <- descomponer_peso(df_base)

  # 4 modelos
  m_sinpeso  <- ajustar_nulo_w(df2, var_resp, w_var = NULL,       etiqueta = "sin_peso")
  m_pesoreal <- ajustar_nulo_w(df2, var_resp, w_var = "peso_centrado",     etiqueta = "peso_real")
  m_wccaa    <- ajustar_nulo_w(df2, var_resp, w_var = "w_ccaa",   etiqueta = "w_ccaa")
  m_wdentro  <- ajustar_nulo_w(df2, var_resp, w_var = "w_dentro", etiqueta = "w_dentro")

  v_sinpeso  <- vpc_nulo(m_sinpeso)
  v_pesoreal <- vpc_nulo(m_pesoreal)
  v_wccaa    <- vpc_nulo(m_wccaa)
  v_wdentro  <- vpc_nulo(m_wdentro)

  tibble::tibble(
    anyo     = anyo,
    variable = var_resp,
    n_modelo = nrow(df2),
    n_pos    = n_pos,
    modelo   = c("sin_peso", "peso_real", "w_ccaa", "w_dentro"),
    VPC_estrato_pct  = as.numeric(c(v_sinpeso["VPC_estrato"],  v_pesoreal["VPC_estrato"],
                                     v_wccaa["VPC_estrato"],    v_wdentro["VPC_estrato"])),
    VPC_ccaa_pct     = as.numeric(c(v_sinpeso["VPC_ccaa"],     v_pesoreal["VPC_ccaa"],
                                     v_wccaa["VPC_ccaa"],       v_wdentro["VPC_ccaa"])),
    sigma2_estrato   = as.numeric(c(v_sinpeso["sigma2_estrato"], v_pesoreal["sigma2_estrato"],
                                     v_wccaa["sigma2_estrato"],   v_wdentro["sigma2_estrato"])),
    sigma2_ccaa      = as.numeric(c(v_sinpeso["sigma2_ccaa"],   v_pesoreal["sigma2_ccaa"],
                                     v_wccaa["sigma2_ccaa"],     v_wdentro["sigma2_ccaa"])),
    singular         = as.numeric(c(v_sinpeso["singular"],      v_pesoreal["singular"],
                                     v_wccaa["singular"],        v_wdentro["singular"])),
    convergencia     = c(v_sinpeso["convergencia"], v_pesoreal["convergencia"],
                         v_wccaa["convergencia"],   v_wdentro["convergencia"])
  )
}

# ── Correr ───────────────────────────────────────────────────
f_olas <- file.path(DIR_OUTPUT, "olas_preparadas_v5.rds")
message("\n── Cargando ", f_olas, " ──")
OLAS <- readRDS(f_olas)

VARS_RESP <- c("soh_2a", "mama_2a", "cito_3a")
jobs <- expand.grid(anyo = sort(unique(OLAS$anyo)), v = VARS_RESP,
                    stringsAsFactors = FALSE)

# Secuencial — mclapply con lme4 en macOS puede fallar silenciosamente
res_list <- lapply(seq_len(nrow(jobs)), function(i)
  descomposicion_ola(OLAS[OLAS$anyo == jobs$anyo[i], ], jobs$v[i], jobs$anyo[i]))

res_ok <- Filter(Negate(is.null), res_list)
if (length(res_ok) == 0) stop("Todos los modelos fallaron — revisa olas_preparadas_v5.rds")

tabla_descomposicion <- dplyr::bind_rows(res_ok) |>
  dplyr::arrange(variable, anyo, modelo)

message("\n── Resumen descomposición ──")
print(tabla_descomposicion |>
        dplyr::select(variable, anyo, modelo, VPC_estrato_pct, VPC_ccaa_pct), n = Inf)

saveRDS(tabla_descomposicion,
        file.path(DIR_OUTPUT, "tabla_descomposicion_pesos_v5.rds"))
readr::write_csv(tabla_descomposicion,
                 file.path(DIR_OUTPUT, "tabla_descomposicion_pesos_v5.csv"))

message("\n✓ Guardado: output/tabla_descomposicion_pesos_v5.rds/.csv")

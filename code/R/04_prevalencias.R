# ============================================================
# 04_prevalencias.R — Prevalencias ponderadas de cribado por ola (Tabla 2 / Figura 1)
# Entrada:  output/olas_preparadas_v3.rds   Salida: output/tabla_prevalencias.csv
# Nota: diseño con pesos muestrales únicamente (sin conglomerados ni estratos).
# ============================================================
source("R/00_config.R")
OLAS <- readRDS(file.path(DIR_OUTPUT, "olas_preparadas_v3.rds"))
calcular_prevalencias <- function(OLAS, vars = c("soh_2a", "mama_2a", "cito_3a")) {
  purrr::map_dfr(vars, function(v) {
    purrr::map_dfr(sort(unique(OLAS$anyo)), function(yr) {
      d <- if (v == "cito_3a") {
        OLAS |> dplyr::filter(anyo == yr, sexo_f == "Mujer", !is.na(edad_grupo_cito),
                              !is.na(.data[[v]]), !is.na(peso_centrado))
      } else if (v == "mama_2a") {
        OLAS |> dplyr::filter(anyo == yr, sexo_f == "Mujer", !is.na(edad_grupo),
                              !is.na(.data[[v]]), !is.na(peso_centrado))
      } else {
        OLAS |> dplyr::filter(anyo == yr, !is.na(edad_grupo), !is.na(.data[[v]]), !is.na(peso_centrado))
      }
      des <- survey::svydesign(ids = ~1, weights = ~peso_centrado, data = d)
      est <- survey::svymean(as.formula(paste0("~", v)), des, na.rm = TRUE)
      tibble::tibble(Año = yr, N = nrow(d), Prevalencia = round(100 * as.numeric(est)[1], 1),
                     SE = round(100 * sqrt(attr(est, "var")[1]), 2), Variable = v)
    })
  })
}
tabla_prevalencias <- calcular_prevalencias(OLAS)
print(tabla_prevalencias)
readr::write_csv(tabla_prevalencias, file.path(DIR_OUTPUT, "tabla_prevalencias.csv"))

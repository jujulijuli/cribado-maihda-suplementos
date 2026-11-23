# ============================================================
# 02_preparar_datos_v5.R  —  Armonización MAIHDA × 7 olas
# ============================================================
# Proyecto: CRISEC (GN2026) — desigualdades en cribado de cáncer
#
# Cambios respecto a v4:
#   8. Nueva ola: ESdE 2025
#      - Variables de cribado idénticas a ESdE 2023:
#        L10/L11 (SOH), L14/L15 (mama), L16/L17 (cérvix)
#      - País de nacimiento: A1a (1=España, 2=Extranjero)
#      - Nivel educativo: NIVEST (9 cat ISCED)
#      - Clase social: CLASE_PR
#      - Factor de ponderación: FACTORADULTO
#      - Ruta: data/datos_2025/ESdEadulto_2025/SPSS/ESdEadulto_2025.sav
#   9. Output: output/olas_preparadas_v5.rds  (7 olas: 2009–2025)
#
# Flujo de uso (igual que v4):
#   source("R/01_cargar_datos.R")        # carga *_raw en memoria (incluye esde2025_raw)
#   source("R/02_preparar_datos_v5.R")   # armoniza y guarda olas_preparadas_v5.rds
# ============================================================

# ── Heredar todas las funciones de v4 ────────────────────────
options(ccr.solo_funciones = TRUE)   # evita que v4 ejecute su bloque (6 olas) y guarde v3.rds
source("R/02_preparar_datos_v4.R")
options(ccr.solo_funciones = NULL)

# ── Nueva función: ESdE 2025 ─────────────────────────────────
# Idéntica a preparar_esde2023(); las variables del cuestionario no cambiaron.
preparar_esde2025 <- function(df) {
  message("\n── Preparando ESdE 2025 ──")
  df <- dplyr::rename_with(df, tolower)
  df <- df |> dplyr::filter(edada >= 25, edada <= 69)
  message("  n total 25-69: ", nrow(df))

  df <- df |> dplyr::mutate(
    peso_centrado   = .peso_centrado(factoradulto),
    sexo_f          = factor(as.integer(sexoa), levels = c(1, 2),
                             labels = c("Hombre", "Mujer")),
    edad            = as.integer(edada),
    edad_grupo      = .grupos_edad(edada),
    edad_grupo_cito = .grupos_edad_cito(edada),
    ccaa_f          = .ccaa_factor(ccaa),

    # País de nacimiento: A1a (1=España, 2=Extranjero, 9=NC)
    pais_nac_f = dplyr::case_when(
      a1a == 1               ~ "España",
      !is.na(a1a) & a1a != 1 ~ "Extranjero",
      TRUE                   ~ NA_character_) |>
      factor(levels = c("España", "Extranjero")),

    # Nivel educativo → ISCED desde escala 9 categorías (NIVEST)
    isced_f = .isced_desde_9cat(nivest),
    isced_3 = .isced_3(isced_f),

    # Clase social
    clase_3 = .clase_3(clase_pr),

    # SOH: positivo = alguna vez (L10=1) + últimos 2 años (L11 ∈ {1,2})
    soh_2a = dplyr::case_when(
      l10 == 1 & l11 %in% c(1, 2)   ~ 1L,
      l10 == 2                       ~ 0L,
      l10 == 1 & l11 %in% c(3, 4, 5) ~ 0L,
      TRUE                           ~ NA_integer_),

    # Mamografía: positivo = alguna vez + últimos 2 años (L15 ∈ {1,2})
    mama_algvez = dplyr::case_when(l14 == 1 ~ 1L, l14 == 2 ~ 0L, TRUE ~ NA_integer_),
    mama_2a = dplyr::case_when(
      mama_algvez == 1 & l15 %in% c(1, 2) ~ 1L,
      mama_algvez == 0                     ~ 0L,
      mama_algvez == 1 & l15 %in% c(3, 4) ~ 0L,
      TRUE                                 ~ NA_integer_),

    # Citología: positivo = alguna vez + últimos 3 años (L17 ∈ {1,2,3})
    cito_algvez = dplyr::case_when(l16 == 1 ~ 1L, l16 == 2 ~ 0L, TRUE ~ NA_integer_),
    cito_3a = dplyr::case_when(
      cito_algvez == 1 & l17 %in% c(1, 2, 3) ~ 1L,
      cito_algvez == 0                        ~ 0L,
      cito_algvez == 1 & l17 %in% c(4, 5)    ~ 0L,
      TRUE                                    ~ NA_integer_),

    anyo = 2025L
  )

  message("  SOH_2a:  n=", sum(df$soh_2a  == 1, na.rm = TRUE), "/",
          sum(!is.na(df$soh_2a)),  " (", round(100 * mean(df$soh_2a  == 1, na.rm = TRUE), 1), "%)")
  message("  Mama_2a: n=", sum(df$mama_2a == 1, na.rm = TRUE), "/",
          sum(!is.na(df$mama_2a)), " (", round(100 * mean(df$mama_2a == 1, na.rm = TRUE), 1), "%)")
  message("  Cito_3a: n=", sum(df$cito_3a == 1, na.rm = TRUE), "/",
          sum(!is.na(df$cito_3a)), " (", round(100 * mean(df$cito_3a == 1, na.rm = TRUE), 1), "%)")
  df
}

# ── Bloque de ejecución ──────────────────────────────────────
if (!exists("eese2009_raw")) {
  message("  [02_preparar_datos_v5.R] Cargado como librería de funciones — OK")
} else {
  # Hogar ENSE 2011
  ruta_hogar_2011 <- file.path(DIR_DATA, "Hogar-ENSE-2011-12",
                                "MICRODATO ANONIMIZADO PUBLICACION HOGAR.txt")
  if (file.exists(ruta_hogar_2011)) {
    ense2011_hogar_raw <- leer_hogar_ense2011(ruta_hogar_2011)
  } else {
    message("  AVISO: no se encontró fichero de hogar ENSE 2011.")
    ense2011_hogar_raw <- NULL
  }

  # Preparar todas las olas
  eese2009 <- preparar_eese2009(eese2009_raw)                    |> agregar_estratos()
  ense2011 <- preparar_ense2011(ense2011_raw, ense2011_hogar_raw) |> agregar_estratos()
  eese2014 <- preparar_eese2014(eese2014_raw)                    |> agregar_estratos()
  ense2017 <- preparar_ense2017(ense2017_raw)                    |> agregar_estratos()
  eese2020 <- preparar_eese2020(eese2020_raw)                    |> agregar_estratos()
  esde2023 <- preparar_esde2023(esde2023_raw)                    |> agregar_estratos()
  esde2025 <- preparar_esde2025(esde2025_raw)                    |> agregar_estratos()

  OLAS <- dplyr::bind_rows(
    lapply(
      list(eese2009, ense2011, eese2014, ense2017, eese2020, esde2023, esde2025),
      dplyr::select, dplyr::any_of(VARS_COMUNES)
    )
  ) |>
    dplyr::mutate(
      anyo_f   = factor(anyo),
      encuesta = dplyr::case_when(
        anyo %in% c(2011, 2017) ~ paste0("ENSE ", anyo),
        anyo %in% c(2023, 2025) ~ paste0("ESdE ", anyo),
        TRUE                    ~ paste0("EESE ", anyo))
    )

  # Resumen
  message("\n── Distribución ISCED por ola (%) ──")
  OLAS |>
    dplyr::filter(!is.na(isced_3)) |>
    dplyr::group_by(anyo, isced_3) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    dplyr::group_by(anyo) |>
    dplyr::mutate(pct = round(100 * n / sum(n), 1)) |>
    tidyr::pivot_wider(names_from = isced_3, values_from = c(n, pct)) |>
    print()

  message("\n── N estratos por ola ──")
  OLAS |>
    dplyr::group_by(anyo) |>
    dplyr::summarise(
      n_total = dplyr::n(),
      n_est4  = dplyr::n_distinct(estrato_4, na.rm = TRUE),
      n_est5  = dplyr::n_distinct(estrato_5, na.rm = TRUE),
      .groups = "drop"
    ) |> print()

  saveRDS(OLAS, file.path(DIR_OUTPUT, "olas_preparadas_v5.rds"))
  message("\n✓ Guardado output/olas_preparadas_v5.rds  (7 olas: 2009–2025)")
}

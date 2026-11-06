# ============================================================
# 01_cargar_datos.R  —  Lectura de microdatos INE
# ============================================================
# Estructura real en disco:
#   data/eese2009/SPSS/EESEADULTO_2009.sav
#   data/eese2014/SPSS/EESEADULTO_2014.sav
#   data/eese2020/SPSS/EESEadulto_2020.sav
#   data/esde2023/SPSS/ESdEadulto_2023.sav
#   data/Adulto-ENSE-2011-12/MicrodatoAdultos.txt  (fwf)
#   data/Adultos_2017/MICRODAT.CA.txt              (fwf)

source("R/00_config.R")

message("\n=== Cargando microdatos ===")

# ── Función genérica para ficheros SPSS ────────────────────
cargar_sav <- function(ruta, anyo) {
  if (!file.exists(ruta)) stop("No se encontró: ", ruta)
  message("  Leyendo ", basename(ruta), " (", anyo, ")...")
  df <- haven::read_sav(ruta, encoding = "latin1") |>
    haven::zap_labels() |>
    haven::zap_formats() |>
    rename_with(tolower)
  message("  → ", nrow(df), " filas, ", ncol(df), " columnas")
  df
}

# ── Carga directa (sólo en modo interactivo, no en {targets}) ──────────────
# tar_source() carga este archivo para las definiciones de funciones.
# Las líneas de abajo sólo se ejecutan si DIR_DATA ya existe (modo interactivo).
if (exists("DIR_DATA")) {

# ── EESE 2009 ───────────────────────────────────────────────
eese2009_raw <- cargar_sav(
  file.path(DIR_DATA, "eese2009/SPSS/EESEADULTO_2009.sav"), 2009)

# ── EESE 2014 ───────────────────────────────────────────────
eese2014_raw <- cargar_sav(
  file.path(DIR_DATA, "eese2014/SPSS/EESEADULTO_2014.sav"), 2014)

# ── EESE 2020 ───────────────────────────────────────────────
eese2020_raw <- cargar_sav(
  file.path(DIR_DATA, "eese2020/SPSS/EESEadulto_2020.sav"), 2020)

# ── ESdE 2023 ───────────────────────────────────────────────
esde2023_raw <- cargar_sav(
  file.path(DIR_DATA, "esde2023/SPSS/ESdEadulto_2023.sav"), 2023)

# ── ENSE 2011 (formato ancho fijo) ─────────────────────────
cargar_ense2011 <- function() {
  ruta <- file.path(DIR_DATA, "Adulto-ENSE-2011-12/MicrodatoAdultos.txt")
  if (!file.exists(ruta)) stop("No se encontró: ", ruta)
  message("  Leyendo ENSE 2011 (fwf)...")

  # Posiciones verificadas contra DisenoAdultos.xls (CORREGIDAS):
  #   CCAA=1-2, SEXOa=14, EDADa=15-17, E1_1=28,
  #   FACTORADULTO=707-717 (11 dig, formato 5 enteros + 6 decimales sin punto),
  #   CLASE_PR=718 (clase social derivada — sustituto de estudios en 2011)
  #   Q90=415, Q91=416, Q92=417,
  #   Q96=421, Q97=422, Q98=423,
  #   Q99=424, Q100=425, Q101=426
  # NOTA: A10_I (nivel de estudios individual) NO está en el microdato de 2011.
  #       En v2 se recupera del fichero de hogar (A10_i) mediante join por IDENTHOGAR.
  # IDENTHOGAR (pos 3-10) y A7_2a (pos 12-13) son las claves del join con hogar.
  col_names <- c("ccaa","identhogar","a7_2a","sexoa","edada","e1_1",
                 "factoradulto","clase_pr",
                 "q90","q91","q92","q96","q97","q98","q99","q100","q101")
  col_start <- c(  1,    3,   12,   14,   15,   28,  707,  718, 415, 416, 417, 421, 422, 423, 424, 425, 426)
  col_end   <- c(  2,   10,   13,   14,   17,   28,  717,  718, 415, 416, 417, 421, 422, 423, 424, 425, 426)

  df <- readr::read_fwf(
    ruta,
    col_positions = readr::fwf_positions(col_start, col_end, col_names),
    col_types     = readr::cols(.default = "i",
                                identhogar   = "c",
                                factoradulto = "d"),
    na            = c("","NA","  ","   ","    ","           "))
  message("  → ", nrow(df), " filas")
  df
}
ense2011_raw <- cargar_ense2011()

# ── ENSE 2017 (formato ancho fijo) ─────────────────────────
cargar_ense2017 <- function() {
  ruta <- file.path(DIR_DATA, "Adultos_2017/MICRODAT.CA.txt")
  if (!file.exists(ruta)) stop("No se encontró: ", ruta)
  message("  Leyendo ENSE 2017 (fwf)...")

  # Posiciones verificadas contra diseño registro ENSE 2017:
  #   CCAA=1-2, SEXOa=13, EDADa=14-16, E1_1=28, NIVEST=37-38,
  #   CLASE_PR=578 (clase social derivada; verificado en MICRODAT.CA.txt)
  #   FACTORADULTO=567-577 (11 dig)
  #   Q95=382, Q96=383, Q97=384, Q100=387, Q101=388, Q102=389, Q103=390, Q104=391
  col_names <- c("ccaa","sexoa","edada","e1_1","nivest","clase_pr","factoradulto",
                 "q95","q96","q97","q100","q101","q102","q103","q104")
  col_start <- c(  1,   13,   14,   28,   37,  578,  567, 382, 383, 384, 387, 388, 389, 390, 391)
  col_end   <- c(  2,   13,   16,   28,   38,  578,  577, 382, 383, 384, 387, 388, 389, 390, 391)

  df <- readr::read_fwf(
    ruta,
    col_positions = readr::fwf_positions(col_start, col_end, col_names),
    col_types     = readr::cols(.default = "i",
                                factoradulto = "d"),
    na            = c("","NA","  ","   ","    ","           "))
  message("  → ", nrow(df), " filas")
  df
}
ense2017_raw <- cargar_ense2017()

message("\n✓ Todos los microdatos cargados")
} # fin if(exists(DIR_DATA))

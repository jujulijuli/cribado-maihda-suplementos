# ============================================================
# 02_preparar_datos_v4.R  —  Armonización MAIHDA × 6 olas
# ============================================================
# Proyecto: CRISEC (GN2026) — desigualdades en cribado de cáncer
#
# Cambios respecto a v2:
#   4. Rango de edad ampliado a 25-69 en todas las olas (antes: 50-69)
#      - Cubre la diana de citología (25-65) además de SOH/mama (50-69)
#      - El filtro por población diana se aplica en subconjunto_maihda()
#   5. Nueva variable edad_grupo_cito: 8 bandas de 5 años (25-29 … 60-65)
#      Alineada con edad_grupo donde se solapan (50-54, 55-59)
#   6. Nuevos estratos estrato_4_cito / estrato_5_cito (sin sexo_f, todas mujeres)
#   7. Nueva función subconjunto_maihda() para filtrar por población diana SNS
#
# Cambios respecto a v1:
#   1. Nivel de estudios → ISCED-2011 (5 niveles) en todas las olas
#      - EESE 2009: desde TESTUD 2009 (HH13), función .isced_desde_testud2009()
#      - ENSE 2011: desde A10_i del fichero de hogar (9 cat), .isced_desde_9cat()
#      - EESE 2014/2020, ENSE 2017, ESdE 2023: también escala 9 cat, .isced_desde_9cat()
#   2. ENSE 2011: merge con fichero de hogar para obtener nivel de estudios
#      real (P10 hogar) en vez de usar CLASE_PR como proxy
#   3. CLASE_PR → clase_3 (3 grupos SEE colapsados) en olas 2011–2023
#      EESE 2009: clase_3 = NA (ISCO-88, no derivable con esquema SEE español)
#
# Variables de estratificación MAIHDA (nivel 2):
#   sexo_f          : Hombre / Mujer
#   isced_f         : ISCED 0-1 / ISCED 2-3 / ISCED 4 / ISCED 5-6 / ISCED 7-8
#   isced_3         : Bajo (0-2) / Medio (3-4) / Alto (5-8)   [versión colapsada]
#   pais_nac_f      : España / Extranjero
#   edad_grupo      : 50-54 / 55-59 / 60-64 / 65-69   (SOH y mama, diana 50-69)
#   edad_grupo_cito : 25-29 / 30-34 / 35-39 / 40-44 / 45-49 /
#                     50-54 / 55-59 / 60-65             (cito, diana 25-65)
#   clase_3         : Alta (I-II) / Media (IIIa-IIIb-IVa) / Baja (IVb-V-VI)
#                     NA en EESE 2009
#
# Join ENSE 2011 hogar × adulto:
#   El fichero de hogar (ancho fijo .txt) tiene UNA FILA POR MIEMBRO del hogar.
#   El adulto seleccionado se identifica con A8_1_i = 1 (exactamente uno por hogar).
#   Clave de join: IDENTHOGAR (pos 3-10 en .txt) — coincide con IDENTHOGAR en adulto.
#   Variable objetivo: A10_i (pos 35-36) = nivel de estudios del adulto seleccionado.
#   Función leer_hogar_ense2011() lee el .txt y extrae sólo esa fila por hogar.
#
# ============================================================

source("R/00_config.R")

# ── Leer microdato de hogar ENSE 2011 (fichero ancho fijo .txt) ────────────
# Diseño de registro (CuestionarioHogar.xls):
#   Col   Pos  Campo
#   CCAA        1-2    Comunidad autónoma
#   IDENTHOGAR  3-10   ID hogar (Sección+Vivienda+Hogar)
#   SEXO_i     12     Sexo del miembro
#   EDAD_i     13-15  Edad
#   NORDEN_Ai  16-17  Número de orden adulto encuestable
#   A7_2a      27-28  Número de orden del adulto SELECCIONADO
#   A8_1_i     31     ¿Es este miembro el adulto seleccionado? (1=sí, 6=no)
#   NORINF     33-34  Orden del informante del cuestionario hogar
#   A10_i      35-36  Nivel de estudios del informante (escala 9 cat)
#   CLASE_PR    97     Clase social (SEE derivada de CNO-2011)
#
# NOTA: A8_1_i = 1 identifica exactamente al adulto seleccionado (1 por hogar).
#       Su A10_i es el nivel de estudios que usamos (no el del informante NORINF).
#       En la mayoría de hogares el informante coincide con el adulto seleccionado,
#       pero si no: tomamos el del miembro con A8_1_i=1.

leer_hogar_ense2011 <- function(ruta_txt) {
  message("  Leyendo microdato hogar ENSE 2011: ", basename(ruta_txt))

  # Posiciones (base-0, para substring()): inicio_r = pos_diseño - 1
  # substring() en R usa base-1 → inicio = pos_diseño, fin = pos_fin
  con <- file(ruta_txt, "r", encoding = "latin1")
  on.exit(close(con))

  registros <- list()
  i <- 0L
  while (TRUE) {
    linea <- readLines(con, n = 1L, warn = FALSE)
    if (length(linea) == 0L) break
    i <- i + 1L

    # Extraer campos (posiciones del diseño, base-1)
    identhogar <- trimws(substr(linea,  3, 10))
    a8_1_i     <- trimws(substr(linea, 31, 31))   # ¿Es el adulto seleccionado?
    a10_i      <- trimws(substr(linea, 35, 36))   # Nivel de estudios
    clase_pr_h <- trimws(substr(linea, 97, 97))   # Clase social

    # Sólo el adulto seleccionado
    if (a8_1_i == "1") {
      registros[[length(registros) + 1L]] <- list(
        identhogar   = identhogar,
        a10_i_hogar  = suppressWarnings(as.integer(a10_i)),
        clase_pr_h   = suppressWarnings(as.integer(clase_pr_h))
      )
    }
  }

  df <- dplyr::bind_rows(registros)
  message("  Registros hogar leídos (adulto seleccionado): ", nrow(df))
  df
}

# ── Utilitarios ─────────────────────────────────────────────
.peso_centrado <- function(w) w / mean(w, na.rm = TRUE)

# Grupos de edad para SOH y mamografía (población diana 50-69)
.grupos_edad <- function(x) {
  cut(as.integer(x),
      breaks = c(49, 54, 59, 64, 69),
      labels = c("50-54","55-59","60-64","65-69"),
      right  = TRUE) |>
    factor(levels = c("50-54","55-59","60-64","65-69"))
}

# Grupos de edad para citología (población diana mujeres 25-65)
# Bandas de 5 años, alineadas con edad_grupo donde se solapan (50-54, 55-59);
# el último grupo cierra en 65 (tope de la diana SNS).
.grupos_edad_cito <- function(x) {
  cut(as.integer(x),
      breaks = c(24, 29, 34, 39, 44, 49, 54, 59, 65),
      labels = c("25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-65"),
      right  = TRUE) |>
    factor(levels = c("25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-65"))
}

.ccaa_factor <- function(x) {
  factor(as.integer(x), levels = 1:19, labels = CCAA_LABELS)
}

# ── Recode ISCED-2011 desde CNED-2014 (~22 cat.) ────────────
# Correspondencia CNED-2014 → ISCED-2011:
#   ISCED 0  (educación temprana)       : CNED 0
#   ISCED 1  (primaria)                 : CNED 1  (códigos 10, 21-24 aprox.)
#   ISCED 2  (secundaria inferior)      : CNED 2  (32-38)
#   ISCED 3  (secundaria superior)      : CNED 3  (41-44, 51-55)
#   ISCED 4  (post-secundaria no sup.)  : CNED 4  (56-59)  — raro en España
#   ISCED 5  (ciclos formativos sup.)   : CNED 5  (61-65)
#   ISCED 6  (grado universitario)      : CNED 6  (71-74)
#   ISCED 7  (máster / posgrado)        : CNED 7  (81)
#   ISCED 8  (doctorado)                : CNED 8  (91)
#
# En la práctica española los códigos 2-dígito usados en microdatos son:
#   00/01/02 → sin estudios / analfabeto
#   10       → educación primaria incompleta (ISCED 1)
#   21-24    → educación primaria (ISCED 1)
#   32-35    → ESO / secundaria oblig. (ISCED 2)
#   38       → programas iniciación prof. (ISCED 2)
#   41       → bachillerato (ISCED 3)
#   51-52    → ciclos form. grado medio / equiv. (ISCED 3)
#   61-62    → ciclos form. grado superior (ISCED 5)
#   71-74    → títulos universitarios grado (ISCED 6)
#   81       → máster oficial (ISCED 7)
#   91       → doctorado (ISCED 8)

.isced_desde_cned <- function(x) {
  x <- as.integer(x)
  dplyr::case_when(
    x %in% c(0, 1, 2)                      ~ "ISCED 0-1",   # sin estudios / primaria incomp.
    x %in% c(10, 21, 22, 23, 24)           ~ "ISCED 0-1",   # primaria
    x %in% c(32, 33, 34, 35, 38)           ~ "ISCED 2",     # secundaria inferior (ESO)
    x %in% c(41, 42, 43, 44, 51, 52, 53,
              54, 55)                       ~ "ISCED 3",     # secundaria superior (Bach./CFGM)
    x %in% c(56, 57, 58, 59)               ~ "ISCED 4",     # post-secundaria no superior
    x %in% c(61, 62, 63, 64, 65)           ~ "ISCED 5",     # CFGS / técnico superior
    x %in% c(71, 72, 73, 74)               ~ "ISCED 6",     # grado universitario
    x %in% c(81)                           ~ "ISCED 7",     # máster
    x %in% c(91)                           ~ "ISCED 8",     # doctorado
    TRUE ~ NA_character_
  ) |>
    factor(levels = c("ISCED 0-1","ISCED 2","ISCED 3","ISCED 4",
                      "ISCED 5","ISCED 6","ISCED 7","ISCED 8"))
}

# Versión colapsada en 3 niveles (Bajo / Medio / Alto)
.isced_3 <- function(isced_f) {
  dplyr::case_when(
    isced_f %in% c("ISCED 0-1","ISCED 2")              ~ "Bajo",
    isced_f %in% c("ISCED 3","ISCED 4","ISCED 5")      ~ "Medio",
    isced_f %in% c("ISCED 6","ISCED 7","ISCED 8")      ~ "Alto",
    TRUE ~ NA_character_
  ) |> factor(levels = c("Bajo","Medio","Alto"))
}

# ── Recode ISCED-2011 desde escala 9-cat (EESE 2009 / ENSE 2011 hogar) ──
# Categorías:
#   01 = menor 10a (excluir/NA)
#   02 = no sabe leer/escribir          → ISCED 0-1
#   03 = primaria incompleta            → ISCED 0-1
#   04 = primaria completa              → ISCED 0-1
#   05 = secundaria 1ª etapa (ESO/EGB) → ISCED 2
#   06 = bachillerato                   → ISCED 3
#   07 = FP grado medio                 → ISCED 3
#   08 = FP grado superior              → ISCED 5
#   09 = universitarios o equiv.        → ISCED 6
#   98/99 = NS/NC                       → NA
.isced_desde_9cat <- function(x) {
  x <- as.integer(x)
  dplyr::case_when(
    x %in% c(2, 3, 4)  ~ "ISCED 0-1",
    x == 5              ~ "ISCED 2",
    x %in% c(6, 7)     ~ "ISCED 3",
    x == 8              ~ "ISCED 5",
    x == 9              ~ "ISCED 6",
    TRUE ~ NA_character_
  ) |>
    factor(levels = c("ISCED 0-1","ISCED 2","ISCED 3","ISCED 4",
                      "ISCED 5","ISCED 6","ISCED 7","ISCED 8"))
}

# ── Recode ISCED-2011 desde TESTUD 2009 (HH13) ──────────────
# EESE 2009 usa TESTUD con codificación diferente a 2011-2023:
#   01 = No sabe leer o escribir             → ISCED 0-1
#   02 = Primaria incompleta                 → ISCED 0-1
#   03 = Primaria completa                   → ISCED 0-1
#   04 = Secundaria 1ª etapa                 → ISCED 2
#   05 = Bachillerato                        → ISCED 3
#   06 = CFGM / equivalente                  → ISCED 3
#   07 = CFGS / equivalente                  → ISCED 5
#   08 = Universitarios 1º y 2º ciclo        → ISCED 6  (≡ universitarios en 2011-2023)
#   09 = Doctorado                           → ISCED 8
#   98/99 = No sabe / No contesta            → NA
# NOTA: 08→ISCED 6 (no 5) para que la categoría "Alto" (.isced_3) sea equivalente
#       a la de las olas 2011-2023, donde ISCED 6 = estudios universitarios.
.isced_desde_testud2009 <- function(x) {
  x <- as.integer(x)
  dplyr::case_when(
    x %in% c(1, 2, 3) ~ "ISCED 0-1",
    x == 4             ~ "ISCED 2",
    x %in% c(5, 6)    ~ "ISCED 3",
    x == 7             ~ "ISCED 5",
    x == 8             ~ "ISCED 6",
    x == 9             ~ "ISCED 8",
    TRUE ~ NA_character_
  ) |> factor(levels = c("ISCED 0-1","ISCED 2","ISCED 3","ISCED 4",
                          "ISCED 5","ISCED 6","ISCED 7","ISCED 8"))
}

# ── Recode CLASE_PR → clase_3 (SEE colapsado) ───────────────
# Esquema SEE (Sociedad Española de Epidemiología):
#   I    = directivos/profesionales superiores
#   II   = profesionales de apoyo técnico
#   IIIa = trabajadores no manuales cualificados
#   IIIb = pequeños propietarios sin asalariados
#   IVa  = supervisores/trabajadores manuales cualificados
#   IVb  = trabajadores manuales semicualificados
#   V+VI = trabajadores no cualificados
#
# Colapso en 3 grupos:
#   Alta  = I + II         (códigos 1, 2)
#   Media = IIIa + IIIb + IVa (códigos 3, 4, 5)
#   Baja  = IVb + V+VI     (códigos 6, 7)
.clase_3 <- function(x) {
  x <- as.integer(x)
  dplyr::case_when(
    x %in% c(1, 2)      ~ "Alta",
    x %in% c(3, 4, 5)   ~ "Media",
    x %in% c(6, 7)      ~ "Baja",
    TRUE ~ NA_character_
  ) |> factor(levels = c("Alta","Media","Baja"))
}

# ── EESE 2009 ───────────────────────────────────────────────
preparar_eese2009 <- function(df) {
  message("\n── Preparando EESE 2009 ──")
  df <- dplyr::rename_with(df, tolower)
  # Rango amplio 25-69: cubre diana SOH/mama (50-69) y cito (25-65)
  # El filtro por población diana se aplica en subconjunto_maihda()
  df <- df |> dplyr::filter(edad >= 25, edad <= 69)
  message("  n total 25-69: ", nrow(df))

  df <- df |> dplyr::mutate(
    peso_centrado   = .peso_centrado(factoradulto),
    sexo_f          = factor(as.integer(sexo), levels = c(1,2),
                             labels = c("Hombre","Mujer")),
    edad_grupo      = .grupos_edad(edad),
    edad_grupo_cito = .grupos_edad_cito(edad),
    ccaa_f          = .ccaa_factor(ccaa),

    # Origen: HH9_1 "País de nacimiento" (1 = España, 2 = Extranjero,
    # 8 = No sabe, 9 = No contesta → NA).
    # CORREGIDO en v4: antes se usaba IN2, que en la EESE 2009 es el
    # ingreso mensual neto del hogar, no el país de nacimiento.
    pais_nac_f = dplyr::case_when(
      as.character(hh9_1) == "1" ~ "España",
      as.character(hh9_1) == "2" ~ "Extranjero",
      TRUE ~ NA_character_) |>
      factor(levels = c("España","Extranjero")),

    # Nivel de estudios → ISCED desde HH13 (TESTUD 2009)
    isced_f   = .isced_desde_testud2009(hh13),
    isced_3   = .isced_3(isced_f),

    # Clase social: no disponible en EESE 2009 (ISCO-88, no CNO)
    clase_3   = factor(NA_character_, levels = c("Alta","Media","Baja")),

    # ── SOH ──────────────────────────────────────────────────────────────────────
    # PA16 (TSINO):  ¿Prueba de sangre oculta en heces alguna vez? 1=Sí, 6=No
    # PA17 (T5PERIO): periodo de la última prueba (solo si PA16=1)
    #   1=≤12 meses, 2=1-2 años, 3=2-3 años, 4=>3 años, 8=NS, 9=NC
    # soh_2a = 1 si realizada en los últimos 2 años (PA17 %in% c(1,2))
    soh_2a = dplyr::case_when(
      pa16 == 6                              ~ 0L,  # nunca → negativo
      pa16 == 1 & pa17 %in% c(1L, 2L)       ~ 1L,  # ≤2 años → positivo
      pa16 == 1 & pa17 %in% c(3L, 4L)       ~ 0L,  # >2 años → negativo
      pa16 %in% c(8L, 9L) | pa17 %in% c(8L, 9L) ~ NA_integer_,
      TRUE ~ NA_integer_),

    # ── Mamografía ────────────────────────────────────────────────────────────
    # PA10 (TSINO):  ¿Mamografía alguna vez? 1=Sí, 6=No
    # PA11 (T3PERIO): periodo de la última (solo si PA10=1)
    #   1=≤3 años, 2=>3 años, 8=NS, 9=NC
    # mama_2a = 1 si realizada en los últimos 3 años (T3PERIO=1)
    mama_algvez = dplyr::case_when(pa10==1~1L, pa10==6~0L, TRUE~NA_integer_),
    mama_2a = dplyr::case_when(
      mama_algvez == 1 & pa11 == 1  ~ 1L,  # ≤3 años → positivo
      mama_algvez == 0               ~ 0L,
      mama_algvez == 1 & pa11 == 2  ~ 0L,  # >3 años → negativo
      TRUE ~ NA_integer_),

    # ── Citología ─────────────────────────────────────────────────────────────
    # PA13 (TSINO):  ¿Citología vaginal alguna vez? 1=Sí, 6=No
    # PA14 (T4PERIO): periodo de la última (solo si PA13=1)
    #   1=≤3 años, 2=3-5 años, 3=>5 años, 8=NS, 9=NC
    # cito_3a = 1 si realizada en los últimos 3 años (PA14==1)
    cito_algvez = dplyr::case_when(pa13==1~1L, pa13==6~0L, TRUE~NA_integer_),
    cito_3a = dplyr::case_when(
      cito_algvez == 1 & pa14 == 1           ~ 1L,  # ≤3 años → positivo
      cito_algvez == 0                        ~ 0L,
      cito_algvez == 1 & pa14 %in% c(2L, 3L) ~ 0L,  # >3 años → negativo
      TRUE ~ NA_integer_),

    anyo = 2009L
  )

  message("  SOH_2a: n=", sum(df$soh_2a==1, na.rm=TRUE), "/",
          sum(!is.na(df$soh_2a)), " (",
          round(100*mean(df$soh_2a==1, na.rm=TRUE), 1), "%)")
  df
}

# ── ENSE 2011 ───────────────────────────────────────────────
# df_hogar: data.frame producido por leer_hogar_ense2011()
#   Variables: identhogar, a10_i_hogar, clase_pr_h
#   Una fila por hogar (el adulto seleccionado, A8_1_i=1)
#
# Join por IDENTHOGAR (único entre adulto y hogar):
#   El microdato de adultos tiene 'identhogar' (minúscula tras rename_with(tolower))
#   El df_hogar tiene 'identhogar' (ya en minúscula desde leer_hogar_ense2011)
preparar_ense2011 <- function(df, df_hogar = NULL) {
  message("\n── Preparando ENSE 2011 ──")
  df <- dplyr::rename_with(df, tolower)
  # Rango amplio 25-69: cubre diana SOH/mama (50-69) y cito (25-65)
  df <- df |> dplyr::filter(edada >= 25, edada <= 69)
  message("  n total 25-69 (antes join hogar): ", nrow(df))

  # ── Merge con fichero de hogar ──
  if (!is.null(df_hogar)) {
    df <- df |>
      dplyr::left_join(df_hogar, by = "identhogar")
    n_matched <- sum(!is.na(df$a10_i_hogar))
    message("  Registros con A10_i recuperado del hogar: ", n_matched,
            " / ", nrow(df), " (",
            round(100 * n_matched / nrow(df), 1), "%)")
  } else {
    message("  AVISO: df_hogar no proporcionado — use leer_hogar_ense2011() primero")
    message("    df_hogar <- leer_hogar_ense2011(file.path(DIR_DATA,")
    message("      'Hogar-ENSE-2011-12', 'MICRODATO ANONIMIZADO PUBLICACION HOGAR.txt'))")
    df$a10_i_hogar <- NA_integer_
    df$clase_pr_h  <- NA_integer_
  }

  df <- df |> dplyr::mutate(
    peso_centrado   = .peso_centrado(factoradulto),
    sexo_f          = factor(as.integer(sexoa), levels = c(1,2),
                             labels = c("Hombre","Mujer")),
    edad            = as.integer(edada),          # columna estandarizada (edada en ENS/EESE post-2009)
    edad_grupo      = .grupos_edad(edada),
    edad_grupo_cito = .grupos_edad_cito(edada),
    ccaa_f          = .ccaa_factor(ccaa),

    # Origen
    pais_nac_f = dplyr::case_when(
      e1_1 == 1                  ~ "España",
      !is.na(e1_1) & e1_1 != 1  ~ "Extranjero",
      TRUE ~ NA_character_) |>
      factor(levels = c("España","Extranjero")),

    # Nivel de estudios → ISCED desde A10_i del hogar (9 categorías)
    # (A10_i del miembro con A8_1_i=1 = adulto seleccionado)
    isced_f = .isced_desde_9cat(a10_i_hogar),
    isced_3 = .isced_3(isced_f),

    # Clase social: preferir clase_pr del microdato adulto (si disponible);
    # si no, usar clase_pr_h del fichero hogar (idéntico en la mayoría de casos)
    clase_3 = dplyr::coalesce(.clase_3(clase_pr), .clase_3(clase_pr_h)),

    # Variables SPSS leídas como character → convertir a integer
    dplyr::across(dplyr::matches("^q\\d+$"), ~ suppressWarnings(as.integer(.x))),

    # SOH — Q90=alguna vez (1=Sí, 6=No), Q91=periodo (1=≤12m, 2=1-2a, 3=2-3a, 4=>3a)
    soh_2a = dplyr::case_when(
      q90 == 6                    ~ 0L,
      q90 == 1 & q91 %in% c(1L,2L) ~ 1L,
      q90 == 1 & q91 %in% c(3L,4L) ~ 0L,
      q90 %in% c(8L,9L) | q91 %in% c(8L,9L) ~ NA_integer_,
      TRUE ~ NA_integer_),

    # Mamografía — Q96=alguna vez (1=Sí, 6=No), Q97=periodo (1=≤12m, 2=1-2a, 3=2-3a, 4=>3a)
    # Sin filtro por razón (q98) para mantener criterio homogéneo con el resto de olas
    mama_algvez = dplyr::case_when(q96==1~1L, q96==6~0L, TRUE~NA_integer_),
    mama_2a = dplyr::case_when(
      mama_algvez == 0                          ~ 0L,
      mama_algvez == 1 & q97 %in% c(1L,2L)     ~ 1L,
      mama_algvez == 1 & q97 %in% c(3L,4L)     ~ 0L,
      TRUE ~ NA_integer_),

    # Citología — Q99=alguna vez (1=Sí, 6=No), Q100=periodo (1=≤3a, 2=3-5a, 3=>5a)
    # Sin filtro por razón (q101); periodo 3-cat diferente a 2014/2020 (5-cat)
    cito_algvez = dplyr::case_when(q99==1~1L, q99==6~0L, TRUE~NA_integer_),
    cito_3a = dplyr::case_when(
      cito_algvez == 0          ~ 0L,
      cito_algvez == 1 & q100 == 1L ~ 1L,
      cito_algvez == 1 & q100 %in% c(2L,3L) ~ 0L,
      TRUE ~ NA_integer_),

    anyo = 2011L
  )

  message("  SOH_2a: n=", sum(df$soh_2a==1, na.rm=TRUE), "/",
          sum(!is.na(df$soh_2a)), " (",
          round(100*mean(df$soh_2a==1, na.rm=TRUE), 1), "%)")
  df
}

# ── EESE 2014 ───────────────────────────────────────────────
preparar_eese2014 <- function(df) {
  message("\n── Preparando EESE 2014 ──")
  df <- dplyr::rename_with(df, tolower)
  df <- df |> dplyr::filter(edada >= 25, edada <= 69)
  message("  n total 25-69: ", nrow(df))

  df <- df |> dplyr::mutate(
    peso_centrado   = .peso_centrado(factoradulto),
    sexo_f          = factor(as.integer(sexoa), levels = c(1,2),
                             labels = c("Hombre","Mujer")),
    edad            = as.integer(edada),          # columna estandarizada
    edad_grupo      = .grupos_edad(edada),
    edad_grupo_cito = .grupos_edad_cito(edada),
    ccaa_f          = .ccaa_factor(ccaa),

    pais_nac_f = dplyr::case_when(
      e1_1 == 1                  ~ "España",
      !is.na(e1_1) & e1_1 != 1  ~ "Extranjero",
      TRUE ~ NA_character_) |>
      factor(levels = c("España","Extranjero")),

    # Nivel de estudios → ISCED desde escala 9 categorías (ESTUDIOS)
    isced_f = .isced_desde_9cat(estudios),
    isced_3 = .isced_3(isced_f),

    # Clase social
    clase_3 = .clase_3(clase_pr),

    # SOH — Q95=alguna vez (1=Sí, 2=No), Q96=periodo (1=≤12m, 2=1-2a, 3=2-3a, 4=3-5a, 5=>5a)
    soh_2a = dplyr::case_when(
      q95 == 2                          ~ 0L,
      q95 == 1 & q96 %in% c(1L,2L)     ~ 1L,
      q95 == 1 & q96 %in% c(3L,4L,5L)  ~ 0L,
      q95 %in% c(8L,9L) | q96 %in% c(8L,9L) ~ NA_integer_,
      TRUE ~ NA_integer_),

    # Mamografía — Q100=alguna vez (1=Sí, 2=No), Q101=periodo (1=≤12m, 2=1-2a, 3=2-3a, 4=3-5a, 5=>5a)
    mama_algvez = dplyr::case_when(q100==1~1L, q100==2~0L, TRUE~NA_integer_),
    mama_2a = dplyr::case_when(
      mama_algvez == 0                          ~ 0L,
      mama_algvez == 1 & q101 %in% c(1L,2L)    ~ 1L,
      mama_algvez == 1 & q101 %in% c(3L,4L,5L) ~ 0L,
      TRUE ~ NA_integer_),

    # Citología — Q103=alguna vez (1=Sí, 2=No), Q104=periodo (1=≤12m, 2=1-3a, 3=3-5a, 4=>5a)
    cito_algvez = dplyr::case_when(q103==1~1L, q103==2~0L, TRUE~NA_integer_),
    cito_3a = dplyr::case_when(
      cito_algvez == 0                          ~ 0L,
      cito_algvez == 1 & q104 %in% c(1L,2L)    ~ 1L,
      cito_algvez == 1 & q104 %in% c(3L,4L)    ~ 0L,
      TRUE ~ NA_integer_),

    anyo = 2014L
  )

  message("  SOH_2a: n=", sum(df$soh_2a==1, na.rm=TRUE), "/",
          sum(!is.na(df$soh_2a)), " (",
          round(100*mean(df$soh_2a==1, na.rm=TRUE), 1), "%)")
  df
}

# ── ENSE 2017 ───────────────────────────────────────────────
preparar_ense2017 <- function(df) {
  message("\n── Preparando ENSE 2017 ──")
  df <- dplyr::rename_with(df, tolower)
  df <- df |> dplyr::filter(edada >= 25, edada <= 69)
  message("  n total 25-69: ", nrow(df))

  df <- df |> dplyr::mutate(
    peso_centrado   = .peso_centrado(factoradulto),
    sexo_f          = factor(as.integer(sexoa), levels = c(1,2),
                             labels = c("Hombre","Mujer")),
    edad            = as.integer(edada),          # columna estandarizada
    edad_grupo      = .grupos_edad(edada),
    edad_grupo_cito = .grupos_edad_cito(edada),
    ccaa_f        = .ccaa_factor(ccaa),

    pais_nac_f = dplyr::case_when(
      e1_1 == 1                  ~ "España",
      !is.na(e1_1) & e1_1 != 1  ~ "Extranjero",
      TRUE ~ NA_character_) |>
      factor(levels = c("España","Extranjero")),

    # Nivel de estudios → ISCED desde escala 9 categorías (NIVEST)
    isced_f = .isced_desde_9cat(nivest),
    isced_3 = .isced_3(isced_f),

    # Clase social
    clase_3 = .clase_3(clase_pr),

    # SOH — Q95=alguna vez (1=Sí, 2=No), Q96=periodo (1=≤12m, 2=1-2a, 3=2-3a, 4=3-5a, 5=>5a)
    # Sin filtro por razón (q97) para mantener criterio homogéneo con el resto de olas
    soh_2a = dplyr::case_when(
      q95 == 2                        ~ 0L,
      q95 == 1 & q96 %in% c(1L,2L)   ~ 1L,
      q95 == 1 & q96 %in% c(3L,4L,5L) ~ 0L,
      q95 %in% c(8L,9L) | q96 %in% c(8L,9L) ~ NA_integer_,
      TRUE ~ NA_integer_),

    # Mamografía — Q100=alguna vez (1=Sí, 2=No), Q101=periodo (1=≤12m, 2=1-2a, 3=2-3a, 4=≥3a)
    # Sin filtro por razón (q102) para mantener criterio homogéneo con el resto de olas
    mama_algvez = dplyr::case_when(q100==1~1L, q100==2~0L, TRUE~NA_integer_),
    mama_2a = dplyr::case_when(
      mama_algvez == 0                       ~ 0L,
      mama_algvez == 1 & q101 %in% c(1L,2L) ~ 1L,
      mama_algvez == 1 & q101 %in% c(3L,4L) ~ 0L,
      TRUE ~ NA_integer_),

    # Citología — Q103=alguna vez (1=Sí, 2=No), Q104=periodo (1=≤12m, 2=1-2a, 3=2-3a, 4=3-5a, 5=>5a)
    cito_algvez = dplyr::case_when(q103==1~1L, q103==2~0L, TRUE~NA_integer_),
    cito_3a = dplyr::case_when(
      cito_algvez == 0                          ~ 0L,
      cito_algvez == 1 & q104 %in% c(1L,2L,3L) ~ 1L,
      cito_algvez == 1 & q104 %in% c(4L,5L)    ~ 0L,
      TRUE ~ NA_integer_),

    anyo = 2017L
  )

  message("  SOH_2a: n=", sum(df$soh_2a==1, na.rm=TRUE), "/",
          sum(!is.na(df$soh_2a)), " (",
          round(100*mean(df$soh_2a==1, na.rm=TRUE), 1), "%)")
  df
}

# ── EESE 2020 ───────────────────────────────────────────────
preparar_eese2020 <- function(df) {
  message("\n── Preparando EESE 2020 ──")
  df <- dplyr::rename_with(df, tolower)
  df <- df |> dplyr::filter(edada >= 25, edada <= 69)
  message("  n total 25-69: ", nrow(df))

  df <- df |> dplyr::mutate(
    peso_centrado   = .peso_centrado(factoradulto),
    sexo_f          = factor(as.integer(sexoa), levels = c(1,2),
                             labels = c("Hombre","Mujer")),
    edad            = as.integer(edada),          # columna estandarizada
    edad_grupo      = .grupos_edad(edada),
    edad_grupo_cito = .grupos_edad_cito(edada),
    ccaa_f          = .ccaa_factor(ccaa),

    pais_nac_f = dplyr::case_when(
      e1_1 == 1                  ~ "España",
      !is.na(e1_1) & e1_1 != 1  ~ "Extranjero",
      TRUE ~ NA_character_) |>
      factor(levels = c("España","Extranjero")),

    # Nivel de estudios → ISCED desde escala 9 categorías (ESTUDIOS)
    isced_f = .isced_desde_9cat(estudios),
    isced_3 = .isced_3(isced_f),

    # Clase social
    clase_3 = .clase_3(clase_pr),

    # SOH — Q95=alguna vez (1=Sí, 2=No), Q96=periodo (1=≤12m, 2=1-2a, 3=2-3a, 4=3-5a, 5=>5a)
    soh_2a = dplyr::case_when(
      q95 == 2                          ~ 0L,
      q95 == 1 & q96 %in% c(1L,2L)     ~ 1L,
      q95 == 1 & q96 %in% c(3L,4L,5L)  ~ 0L,
      q95 %in% c(8L,9L) | q96 %in% c(8L,9L) ~ NA_integer_,
      TRUE ~ NA_integer_),

    # Mamografía — Q100=alguna vez (1=Sí, 2=No), Q101=periodo (1=≤12m, 2=1-2a, 3=2-3a, 4=3-5a, 5=>5a)
    mama_algvez = dplyr::case_when(q100==1~1L, q100==2~0L, TRUE~NA_integer_),
    mama_2a = dplyr::case_when(
      mama_algvez == 0                          ~ 0L,
      mama_algvez == 1 & q101 %in% c(1L,2L)    ~ 1L,
      mama_algvez == 1 & q101 %in% c(3L,4L,5L) ~ 0L,
      TRUE ~ NA_integer_),

    # Citología — Q103=alguna vez (1=Sí, 2=No), Q104=periodo (1=≤12m, 2=1-3a, 3=3-5a, 4=>5a)
    cito_algvez = dplyr::case_when(q103==1~1L, q103==2~0L, TRUE~NA_integer_),
    cito_3a = dplyr::case_when(
      cito_algvez == 0                          ~ 0L,
      cito_algvez == 1 & q104 %in% c(1L,2L)    ~ 1L,
      cito_algvez == 1 & q104 %in% c(3L,4L)    ~ 0L,
      TRUE ~ NA_integer_),

    anyo = 2020L
  )

  message("  SOH_2a: n=", sum(df$soh_2a==1, na.rm=TRUE), "/",
          sum(!is.na(df$soh_2a)), " (",
          round(100*mean(df$soh_2a==1, na.rm=TRUE), 1), "%)")
  df
}

# ── ESdE 2023 ───────────────────────────────────────────────
preparar_esde2023 <- function(df) {
  message("\n── Preparando ESdE 2023 ──")
  df <- dplyr::rename_with(df, tolower)
  df <- df |> dplyr::filter(edada >= 25, edada <= 69)
  message("  n total 25-69: ", nrow(df))

  df <- df |> dplyr::mutate(
    peso_centrado   = .peso_centrado(factoradulto),
    sexo_f          = factor(as.integer(sexoa), levels = c(1,2),
                             labels = c("Hombre","Mujer")),
    edad            = as.integer(edada),          # columna estandarizada
    edad_grupo      = .grupos_edad(edada),
    edad_grupo_cito = .grupos_edad_cito(edada),
    ccaa_f          = .ccaa_factor(ccaa),

    # Origen: A1a (1=España, 2+=extranjero)
    pais_nac_f = dplyr::case_when(
      a1a == 1                   ~ "España",
      !is.na(a1a) & a1a != 1    ~ "Extranjero",
      TRUE ~ NA_character_) |>
      factor(levels = c("España","Extranjero")),

    # Nivel de estudios → ISCED desde escala 9 categorías (NIVEST)
    isced_f = .isced_desde_9cat(nivest),
    isced_3 = .isced_3(isced_f),

    # Clase social
    clase_3 = .clase_3(clase_pr),

    # SOH
    soh_2a = dplyr::case_when(
      l10 == 1 & l11 %in% c(1,2)   ~ 1L,
      l10 == 2                      ~ 0L,
      l10 == 1 & l11 %in% c(3,4,5) ~ 0L,
      TRUE ~ NA_integer_),

    # Mamografía
    mama_algvez = dplyr::case_when(l14==1~1L, l14==2~0L, TRUE~NA_integer_),
    mama_2a = dplyr::case_when(
      mama_algvez==1 & l15 %in% c(1,2) ~ 1L,
      mama_algvez==0                    ~ 0L,
      mama_algvez==1 & l15 %in% c(3,4) ~ 0L,
      TRUE ~ NA_integer_),

    # Citología
    cito_algvez = dplyr::case_when(l16==1~1L, l16==2~0L, TRUE~NA_integer_),
    cito_3a = dplyr::case_when(
      cito_algvez==1 & l17 %in% c(1,2,3) ~ 1L,
      cito_algvez==0                      ~ 0L,
      cito_algvez==1 & l17 %in% c(4,5)   ~ 0L,
      TRUE ~ NA_integer_),

    anyo = 2023L
  )

  message("  SOH_2a: n=", sum(df$soh_2a==1, na.rm=TRUE), "/",
          sum(!is.na(df$soh_2a)), " (",
          round(100*mean(df$soh_2a==1, na.rm=TRUE), 1), "%)")
  df
}

# ── Estratos MAIHDA ─────────────────────────────────────────
# Dos versiones base (SOH y mamografía, ambos sexos/solo mujeres, 50-69):
#   estrato_4  : sexo × isced_3 × pais_nac × edad_grupo        (4 dim., 6 olas)
#   estrato_5  : sexo × isced_3 × pais_nac × edad_grupo × clase_3 (5 dim., 2011-2023)
#
# Para citología se usan estratos paralelos con edad_grupo_cito (25-65):
#   estrato_4_cito : isced_3 × pais_nac × edad_grupo_cito        (solo mujeres)
#   estrato_5_cito : isced_3 × pais_nac × edad_grupo_cito × clase_3
#
# NOTA sobre población diana:
#   - soh_2a  : hombres y mujeres 50-69 → estratos con sexo_f incluido
#   - mama_2a : solo mujeres 50-69      → sexo_f constante (Mujer); se incluye
#               para mantener estructura de estratos compatible con soh_2a
#   - cito_3a : solo mujeres 25-65      → edad_grupo_cito; sexo_f omitido
#               (todos son Mujer, añadirlo no aporta varianza)
agregar_estratos <- function(df) {
  df |> dplyr::mutate(
    # ── Estratos SOH / mamografía (edad_grupo: 50-69 en bandas de 5) ──
    estrato_4 = paste(sexo_f, isced_3, pais_nac_f, edad_grupo,
                      sep = "_") |> factor(),
    estrato_5 = dplyr::if_else(
      !is.na(clase_3),
      paste(sexo_f, isced_3, pais_nac_f, edad_grupo, clase_3, sep = "_"),
      NA_character_
    ) |> factor(),

    # ── Estratos citología (solo mujeres 25-65; sexo_f omitido) ──
    estrato_4_cito = paste(isced_3, pais_nac_f, edad_grupo_cito,
                           sep = "_") |> factor(),
    estrato_5_cito = dplyr::if_else(
      !is.na(clase_3),
      paste(isced_3, pais_nac_f, edad_grupo_cito, clase_3, sep = "_"),
      NA_character_
    ) |> factor()
  )
}

# ── Variables comunes a seleccionar en el bind_rows final ───
VARS_COMUNES <- c(
  "anyo", "ccaa_f", "peso_centrado",
  "sexo_f", "edad", "edad_grupo", "edad_grupo_cito", "pais_nac_f",
  "isced_f", "isced_3", "clase_3",
  "estrato_4", "estrato_5",
  "estrato_4_cito", "estrato_5_cito",
  "soh_2a", "mama_algvez", "mama_2a", "cito_algvez", "cito_3a"
)

# ── Subconjuntos para MAIHDA por variable de resultado ──────
# Aplicar sobre OLAS antes de correr el análisis MAIHDA:
#   olas_soh  <- subconjunto_maihda(OLAS, "soh_2a")   # todos, 50-69
#   olas_mama <- subconjunto_maihda(OLAS, "mama_2a")  # mujeres, 50-69
#   olas_cito <- subconjunto_maihda(OLAS, "cito_3a")  # mujeres, 25-65
#
# Los NA en edad_grupo / edad_grupo_cito actúan como filtros implícitos:
# fuera del rango correcto el valor es NA → !is.na() excluye esas filas.
subconjunto_maihda <- function(OLAS, variable) {
  switch(variable,
    "soh_2a"  = OLAS |>
                  dplyr::filter(!is.na(edad_grupo)),
    "mama_2a" = OLAS |>
                  dplyr::filter(sexo_f == "Mujer", !is.na(edad_grupo)),
    "cito_3a" = OLAS |>
                  dplyr::filter(sexo_f == "Mujer", !is.na(edad_grupo_cito)),
    stop("Variable desconocida: ", variable)
  )
}

# ── Ejecución ───────────────────────────────────────────────
# Uso:
#   - Carga los raw objects y, para ENSE 2011, el fichero de hogar:
#       ense2011_hogar_raw <- haven::read_sav("ruta/al/hogar2011.sav")
#   - Llama a las funciones:
#       eese2009 <- preparar_eese2009(eese2009_raw) |> agregar_estratos()
#       ense2011 <- preparar_ense2011(ense2011_raw, ense2011_hogar_raw) |> agregar_estratos()
#       ...etc
#   - Une todo:
#       OLAS <- dplyr::bind_rows(
#         lapply(list(eese2009, ense2011, eese2014, ense2017, eese2020, esde2023),
#                dplyr::select, dplyr::any_of(VARS_COMUNES))
#       )

if (!exists("eese2009_raw")) {
  message("  [02_preparar_datos_v3.R] Cargado como librería de funciones — OK")
  message("  Para ENSE 2011 con merge hogar, proporcionar df_hogar como segundo argumento:")
  message("    ense2011_hogar_raw <- haven::read_sav('<ruta>/ense2011_hogar.sav')")
  message("    ense2011 <- preparar_ense2011(ense2011_raw, ense2011_hogar_raw)")
} else {
  # ── Carga hogar ENSE 2011 si existe ──
  # Fichero de ancho fijo: "MICRODATO ANONIMIZADO PUBLICACION HOGAR.txt"
  ruta_hogar_2011 <- file.path(DIR_DATA, "Hogar-ENSE-2011-12",
                                "MICRODATO ANONIMIZADO PUBLICACION HOGAR.txt")
  if (file.exists(ruta_hogar_2011)) {
    ense2011_hogar_raw <- leer_hogar_ense2011(ruta_hogar_2011)
  } else {
    message("  AVISO: no se encontró fichero de hogar ENSE 2011 en:")
    message("    ", ruta_hogar_2011)
    message("  isced_f será NA para ENSE 2011.")
    ense2011_hogar_raw <- NULL
  }

  eese2009 <- preparar_eese2009(eese2009_raw)  |> agregar_estratos()
  ense2011 <- preparar_ense2011(ense2011_raw, ense2011_hogar_raw) |> agregar_estratos()
  eese2014 <- preparar_eese2014(eese2014_raw)  |> agregar_estratos()
  ense2017 <- preparar_ense2017(ense2017_raw)  |> agregar_estratos()
  eese2020 <- preparar_eese2020(eese2020_raw)  |> agregar_estratos()
  esde2023 <- preparar_esde2023(esde2023_raw)  |> agregar_estratos()

  OLAS <- dplyr::bind_rows(
    lapply(list(eese2009, ense2011, eese2014, ense2017, eese2020, esde2023),
           dplyr::select, dplyr::any_of(VARS_COMUNES))
  ) |>
    dplyr::mutate(
      anyo_f    = factor(anyo),
      encuesta  = dplyr::case_when(
        anyo %in% c(2011, 2017) ~ paste0("ENSE ", anyo),
        anyo == 2023            ~ paste0("ESdE ", anyo),
        TRUE                    ~ paste0("EESE ", anyo))
    )

  # ── Resumen de distribución ISCED por ola ──
  message("\n── Distribución ISCED por ola (%) ──")
  OLAS |>
    dplyr::filter(!is.na(isced_3)) |>
    dplyr::group_by(anyo, isced_3) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    dplyr::group_by(anyo) |>
    dplyr::mutate(pct = round(100 * n / sum(n), 1)) |>
    tidyr::pivot_wider(names_from = isced_3, values_from = c(n, pct)) |>
    print()

  # ── Resumen clase social por ola (2011-2023) ──
  message("\n── Distribución clase social por ola (%) ──")
  OLAS |>
    dplyr::filter(!is.na(clase_3)) |>
    dplyr::group_by(anyo, clase_3) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    dplyr::group_by(anyo) |>
    dplyr::mutate(pct = round(100 * n / sum(n), 1)) |>
    tidyr::pivot_wider(names_from = clase_3, values_from = c(n, pct)) |>
    print()

  # ── Número de estratos MAIHDA por ola ──
  message("\n── N estratos por ola ──")
  OLAS |>
    dplyr::group_by(anyo) |>
    dplyr::summarise(
      n_total    = dplyr::n(),
      n_est4     = dplyr::n_distinct(estrato_4, na.rm = TRUE),
      n_est5     = dplyr::n_distinct(estrato_5, na.rm = TRUE),
      .groups = "drop"
    ) |> print()

  saveRDS(OLAS, file.path(DIR_OUTPUT, "olas_preparadas_v3.rds"))
  message("\n✓ Guardado output/olas_preparadas_v3.rds")
}

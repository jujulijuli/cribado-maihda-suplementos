# ============================================================
# 00_config.R  —  Configuración global del proyecto CCR
# ============================================================
# Rutas relativas al directorio del proyecto (ccr-informe/)
# Los datos originales están en ../data/

library(tidyverse)
library(survey)
library(haven)
library(readr)
library(lme4)
library(pROC)

# Prioridad dplyr
conflicted::conflicts_prefer(dplyr::filter, dplyr::select, dplyr::lag)

# ── Rutas ──────────────────────────────────────────────────
# DIR_DATA: ruta a los microdatos originales
# Se prueba en orden: rutas candidatas conocidas, luego fallback relativo.
# Para añadir una máquina nueva, añadir una línea a .candidates.
.candidates <- c(
  # Ruta local (opcional)
  # Añadir aquí la ruta local a los microdatos, p. ej. "~/datos/encuestas_salud",
  # Ruta relativa clásica (wd = ccr-informe/)
  "../data",
  # fallback here::here
  if (requireNamespace("here", quietly = TRUE)) here::here("../data") else character(0)
)
DIR_DATA <- normalizePath(
  Filter(function(p) dir.exists(normalizePath(p, mustWork=FALSE)),
         .candidates)[1],
  mustWork = FALSE)
if (is.na(DIR_DATA) || !dir.exists(DIR_DATA))
  warning("DIR_DATA no encontrado. Ajusta .candidates en 00_config.R. Valor actual: ", DIR_DATA)
rm(.candidates)

DIR_OUTPUT  <- "output"
DIR_FIGS    <- "output/figuras"
DIR_TABLAS  <- "output/tablas"

# ── CCAA etiquetas (orden INE: 1-19) ───────────────────────
CCAA_LABELS <- c(
  "Andalucía","Aragón","Asturias","Baleares","Canarias",
  "Cantabria","Castilla y León","Castilla-La Mancha","Cataluña",
  "C. Valenciana","Extremadura","Galicia","Madrid","Murcia",
  "Navarra","País Vasco","La Rioja","Ceuta","Melilla"
)

# ── Dimensiones MAIHDA ─────────────────────────────────────
DIMS_3 <- c("sexo_f","isced_3","pais_nac_f","edad_grupo")

# ── % inmigración nacional por año (datos EPA/INE) ─────────
# 2011 y 2017 interpolados linealmente
MEDIA_NAC <- c("2009" = 3.4, "2014" = 9.2, "2020" = 31.9, "2023" = 38.7)
MEDIA_NAC["2011"] <- MEDIA_NAC["2009"] +
  (MEDIA_NAC["2014"] - MEDIA_NAC["2009"]) * (2011 - 2009) / (2014 - 2009)
MEDIA_NAC["2017"] <- MEDIA_NAC["2014"] +
  (MEDIA_NAC["2020"] - MEDIA_NAC["2014"]) * (2017 - 2014) / (2020 - 2014)

# ── Paleta de colores ───────────────────────────────────────
PAL_OLAS   <- c("#1b6ca8","#4a9fd4","#2e8b57","#a3c95a","#e07b39","#c0392b")
PAL_SEXO   <- c("Hombre" = "#1b6ca8", "Mujer" = "#c0392b")
PAL_EDUC   <- c("Bajo" = "#d9534f", "Medio" = "#f0ad4e", "Alto" = "#5cb85c")
PAL_NAC    <- c("España" = "#1b6ca8", "Extranjero" = "#e07b39")

# ============================================================
# 06_tabla1_dataset_v5.R — Tabla 1 (características por ola) y prevalencias
# a partir de olas_preparadas_v5.rds — 7 OLAS (2009–2025)
# ============================================================
# Población elegible = la del modelo MAIHDA: CRC 50–69 ambos sexos; mama mujeres
# 50–69; cervical mujeres 25–65; con resultado no ausente. Porcentajes ponderados
# (peso centrado), media de edad ponderada; N y N positivos sin ponderar.
#
# Cambios respecto a 06_tabla1_dataset.R:
#   - Lee olas_preparadas_v5.rds (7 olas, incluye 2025)
#   - enc añade 2025 = "ESdE 2025"
#   - Salidas: output/tabla1_v5.csv, output/tabla_prevalencias_v5.csv
#              output/analysis_dataset_v5.csv
# ============================================================
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(readr) })
DIR_OUTPUT <- "output"
OLAS <- readRDS(file.path(DIR_OUTPUT, "olas_preparadas_v5.rds"))

pob <- list(
  CRC      = OLAS |> filter(!is.na(edad_grupo), !is.na(soh_2a)) |> mutate(y = soh_2a, g = edad_grupo),
  Breast   = OLAS |> filter(sexo_f == "Mujer", !is.na(edad_grupo), !is.na(mama_2a)) |> mutate(y = mama_2a, g = edad_grupo),
  Cervical = OLAS |> filter(sexo_f == "Mujer", !is.na(edad_grupo_cito), edad <= 65, !is.na(cito_3a)) |> mutate(y = cito_3a, g = edad_grupo_cito))

wpct <- function(x, w, lv) { ok <- !is.na(x); 100 * sapply(lv, function(l) sum(w[ok & x == l]) / sum(w[ok])) }
fmt  <- function(x) sub("\\.0$", "", sprintf("%.1f", x))

tab1 <- bind_rows(lapply(names(pob), function(p) {
  d <- pob[[p]]
  bind_rows(lapply(sort(unique(d$anyo)), function(yr) {
    s <- d[d$anyo == yr, ]; w <- s$peso_centrado
    rows <- list(c("N", nrow(s)))
    if (p == "CRC") { v <- wpct(s$sexo_f, w, c("Hombre", "Mujer")); rows <- c(rows, list(c("    Male", paste0(fmt(v[1]), "%")), c("    Female", paste0(fmt(v[2]), "%")))) }
    rows <- c(rows, list(c("Mean age (years)", fmt(weighted.mean(s$edad, w)))))
    lv <- levels(droplevels(s$g)); v <- wpct(as.character(s$g), w, lv)
    rows <- c(rows, Map(function(l, x) c(paste0("    ", l), paste0(fmt(x), "%")), lv, v))
    ed <- c(Alto = "    High (ISCED 6-8)", Bajo = "    Low (ISCED 0-2)", Medio = "    Medium (ISCED 3,5)")
    v <- wpct(as.character(s$isced_3), w, names(ed))
    rows <- c(rows, Map(function(l, x) c(l, paste0(fmt(x), "%")), ed, v))
    v <- wpct(as.character(s$pais_nac_f), w, c("España", "Extranjero"))
    rows <- c(rows, list(c("    Spain-born", paste0(fmt(v[1]), "%")), c("    Foreign-born", paste0(fmt(v[2]), "%")),
      c("N territorial units (17 regions + Ceuta + Melilla)", length(unique(s$ccaa_f))),
      c("N positive (unweighted)", sum(s$y == 1)),
      c("Crude prevalence, % (unweighted)", fmt(100 * mean(s$y))),
      c("Weighted prevalence, % (survey-weighted)", fmt(100 * weighted.mean(s$y, w)))))
    tibble(Screening = p, Variable = sapply(rows, `[`, 1), Wave = as.character(yr), value = sapply(rows, `[`, 2), ord = seq_along(rows))
  }))
})) |>
  pivot_wider(names_from = Wave, values_from = value) |> arrange(factor(Screening, levels = names(pob)), ord) |> select(-ord)
write_csv(tab1, file.path(DIR_OUTPUT, "tabla1_v5.csv"))
message("✓ tabla1_v5.csv")

# ── tabla_prevalencias_v5.csv ────────────────────────────────
# Prevalencia ponderada (survey-weighted) por ola y screening — para las figuras S1
prev_rows <- bind_rows(lapply(names(pob), function(p) {
  v_map <- c(CRC = "soh_2a", Breast = "mama_2a", Cervical = "cito_3a")
  var <- v_map[p]
  d <- pob[[p]]
  bind_rows(lapply(sort(unique(d$anyo)), function(yr) {
    s <- d[d$anyo == yr, ]
    tibble(
      Año = yr,
      N = nrow(s),
      Prevalencia = round(100 * weighted.mean(s$y, s$peso_centrado), 1),
      SE = round(100 * sqrt(weighted.mean((s$y - weighted.mean(s$y, s$peso_centrado))^2, s$peso_centrado) / nrow(s)), 2),
      Variable = var)
  }))
}))
write_csv(prev_rows, file.path(DIR_OUTPUT, "tabla_prevalencias_v5.csv"))
message("✓ tabla_prevalencias_v5.csv")
print(prev_rows |> select(Año, Variable, N, Prevalencia))

# ── Dataset de análisis ──────────────────────────────────────
enc <- c(`2009` = "EESE 2009", `2011` = "ENSE 2011", `2014` = "EESE 2014",
         `2017` = "ENSE 2017", `2020` = "EESE 2020", `2023` = "ESdE 2023",
         `2025` = "ESdE 2025")
edu <- c(Bajo = "Low (ISCED 0-2)", Medio = "Medium (ISCED 3,5)", Alto = "High (ISCED 6-8)")
ds <- OLAS |> transmute(
  Survey_wave = anyo, Survey_name = enc[as.character(anyo)], CCAA = as.character(ccaa_f), Weight_centred = peso_centrado,
  Sex = ifelse(sexo_f == "Hombre", "Male", "Female"), Age = edad,
  Age_group_CRC = as.character(edad_grupo), Age_group_Cervical = as.character(edad_grupo_cito),
  Education_3cat = unname(edu[as.character(isced_3)]), Country_of_birth = as.character(pais_nac_f),
  Stratum_CRC = as.character(estrato_4), Stratum_Breast_Cervical = as.character(estrato_4_cito),
  Outcome_CRC = soh_2a, Outcome_Breast = mama_2a, Outcome_Cervical = cito_3a,
  Eligible_CRC = as.integer(!is.na(edad_grupo)),
  Eligible_Breast = as.integer(sexo_f == "Mujer" & !is.na(edad_grupo)),
  Eligible_Cervical = as.integer(sexo_f == "Mujer" & !is.na(edad_grupo_cito) & edad <= 65))
write_csv(ds, file.path(DIR_OUTPUT, "analysis_dataset_v5.csv"), na = "")
message("✓ analysis_dataset_v5.csv")

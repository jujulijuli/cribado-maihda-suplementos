# 10_smaihda_estrato_vs_celda_v5.R — S-MAIHDA por estrato (Tabla suplementaria S3), 7 olas
# Réplica en R del do-file de Raquel (smaihda_estrato_vs_celda.do); reproduce sus cifras 2009–2023.
# Lo que va al artículo: VPC* corregido por ruido binomial por ESTRATO y AUC por estrato
# (in-sample y leave-one-out). Las columnas por celda (estrato × CCAA) solo documentan
# el contraste con el cálculo anterior: NO se usan en el artículo (error de concepto).
suppressMessages(library(dplyr))
O <- readRDS("output/olas_preparadas_v5.rds")
mu <- function(yr, scr) {
  d <- O[O$anyo == yr, ]
  d <- switch(scr,
    CRC = d |> filter(!is.na(edad_grupo)) |> mutate(y = soh_2a, sd = interaction(sexo_f, isced_3, pais_nac_f, edad_grupo, drop = TRUE)),
    Breast = d |> filter(sexo_f == "Mujer", !is.na(edad_grupo)) |> mutate(y = mama_2a, sd = interaction(isced_3, pais_nac_f, edad_grupo, drop = TRUE)),
    Cervical = d |> filter(sexo_f == "Mujer", !is.na(edad_grupo_cito), edad <= 65) |> mutate(y = cito_3a, sd = interaction(isced_3, pais_nac_f, edad_grupo_cito, drop = TRUE)))
  d |> filter(!is.na(y), !is.na(sd), !is.na(ccaa_f))
}
vpc <- function(n, x, insesgado = FALSE) {
  p <- x / n; vb <- var(p); vi <- mean(p * (1 - p))
  ru <- if (insesgado) mean(ifelse(n > 1, p * (1 - p) / (n - 1), NA), na.rm = TRUE) else mean(p * (1 - p) / n)
  vc <- max(vb - ru, 0)
  c(unc = 100 * vb / (vb + vi), corr = 100 * vc / (vc + vi))
}
aucw <- function(y, s) { r <- rank(s); n1 <- sum(y); n0 <- length(y) - n1; (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0) }
res <- list()
for (sc in c("CRC", "Breast", "Cervical")) for (yr in sort(unique(O$anyo))) {
  d <- mu(yr, sc)
  e <- d |> group_by(sd) |> summarise(n = n(), x = sum(y), .groups = "drop")
  c_ <- d |> group_by(sd, ccaa_f) |> summarise(n = n(), x = sum(y), .groups = "drop")
  ve <- vpc(e$n, e$x); vc <- vpc(c_$n, c_$x); vci <- vpc(c_$n, c_$x, TRUE)
  d <- d |> group_by(sd) |> mutate(ne = n(), ce = sum(y)) |> ungroup() |>
    mutate(pe = ce / ne, ploo = ifelse(ne > 1, (ce - y) / (ne - 1), mean(y)))
  res[[length(res) + 1]] <- data.frame(screening = sc, ola = yr, N = nrow(d), n_estratos = nrow(e), n_celdas = nrow(c_),
    celdas_n_mediana = median(c_$n), pct_celdas_extremas = round(100 * mean(c_$x == 0 | c_$x == c_$n), 1),
    VPC_estrato_unc = ve[["unc"]], VPC_estrato_corr = ve[["corr"]],
    VPC_celda_unc = vc[["unc"]], VPC_celda_corr = vc[["corr"]], VPC_celda_corr_insesgado = vci[["corr"]],
    AUC_estrato = aucw(d$y, d$pe), AUC_estrato_loo = aucw(d$y, d$ploo))
}
r <- bind_rows(res); write.csv(r, "output/contraste_smaihda_estrato_celda_v5.csv", row.names = FALSE)
v <- read.csv("/mnt/user-data/uploads/ccr-informe/output/tabla_vpc_v5.csv") |>
  mutate(screening = c(soh_2a = "CRC", mama_2a = "Breast", cito_3a = "Cervical")[variable]) |>
  select(screening, ola = anyo, RE_VPC_estrato = VPC_estrato_pct, RE_AUC_str = AUC_fijos_estrato)
r <- left_join(r, v, by = c("screening", "ola"))
options(width = 250); print(r |> mutate(across(where(is.double), ~ round(.x, 3))))

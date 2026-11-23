suppressMessages(library(dplyr))
U <- "output/"   # ejecutar con wd = ccr-informe/
lab <- c(soh_2a = "CRC", mama_2a = "Breast", cito_3a = "Cervical")
f1 <- function(x) formatC(x, format = "f", digits = 1); f2 <- function(x) formatC(x, format = "f", digits = 2)
# S1 ORs
o <- read.csv(paste0(U, "tabla_or_v5.csv"))
tl <- function(t) case_when(t == "sexo_fMujer" ~ "Female (ref: male)", t == "isced_3Medio" ~ "Education: medium (ref: low)",
  t == "isced_3Alto" ~ "Education: high (ref: low)", t == "pais_nac_fExtranjero" ~ "Foreign-born (ref: Spain-born)",
  grepl("^edad_grupo_cito", t) ~ paste0("Age ", sub("edad_grupo_cito", "", t), " (ref: 25–29)"),
  grepl("^edad_grupo", t) ~ paste0("Age ", sub("edad_grupo", "", t), " (ref: 50–54)"))
ord <- function(t) match(sub(" \\(.*", "", tl(t)), c("Female", "Education: medium", "Education: high", "Foreign-born")) %>% coalesce(10L)
S1 <- o %>% mutate(Screening = lab[variable], term_l = tl(term), k = ord(term)) %>%
  arrange(factor(Screening, c("CRC", "Breast", "Cervical")), anyo, k, term) %>%
  transmute(Screening, Wave = anyo, Term = gsub("-", "–", term_l), OR = f2(OR), `95% CI` = paste0(f2(IC_inf), "–", f2(IC_sup)),
            p = ifelse(p < 0.001, "<0.001", formatC(p, format = "f", digits = 3)))
write.csv(S1, paste0(U, "tabla_S1_or_v5.csv"), row.names = FALSE)
# S2 pesos
# S2: tabla_descomposicion_pesos_v5.csv (03c, 7 olas) si existe; si no, v1 (2009–2023) + 2025
f5 <- "output/tabla_descomposicion_pesos_v5.csv"
w <- if (file.exists(f5)) read.csv(f5) else
  bind_rows(read.csv(paste0(U, "tabla_descomposicion_pesos_v1.csv")), read.csv(paste0(U, "tabla_descomposicion_pesos_2025.csv")))
v <- read.csv(paste0(U, "tabla_vpc_v5.csv"))
chk <- w %>% filter(modelo == "sin_peso") %>% left_join(v %>% select(anyo, variable, VPC_estrato_pct.y = VPC_estrato_pct, VPC_ccaa_pct.y = VPC_ccaa_pct), by = c("anyo", "variable"))
cat("max dif M0 vs Tabla 2:", max(abs(chk$VPC_estrato_pct - chk$VPC_estrato_pct.y)), max(abs(chk$VPC_ccaa_pct - chk$VPC_ccaa_pct.y)), "\n")
mlab <- c(sin_peso = "Unweighted (M0, primary)", peso_real = "Full weight (M1)", w_ccaa = "Between-region component only (M2)", w_dentro = "Within-region component only (M3)")
S2 <- w %>% mutate(Screening = lab[variable], m = factor(modelo, names(mlab))) %>% arrange(factor(Screening, c("CRC", "Breast", "Cervical")), anyo, m) %>%
  transmute(Screening, Wave = anyo, Model = mlab[as.character(m)], `VPC stratum (%)` = f1(VPC_estrato_pct), `VPC region (%)` = f1(VPC_ccaa_pct))
write.csv(S2, paste0(U, "tabla_S2_pesos_v5.csv"), row.names = FALSE)
# S3 S-MAIHDA por estrato
r <- read.csv(paste0(U, "contraste_smaihda_estrato_celda_v5.csv"))
v2 <- v %>% mutate(screening = lab[variable]) %>% select(screening, ola = anyo, RE = VPC_estrato_pct, REauc = AUC_fijos_estrato)
S3 <- r %>% left_join(v2, by = c("screening", "ola")) %>% arrange(factor(screening, c("CRC", "Breast", "Cervical")), ola) %>%
  transmute(Screening = screening, Wave = ola, N = format(N, big.mark = ","), `N strata` = n_estratos,
            `S-MAIHDA VPC* (%)` = f1(VPC_estrato_corr),
            `RE-MAIHDA VPC stratum (%)` = f1(RE), `AUC stratum means` = f2(AUC_estrato), `AUC leave-one-out` = f2(AUC_estrato_loo))
write.csv(S3, paste0(U, "tabla_S3_smaihda_v5.csv"), row.names = FALSE)

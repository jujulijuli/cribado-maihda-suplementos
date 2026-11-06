# Figura 1 (prevalencias con IC 95%) regenerada desde tabla_prevalencias.csv
# Propuesta Figura 3 en escala de probabilidad (desviación en pp respecto al estrato medio, modelo nulo)
suppressPackageStartupMessages({library(dplyr); library(ggplot2); library(readr); library(tidyr)})
DIR <- "output"; W_FULL <- 7.09; DPI <- 300
lab_prog <- c(soh_2a = "Colorectal (FOBT, 50–69 y)", mama_2a = "Breast (mammography, 50–69 y)",
              cito_3a = "Cervical (cytology, 25–65 y)")
cols <- c(soh_2a = "#C0392B", mama_2a = "#1F5FA8", cito_3a = "#2E8B3E")

prev <- read_csv(file.path(DIR, "tabla_prevalencias.csv"), show_col_types = FALSE) |>
  rename(anyo = Año) |>
  mutate(lo = Prevalencia - 1.96 * SE, hi = Prevalencia + 1.96 * SE,
         prog = factor(Variable, levels = names(lab_prog), labels = lab_prog))
f1 <- ggplot(prev, aes(anyo, Prevalencia, colour = Variable)) +
  geom_line(linewidth = 0.6) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.7, linewidth = 0.5) +
  geom_point(size = 1.8) +
  facet_wrap(~prog, nrow = 1, scales = "free_y") +
  scale_colour_manual(values = cols, guide = "none") +
  scale_x_continuous(breaks = c(2009, 2011, 2014, 2017, 2020, 2023)) +
  scale_y_continuous(expand = expansion(mult = c(0.08, 0.15))) +
  labs(x = "Survey year", y = "Screening uptake (%)") +
  theme_bw(base_size = 9) +
  theme(panel.grid.minor = element_blank(), strip.background = element_rect(fill = "grey95"),
        axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(file.path(DIR, "figuras/Figure1_prevalence_CI_cc.png"),  f1, width = W_FULL, height = 2.9, units = "in", dpi = DPI)
ggsave(file.path(DIR, "figuras/Figure1_prevalence_CI_cc.tiff"), f1, width = W_FULL, height = 2.9, units = "in", dpi = DPI, compression = "lzw")

# ---- Figura 3: escala de probabilidad, 2023, estratos etiquetados
expit <- plogis
b <- read_csv(file.path(DIR, "tabla_blups_cc.csv"), show_col_types = FALSE) |>
  mutate(dev = 100 * (expit(beta0 + blup) - expit(beta0)),
         lo  = 100 * (expit(beta0 + blup - 1.96 * se) - expit(beta0)),
         hi  = 100 * (expit(beta0 + blup + 1.96 * se) - expit(beta0)),
         sig = (blup - 1.96 * se > 0) | (blup + 1.96 * se < 0),
         lab = paste0(ifelse(variable == "soh_2a", ifelse(sexo == "Hombre", "Men, ", "Women, "), ""),
                      recode(estudios, Bajo = "low", Medio = "medium", Alto = "high"), " educ., ",
                      recode(origen, `España` = "Spain-born", Extranjero = "foreign-born"), ", ",
                      gsub("-", "–", edad), " (n=", n, ")"))
write_csv(b |> select(anyo, variable, estrato_id, lab, n, beta0, blup, se, dev, lo, hi, sig),
          file.path(DIR, "tabla_blups_prob_cc.csv"))
K <- 8
sel <- b |> filter(anyo == 2023) |> group_by(variable) |>
  arrange(dev, .by_group = TRUE) |> slice(c(1:K, (n() - K + 1):n())) |> ungroup() |>
  mutate(prog = factor(variable, levels = names(lab_prog), labels = lab_prog),
         dir = ifelse(dev > 0, "Above average stratum", "Below average stratum"),
         key = paste(variable, lab))
sel$key <- factor(sel$key, levels = sel$key[order(sel$variable, sel$dev)])
f3 <- ggplot(sel, aes(dev, key, fill = dir)) +
  geom_vline(xintercept = 0, linewidth = 0.4) +
  geom_col(aes(alpha = sig), width = 0.7) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.3, linewidth = 0.3, colour = "grey30") +
  facet_wrap(~prog, ncol = 1, scales = "free_y") +
  scale_y_discrete(labels = function(x) sub("^\\S+ ", "", x)) +
  scale_fill_manual(values = c("Above average stratum" = "#2E8B3E", "Below average stratum" = "#C0392B"), name = NULL, guide = "none") +
  scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.4), labels = c(`TRUE` = "95% interval excludes 0", `FALSE` = "95% interval includes 0"), name = NULL) +
  labs(x = "Deviation in predicted uptake from the average stratum (percentage points)", y = NULL) +
  theme_bw(base_size = 8.5) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(), strip.background = element_rect(fill = "grey95"))
ggsave(file.path(DIR, "figuras/Figure3_prob_scale_2023_cc.png"),  f3, width = W_FULL, height = 8.2, units = "in", dpi = DPI)
ggsave(file.path(DIR, "figuras/Figure3_prob_scale_2023_cc.tiff"), f3, width = W_FULL, height = 8.2, units = "in", dpi = DPI, compression = "lzw")
print(sel |> select(variable, lab, dev, lo, hi, sig) |> mutate(across(c(dev, lo, hi), ~round(.x, 1))), n = 60)

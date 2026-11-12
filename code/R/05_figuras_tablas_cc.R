# ============================================================
# 05_figuras_tablas_cc.R — Tablas y figuras del artículo a partir de los
# modelos MAIHDA CRUZADOS (03_analisis_maihda_v3.R)
# ============================================================
# Entradas (output/): tabla_vpc_cc.csv, tabla_or_cc.csv, tabla_blups_cc.csv,
#                     tabla_prevalencias.csv, tabla_vpc.csv (anidado, comparación)
# Plantillas: R/Supplementary_Figure_S1_template.html (marcador // INJECT_PANEL_DATA)
#             Figure3_interactive_6olas.html (marcadores // <<FIG3_DATA ... // FIG3_DATA>>)
# Salidas:   output/tablas_JECH_CCR_cc.xlsx
#            output/figuras/Figure2_VPC_decomposition_cc.{png,tiff}
#            output/figuras/Figure3_caterpillar_BLUPs_cc.{png,tiff}
#            output/figuras/FigureS_caterpillar_all_strata_cc.png
#            Supplementary_Figure_S1_6olas_cc.html, Figure3_interactive_6olas_cc.html
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(readr); library(jsonlite); library(openxlsx)
})
DIR_OUTPUT <- "output"
dir.create(file.path(DIR_OUTPUT, "figuras"), recursive = TRUE, showWarnings = FALSE)

W_FULL <- 7.09; DPI <- 300
COL_CCAA <- "#e6550d"; COL_ESTRA <- "#3182bd"; COL_PT <- "#1565C0"
OLAS_LAB <- c(`2009` = "EESE 2009", `2011` = "ENSE 2011/12", `2014` = "EESE 2014",
              `2017` = "ENSE 2017", `2020` = "EESE 2020", `2023` = "ESdE 2023")
PROG <- c(soh_2a = "CRC", mama_2a = "Breast", cito_3a = "Cervical")
PROG_FIG <- c(soh_2a = "CRC (FOBT)", mama_2a = "Breast\n(mammography)", cito_3a = "Cervical\n(cytology)")
SCR <- c(soh_2a = "ccr", mama_2a = "mama", cito_3a = "cervix")

tema_base <- theme_classic(base_size = 9) +
  theme(panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
        axis.title = element_text(size = 8), axis.text = element_text(size = 8),
        legend.text = element_text(size = 8), legend.title = element_text(size = 8, face = "bold"),
        strip.text = element_text(face = "bold", size = 8.5),
        strip.background = element_rect(fill = "grey95", color = NA),
        plot.caption = element_text(size = 6.5, color = "grey50", hjust = 0, lineheight = 1.3),
        plot.margin = margin(4, 6, 4, 4, "mm"))

vpc   <- read_csv(file.path(DIR_OUTPUT, "tabla_vpc_v4.csv"), show_col_types = FALSE)
# Recalcular VPC/ICC/PCV sin redondear desde las varianzas (evita doble redondeo:
# 03 guarda los porcentajes ya redondeados a 2 decimales)
vpc <- vpc |> mutate(
  .den = sigma2_estrato_nulo + sigma2_ccaa_nulo + pi^2 / 3,
  VPC_estrato_pct = 100 * sigma2_estrato_nulo / .den,
  VPC_ccaa_pct    = 100 * sigma2_ccaa_nulo / .den,
  ICC_total_pct   = VPC_estrato_pct + VPC_ccaa_pct,
  PCV_estrato_pct = 100 * (sigma2_estrato_nulo - sigma2_estrato_ajust) / sigma2_estrato_nulo,
  PCV_ccaa_pct    = 100 * (sigma2_ccaa_nulo - sigma2_ccaa_ajust) / sigma2_ccaa_nulo) |> select(-.den)
or_t  <- read_csv(file.path(DIR_OUTPUT, "tabla_or_v4.csv"), show_col_types = FALSE)
blups <- read_csv(file.path(DIR_OUTPUT, "tabla_blups_v4.csv"), show_col_types = FALSE)
prev  <- read_csv(file.path(DIR_OUTPUT, "tabla_prevalencias.csv"), show_col_types = FALSE)
vpc_nest <- if (file.exists(file.path(DIR_OUTPUT, "tabla_vpc.csv")))
  read_csv(file.path(DIR_OUTPUT, "tabla_vpc.csv"), show_col_types = FALSE) else NULL

ord_var <- function(v) factor(v, levels = names(PROG))

# ── Tabla 2: VPC / ICC / PCV ────────────────────────────────
t2 <- vpc |>
  mutate(Screening = PROG[variable], ord = ord_var(variable)) |>
  arrange(ord, anyo) |>
  transmute(Screening, Wave = anyo, `N model` = n_modelo, `N positive` = n_pos_modelo,
            `Strata` = n_estratos,
            `VPC_str (%)` = round(VPC_estrato_pct, 1), `VPC_reg (%)` = round(VPC_ccaa_pct, 1),
            `ICC_tot (%)` = round(ICC_total_pct, 1),
            `PCV_str (%)` = round(PCV_estrato_pct, 1), `PCV_reg (%)` = round(PCV_ccaa_pct, 1),
            `Adjusted σ²_str = 0` = ifelse(singular_ajust, "yes", ""))

# ── Tabla AUC ───────────────────────────────────────────────
t_auc <- vpc |>
  mutate(Screening = PROG[variable], ord = ord_var(variable)) |> arrange(ord, anyo) |>
  transmute(Screening, Wave = anyo,
            `AUC fixed effects` = round(AUC_fijos, 3),
            `AUC fixed + stratum` = round(AUC_fijos_estrato, 3),
            `AUC fixed + stratum + territory` = round(AUC_completo, 3))

# ── Tabla 3: OR ─────────────────────────────────────────────
term_lab <- function(t) {
  t <- sub("^sexo_fMujer$", "Female (ref: Male)", t)
  t <- sub("^isced_3Medio$", "Education: medium (ref: low)", t)
  t <- sub("^isced_3Alto$", "Education: high (ref: low)", t)
  t <- sub("^pais_nac_fExtranjero$", "Foreign-born (ref: Spain-born)", t)
  t <- sub("^edad_grupo_cito(.*)$", "Age \\1 (ref: 25–29)", t)
  t <- sub("^edad_grupo(.*)$", "Age \\1 (ref: 50–54)", t)
  t
}
fmt_p <- function(p) ifelse(p < 0.001, "<0.001", ifelse(p < 0.01, sprintf("%.3f", p), sprintf("%.2f", p)))
t3 <- or_t |>
  mutate(Screening = PROG[variable], ord = ord_var(variable)) |> arrange(ord, anyo) |>
  mutate(no_est = se > 10,   # separación completa (p. ej. mama 2009, nacidas fuera: 13/13 cribadas)
         OR_txt = ifelse(no_est, "NE", sprintf("%.2f", OR)),
         CI_txt = ifelse(no_est, "—", sprintf("%.2f–%.2f", IC_inf, IC_sup)),
         p_txt  = ifelse(no_est, "—", fmt_p(p))) |>
  transmute(Screening, Wave = anyo, Term = term_lab(term), OR = OR_txt, `95% CI` = CI_txt, p = p_txt)

# ── Comparación cruzado vs anidado ──────────────────────────
t_cmp <- NULL
if (!is.null(vpc_nest)) {
  t_cmp <- vpc |>
    select(variable, anyo, n_modelo, VPC_estrato_pct, VPC_ccaa_pct, PCV_estrato_pct, PCV_ccaa_pct) |>
    left_join(vpc_nest |> select(variable, anyo, VPC_str_nested = VPC_estrato_pct, VPC_reg_nested = VPC_ccaa_pct,
                                 PCV_str_nested = PCV_estrato_pct, PCV_reg_nested = PCV_ccaa_pct),
              by = c("variable", "anyo")) |>
    mutate(Screening = PROG[variable], ord = ord_var(variable)) |> arrange(ord, anyo) |>
    transmute(Screening, Wave = anyo, `N model` = n_modelo,
              `VPC_str crossed` = round(VPC_estrato_pct, 1), `VPC_str nested` = round(VPC_str_nested, 1),
              `VPC_reg crossed` = round(VPC_ccaa_pct, 1), `VPC_reg nested` = round(VPC_reg_nested, 1),
              `PCV_str crossed` = round(PCV_estrato_pct, 1), `PCV_str nested` = round(PCV_str_nested, 1),
              `PCV_reg crossed` = round(PCV_ccaa_pct, 1), `PCV_reg nested` = round(PCV_reg_nested, 1))
}

wb <- createWorkbook()
hdr <- createStyle(textDecoration = "bold", fgFill = "#1a3a5c", fontColour = "white", wrapText = TRUE)
add_sheet <- function(name, title, note, df) {
  addWorksheet(wb, name)
  writeData(wb, name, title, startRow = 1); addStyle(wb, name, createStyle(textDecoration = "bold"), 1, 1)
  writeData(wb, name, note, startRow = 2)
  writeData(wb, name, df, startRow = 4, headerStyle = hdr)
  setColWidths(wb, name, seq_along(df), "auto"); freezePane(wb, name, firstActiveRow = 5)
}
add_sheet("T2-VPC", "Table 2. MAIHDA variance metrics by screening type and survey wave (cross-classified models)",
  paste("Three-level cross-classified models: individuals in intersectional strata crossed with 19 territorial units.",
        "VPC latent-variable approach, denominator σ²_str + σ²_reg + π²/3. PCV adjusted vs null.",
        "'Adjusted σ²_str = 0': stratum variance estimated at the boundary in the adjusted model (PCV_str = 100%)."), t2)
add_sheet("T3-OR", "Table 3. Fixed-effect odds ratios from adjusted cross-classified MAIHDA models",
  "Wald 95% CI. NE: not estimable (complete separation; breast 2009: all 13 foreign-born women screened).", t3)
add_sheet("AUC", "Discriminatory accuracy (AUC) of adjusted cross-classified MAIHDA models",
  "Unweighted AUC of individual predicted probabilities (pROC).", t_auc)
if (!is.null(t_cmp)) add_sheet("Crossed vs nested", "VPC and PCV: cross-classified (v3) vs nested (v2) models",
  "Nested v2: (1 | ccaa_f/estrato_id), stratum effect per stratum×territory. Crossed v3: (1 | estrato_id) + (1 | ccaa_f).", t_cmp)
add_sheet("BLUPs", "Stratum BLUPs from null cross-classified models", "SE: conditional (postVar) SD.",
  blups |> mutate(Screening = PROG[variable]) |> select(Screening, Wave = anyo, sexo, estudios, origen, edad, n, n_pos, blup, se, beta0))
saveWorkbook(wb, file.path(DIR_OUTPUT, "tablas_JECH_CCR_cc.xlsx"), overwrite = TRUE)
message("✓ tablas_JECH_CCR_cc.xlsx")

# ── Figura 2: VPC con IC bootstrap ──────────────────────────
vpc_long <- vpc |>
  mutate(Programme = factor(PROG_FIG[variable], levels = PROG_FIG)) |>
  select(anyo, Programme,
         vpc_ccaa = VPC_ccaa_pct, vpc_est = VPC_estrato_pct,
         vpc_ccaa_lo, vpc_ccaa_hi, vpc_est_lo, vpc_est_hi) |>
  pivot_longer(c(vpc_ccaa, vpc_est), names_to = "Level", values_to = "VPC") |>
  mutate(
    lo = ifelse(Level == "vpc_ccaa", vpc_ccaa_lo, vpc_est_lo),
    hi = ifelse(Level == "vpc_ccaa", vpc_ccaa_hi, vpc_est_hi),
    Level = factor(Level, levels = c("vpc_ccaa", "vpc_est"),
                   labels = c("Autonomous Community (AC)", "Intersectional stratum")))
fig2 <- ggplot(vpc_long, aes(anyo, VPC, color = Level, fill = Level, shape = Level, group = Level)) +
  geom_vline(data = data.frame(Programme = factor(PROG_FIG[["soh_2a"]], levels = PROG_FIG), x = 2014),
             aes(xintercept = x), linetype = "dotted", color = "grey60", linewidth = 0.5) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.8) + geom_point(size = 2.5) +
  facet_wrap(~Programme, ncol = 3) +
  scale_color_manual(name = "VPC level", values = c("Autonomous Community (AC)" = COL_CCAA, "Intersectional stratum" = COL_ESTRA)) +
  scale_fill_manual(name  = "VPC level", values = c("Autonomous Community (AC)" = COL_CCAA, "Intersectional stratum" = COL_ESTRA)) +
  scale_shape_manual(name = "VPC level", values = c("Autonomous Community (AC)" = 16, "Intersectional stratum" = 17)) +
  scale_x_continuous(name = "Survey year", breaks = as.integer(names(OLAS_LAB))) +
  scale_y_continuous(name = "Variance Partition Coefficient (%)", limits = c(0, NA), labels = function(x) paste0(x, "%")) +
  labs(caption = paste0("Null three-level cross-classified MAIHDA models (strata crossed with 19 territorial units). VPC latent-variable approach. ",
                        "\nShaded bands: 95% bootstrap CI (200 iterations, resampling within autonomous communities). ",
                        "\nOrange circles: VPC at AC level. Blue triangles: VPC at stratum level. ",
                        "Dotted line (CRC): 2014. Strata: sex × education × country of birth × age group.")) +
  tema_base + theme(legend.position = "bottom", legend.key.width = unit(0.8, "cm"),
                    axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(file.path(DIR_OUTPUT, "figuras/Figure2_VPC_decomposition_cc.png"),  fig2, width = W_FULL, height = 4.2, units = "in", dpi = DPI)
ggsave(file.path(DIR_OUTPUT, "figuras/Figure2_VPC_decomposition_cc.tiff"), fig2, width = W_FULL, height = 4.2, units = "in", dpi = DPI, compression = "lzw")
message("✓ Figura 2")

# ── Figura 3: caterpillar (15 más bajos + 15 más altos) ─────
cat_df <- blups |>
  mutate(Programme = factor(PROG[variable], levels = PROG), Wave = factor(anyo)) |>
  group_by(variable, anyo) |> arrange(blup, .by_group = TRUE) |>
  mutate(rank = row_number(), N = n(), lo = blup - 1.96 * se, hi = blup + 1.96 * se,
         sig = lo > 0 | hi < 0) |> ungroup()
cat_top <- cat_df |> filter(rank <= 15 | rank > N - 15) |>
  group_by(variable, anyo) |> mutate(pos = row_number()) |> ungroup()
mk_cat <- function(d, xvar) {
  ggplot(d, aes(.data[[xvar]], blup)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey45", linewidth = 0.35) +
    geom_linerange(aes(ymin = lo, ymax = hi), colour = "grey65", linewidth = 0.35) +
    geom_point(aes(fill = sig), shape = 21, colour = COL_PT, size = 0.9, stroke = 0.4) +
    scale_fill_manual(values = c(`TRUE` = COL_PT, `FALSE` = "white"), guide = "none") +
    facet_grid(Wave ~ Programme, scales = "free_x", labeller = labeller(Wave = OLAS_LAB)) +
    scale_x_continuous(breaks = NULL, name = "Strata ranked by BLUP") +
    scale_y_continuous(name = "Stratum BLUP (log-odds)") +
    tema_base + theme(panel.border = element_rect(fill = NA, colour = "grey80", linewidth = 0.3),
                      strip.text.y = element_text(size = 7))
}
fig3 <- mk_cat(cat_top, "pos") +
  geom_vline(data = cat_top |> distinct(Programme, Wave, N) |> filter(N > 30), aes(xintercept = 15.5),
             colour = "grey80", linewidth = 0.3, linetype = "dotted") +
  labs(caption = paste0("Null three-level cross-classified MAIHDA models. Each point is one intersectional stratum; bars: BLUP ± 1.96 × conditional SE.\n",
                        "Filled points: interval excludes zero. Only the 15 lowest- and 15 highest-ranked strata per panel (separated by the dotted line where strata were omitted)."))
ggsave(file.path(DIR_OUTPUT, "figuras/Figure3_caterpillar_BLUPs_cc.png"),  fig3, width = W_FULL, height = 8.5, units = "in", dpi = DPI)
ggsave(file.path(DIR_OUTPUT, "figuras/Figure3_caterpillar_BLUPs_cc.tiff"), fig3, width = W_FULL, height = 8.5, units = "in", dpi = DPI, compression = "lzw")
figS <- mk_cat(cat_df, "rank") + labs(caption = "All strata. Null three-level cross-classified MAIHDA models; bars: BLUP ± 1.96 × conditional SE.")
ggsave(file.path(DIR_OUTPUT, "figuras/FigureS_caterpillar_all_strata_cc.png"), figS, width = W_FULL, height = 8.5, units = "in", dpi = DPI)
message("✓ Figura 3 (caterpillar)")

# ── Supplementary Figure S1 (trayectorias interactivas) ─────
lab_estr <- function(d) ifelse(d$variable == "soh_2a",
  paste(d$sexo, d$estudios, d$origen, d$edad, sep = " · "),
  paste(d$estudios, d$origen, d$edad, sep = " · "))
s1 <- blups |>
  left_join(prev |> transmute(anyo = Año, variable = Variable, media = Prevalencia), by = c("anyo", "variable")) |>
  mutate(label = lab_estr(pick(everything())),
         prev_pred = round(100 * plogis(beta0 + blup), 2),
         # desviación respecto a un estrato promedio de la misma ola, expit(beta0): misma escala
         # condicional que prev_pred (la media nacional ponderada es marginal y desplazaba todos los estratos)
         prev_rel = round(100 * (plogis(beta0 + blup) - plogis(beta0)), 2))
panels <- lapply(names(SCR), function(v) s1 |> filter(variable == v) |> arrange(label, anyo) |>
                   select(label, anyo, prev_pred, prev_rel, sexo, estudios, origen, edad, n))
names(panels) <- unname(SCR)
fmt_m <- function(v) { d <- prev |> filter(Variable == v) |> arrange(Año); paste0("{", paste0(d$Año, ": ", d$Prevalencia, collapse = ", "), "}") }
s1_js <- c(paste0("const PANEL_DATA = ", toJSON(list(panels = panels), dataframe = "rows", na = "null", auto_unbox = TRUE, digits = NA), ";"),
           "", "const MEDIAS = {",
           paste0("  ccr:    ", fmt_m("soh_2a"), ","), paste0("  mama:   ", fmt_m("mama_2a"), ","),
           paste0("  cervix: ", fmt_m("cito_3a"), ","), "};")
tpl <- "R/Supplementary_Figure_S1_template.html"
if (file.exists(tpl)) {
  h <- readLines(tpl, encoding = "UTF-8", warn = FALSE)
  h <- h[!grepl("^\\s*// v2: MEDIAS .* replace \\{", h)]          # comentario antiguo con datos de 4 olas
  i <- grep("// INJECT_PANEL_DATA", h, fixed = TRUE); stopifnot(length(i) == 1)
  h <- c(h[seq_len(i - 1)], s1_js, h[(i + 1):length(h)])
  h <- paste(h, collapse = "\n")
  rep1 <- function(x, a, b) { if (!grepl(a, x, fixed = TRUE)) warning("S1: texto no encontrado: ", substr(a, 1, 60)); sub(a, b, x, fixed = TRUE) }
  h <- rep1(h, "Intersectional inequalities in cancer screening participation in Spain, 2009&ndash;2023: a three-level MAIHDA analysis.",
               "Intersectional inequalities in cancer screening participation in Spain over six survey waves (2009&ndash;2023): a cross-classified MAIHDA analysis.")
  h <- rep1(h, "<b>Method:</b> Three-level MAIHDA (individual &rarr; intersectional stratum &rarr; autonomous community)",
               "<b>Method:</b> Null three-level cross-classified MAIHDA (intersectional strata &times; 19 territorial units)")
  h <- gsub("EESE 2009/2014/2020, ESdE 2023", "EESE 2009/2014/2020, ENSE 2011/2017, ESdE 2023", h, fixed = TRUE)
  h <- gsub("EESE 2009/2014/2020 and ESdE 2023", "EESE 2009/2014/2020, ENSE 2011/2017 and ESdE 2023", h, fixed = TRUE)
  h <- rep1(h, "Longitudinal trajectories of MAIHDA-predicted cancer screening participation for each intersectional stratum,",
               "Longitudinal trajectories of predicted participation for each intersectional stratum, expit(&beta;<sub>0</sub> + u<sub>j</sub>), from null cross-classified MAIHDA models,")
  h <- rep1(h, "and age group (50&ndash;54, 55&ndash;59, 60&ndash;64, 65&ndash;69 years); for CRC screening, sex is included as a dimension, \n  yielding 48 theoretical strata; for breast and cervical, 24 theoretical strata (women only). \n  Only strata with n&thinsp;&ge;&thinsp;10 in all survey years are shown (34 for CRC; variable for breast/cervical). ",
               "and age group (CRC and breast: 50&ndash;54, 55&ndash;59, 60&ndash;64, 65&ndash;69 years; cervical: eight groups, 25&ndash;65 years); for CRC screening, sex is included as a dimension, \n  yielding 48 theoretical strata; 24 for breast and 48 for cervical (women only). \n  All strata observed in each wave are shown; the deviation is relative to expit(&beta;<sub>0</sub>) of each wave (an average stratum), so that strata are centred within each wave. ")
  h <- gsub("Deviation from national mean (pp)", "Deviation from average stratum (pp)", h, fixed = TRUE)
  h <- gsub("Deviation vs. national mean", "Deviation vs. average stratum", h, fixed = TRUE)
  h <- rep1(h, "expressed as deviation from the national mean (pp) or as predicted prevalence (%). ",
    paste0("expressed as deviation (pp) from the predicted participation of an average stratum, expit(&beta;<sub>0</sub>), or as predicted prevalence (%). ",
           "Because the logit link is non-linear, equal differences on the log-odds scale translate into larger percentage-point differences as participation approaches 50%: ",
           "the widening of CRC deviations from 2017 onwards largely reflects the rise in CRC participation (from 3&ndash;12% to 22&ndash;39%), not only a rise in log-odds heterogeneity. ",
           "The log-odds scale is shown in Interactive Figure 3. "))
  writeLines(h, "Supplementary_Figure_S1_6olas_cc.html", useBytes = TRUE)
  message("✓ Supplementary_Figure_S1_6olas_cc.html")
} else message("⚠ Sin plantilla S1 (", tpl, ")")

# ── Figura 3 interactiva ────────────────────────────────────
f3 <- "Figure3_interactive_6olas.html"
if (file.exists(f3)) {
  strata <- blups |>
    transmute(screening = SCR[variable], wave = as.integer(anyo),
              label = ifelse(variable == "soh_2a", paste(sexo, estudios, origen, edad, sep = " | "),
                             paste(estudios, origen, edad, sep = " | ")),
              sexo, estudios, origen, edad, n, n_pos, blup = round(blup, 4), se = round(se, 4))
  refs <- blups |> distinct(variable, anyo, beta0)
  ref_logit <- lapply(split(refs, SCR[refs$variable]), function(d) setNames(as.list(round(d$beta0, 4)), as.character(d$anyo)))
  payload <- toJSON(list(demo = FALSE, strata = strata, ref_logit = ref_logit), auto_unbox = TRUE, na = "null", digits = NA)
  h <- readLines(f3, encoding = "UTF-8", warn = FALSE)
  i0 <- grep("// <<FIG3_DATA", h, fixed = TRUE); i1 <- grep("// FIG3_DATA>>", h, fixed = TRUE)
  h <- c(h[1:i0], paste0("const FIG3_DATA = ", payload, ";"), h[i1:length(h)])
  writeLines(h, "Figure3_interactive_6olas_cc.html", useBytes = TRUE)
  message("✓ Figure3_interactive_6olas_cc.html")
}

# Analysis code

R code for *"Intersectional inequalities in cancer screening participation in Spain over six survey waves (2009–2023): a cross-classified MAIHDA analysis"*.

## Data
Public-use microdata of the Spanish national health surveys, available from the Instituto Nacional de Estadística (INE) and the Ministerio de Sanidad: EESE 2009, ENSE 2011 (adult and household files), EESE 2014, ENSE 2017, EESE 2020 and ESdE 2023. Microdata are not redistributed here. Set the local path in `R/00_config.R`.

## Pipeline (run from this folder: `source("run_all.R")`)
| Script | Purpose | Main output (`output/`) |
|---|---|---|
| `R/00_config.R` | Packages, paths, labels | – |
| `R/01_cargar_datos.R` | Read raw microdata | – |
| `R/02_preparar_datos_v4.R` | Harmonisation (education ISCED, country of birth, age groups, weights), intersectional strata | `olas_preparadas_v3.rds` |
| `R/04_prevalencias.R` | Survey-weighted uptake by wave | `tabla_prevalencias.csv` |
| `R/03_analisis_maihda_v3.R` | Cross-classified MAIHDA (stratum × autonomous community), null and adjusted logistic models (`lme4::glmer`), VPC, PCV, AUC, BLUPs, OR | `tabla_vpc_cc.csv`, `tabla_or_cc.csv`, `tabla_blups_cc.csv` |
| `R/05_figuras_tablas_cc.R` | Table 2, Figure 2, Excel tables, interactive Supplementary Figures S1–S2 | `tablas_JECH_CCR_cc.xlsx`, figures, HTML |
| `R/06_tabla1_dataset.R` | Table 1 | `tabla1_cc.csv` |
| `R/07_fig1_fig3prob.R` | Figure 1 (uptake with 95% CI) and Figure 3 (probability scale) | figures, `tabla_blups_prob_cc.csv` |

`R/Supplementary_Figure_S1_template.html` and `Figure3_interactive_6olas.html` are the HTML templates into which `05_figuras_tablas_cc.R` injects the model results.

Comments in the scripts are in Spanish. Required packages: tidyverse, conflicted, haven, survey, lme4, pROC, openxlsx, jsonlite. The full pipeline was re-run from the raw microdata and reproduced all published estimates (differences < 1e-4, optimiser tolerance).

License: MIT.

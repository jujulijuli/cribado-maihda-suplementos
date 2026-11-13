# Analysis code

R code for *"Intersectional inequalities in cancer screening participation in Spain over seven survey waves (2009–2025): a cross-classified MAIHDA analysis"*.

## Data
Public-use microdata of the Spanish national health surveys, available from the Instituto Nacional de Estadística (INE) and the Ministerio de Sanidad: EESE 2009, ENSE 2011 (adult and household files), EESE 2014, ENSE 2017, EESE 2020, ESdE 2023 and ESdE 2025. Microdata are not redistributed here. Set the local path in `R/00_config.R`.

## Pipeline (run from this folder: `source("run_all.R")`)
| Script | Purpose | Main output (`output/`) |
|---|---|---|
| `R/00_config.R` | Packages, paths, labels | – |
| `R/01_cargar_datos.R` | Read raw microdata (7 waves) | – |
| `R/02_preparar_datos_v4.R` | Harmonisation (education ISCED, country of birth, age groups, weights), intersectional strata | `olas_preparadas_v3.rds` |
| `R/04_prevalencias.R` | Survey-weighted uptake by wave | `tabla_prevalencias.csv` |
| `R/03_analisis_maihda_v4.R` | Cross-classified MAIHDA (stratum × autonomous community), null and adjusted logistic models (`lme4::glmer`), VPC, PCV, AUC, BLUPs, OR | `tabla_vpc_v5.csv`, `tabla_or_v5.csv`, `tabla_blups_prob_v5.csv` |
| `R/03b_bootstrap_vpc.R` | Bootstrap CIs for VPC (200 replications) | `tabla_vpc_v5.csv` (with CIs) |
| `R/05_figuras_tablas_cc.R` | Table 2, Figure 2, Excel tables, interactive Supplementary Figures S1–S2 | `tablas_JECH_CCR_cc.xlsx`, figures, HTML |
| `R/06_tabla1_dataset.R` | Table 1 | `tabla1_v5.csv` |
| `R/07_fig1_fig3prob_v5.R` | Figure 1 (uptake with 95% CI) and Figure 3 / Supplementary Figure S2 (probability scale) | figures, `tabla_blups_prob_v5.csv` |
| `08_figura_S3_brechas_v5.R` | Supplementary Figure S3: absolute participation gaps by component (education + origin), 7 waves | `tabla_S3_brechas_v5.csv`, `Supplementary_Figure_S3_brechas_componentes.html` |

`R/Supplementary_Figure_S1_template.html` and `Figure3_interactive_6olas.html` are HTML templates into which the pipeline injects model results.

Comments in the scripts are in Spanish. Required packages: tidyverse, conflicted, haven, survey, lme4, pROC, openxlsx, jsonlite. The full pipeline was re-run from the raw microdata and reproduced all published estimates (differences < 1e-4, optimiser tolerance).

License: MIT.

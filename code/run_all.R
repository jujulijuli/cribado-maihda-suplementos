# Ejecuta el flujo completo desde la carpeta code/ (directorio de trabajo).
# Requiere los microdatos originales (INE / Ministerio de Sanidad) en la ruta definida en R/00_config.R.
# 7 encuestas: EESE 2009, ENSE 2011, EESE 2014, ENSE 2017, EESE 2020, ESdE 2023, ESdE 2025
dir.create("output/figuras", recursive = TRUE, showWarnings = FALSE)
source("R/01_cargar_datos.R")              # lectura de microdatos (7 encuestas)
source("R/02_preparar_datos_v5.R")         # armonización, estratos -> output/olas_preparadas_v5.rds
source("R/04_prevalencias.R")              # prevalencias ponderadas -> output/tabla_prevalencias.csv
source("R/03_analisis_maihda_v5.R")        # MAIHDA cruzados nulo/ajustado -> tabla_vpc_v5, tabla_or_v5, tabla_blups_v5
source("R/03b_bootstrap_vpc_v5.R")         # IC bootstrap (200 réplicas) -> tabla_vpc_v5.csv (con IC)
source("R/03c_descomposicion_pesos_v5.R")  # descomposición efecto peso -> tabla suplementaria S2
source("R/05_figuras_tablas_v5.R")         # Tabla 2, Figura 2 (con IC), Excel, figuras interactivas S1/S2
source("R/06_tabla1_dataset_v5.R")         # Tabla 1 y prevalencias por ola
source("R/07_fig1_fig3prob_v5.R")          # Figura 1 y Figura 3 (escala de probabilidad)
source("R/10_smaihda_estrato_vs_celda_v5.R") # Tabla suplementaria S3: S-MAIHDA por estrato
source("R/11_tablas_suplementarias_v5.R")  # Tablas suplementarias S1–S4
source("08_figura_S3_brechas_v5.R")        # Figura S3 (brechas absolutas por componente, 7 olas)

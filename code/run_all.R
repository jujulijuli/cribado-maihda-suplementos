# Ejecuta el flujo completo desde la carpeta code/ (directorio de trabajo).
# Requiere los microdatos originales (INE / Ministerio de Sanidad) en la ruta definida en R/00_config.R.
# 7 encuestas: EESE 2009, ENSE 2011, EESE 2014, ENSE 2017, EESE 2020, ESdE 2023, ESdE 2025
dir.create("output/figuras", recursive = TRUE, showWarnings = FALSE)
source("R/01_cargar_datos.R")         # lectura de microdatos (7 encuestas)
source("R/02_preparar_datos_v4.R")    # armonización, estratos -> output/olas_preparadas_v3.rds
source("R/04_prevalencias.R")         # prevalencias ponderadas -> output/tabla_prevalencias.csv
source("R/03_analisis_maihda_v4.R")   # MAIHDA cruzados nulo/ajustado -> tabla_vpc_v5, tabla_or_v5, tabla_blups_prob_v5
source("R/03b_bootstrap_vpc.R")       # IC bootstrap (200 réplicas) -> tabla_vpc_v5.csv (con IC)
source("R/05_figuras_tablas_cc.R")    # Tabla 2, Figura 2 (con IC), Excel, figuras interactivas S1/S2
source("R/06_tabla1_dataset.R")       # Tabla 1
source("R/07_fig1_fig3prob_v5.R")     # Figura 1 y Figura 3/S2 (escala de probabilidad)
source("08_figura_S3_brechas_v5.R")   # Figura S3 (brechas absolutas por componente, 7 olas)

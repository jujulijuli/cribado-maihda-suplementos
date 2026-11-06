# Ejecuta el flujo completo desde la carpeta code/ (directorio de trabajo).
# Requiere los microdatos originales (INE / Ministerio de Sanidad) en la ruta definida en R/00_config.R.
dir.create("output/figuras", recursive = TRUE, showWarnings = FALSE)
source("R/01_cargar_datos.R")       # lectura de microdatos (6 encuestas)
source("R/02_preparar_datos_v4.R")  # armonización, estratos -> output/olas_preparadas_v3.rds
source("R/04_prevalencias.R")       # prevalencias ponderadas -> output/tabla_prevalencias.csv
source("R/03_analisis_maihda_v3.R") # MAIHDA cruzados nulo/ajustado -> tabla_vpc_cc, tabla_or_cc, tabla_blups_cc
source("R/05_figuras_tablas_cc.R")  # Tabla 2, Figura 2, Excel, figuras interactivas S1/S2
source("R/06_tabla1_dataset.R")     # Tabla 1
source("R/07_fig1_fig3prob.R")      # Figura 1 y Figura 3 (escala de probabilidad)

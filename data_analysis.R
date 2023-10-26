### Análisis de datos: COVID-19 en pacientes con cáncer incluidos en el Registro
### Institucional de Tumores de Argentina del HIGA Alende, Mar del Plata, año 2020:
### incidencia, gravedad y mortalidad
### Autor: Tamara Ricardo
### Última modificación: 2023-10-12

# Carga paquetes ----------------------------------------------------------
pacman::p_load(
  # Statistical analysis
  glmmTMB, 
  survival,
  # Inference and residuals
  DHARMa,
  performance,
  # Exploratory analysis
  compareGroups,
  gtsummary,
  dlookr,
  # Read files
  rio,
  # Data cleaning
  janitor,
  # Data management
  tidyverse
)

# Carga datos -------------------------------------------------------------
data <- import("datos_CA_COVID_clean.xlsx") %>% 
  ### Variables categóricas a factor
  mutate(across(where(is.character)|matches("idpte"),
                as.factor))


# Crea base para análisis supervivencia -----------------------------------
data_surv <- data %>% select(idpte, dx_covid_estudio, meses_CA1_covid) %>% 
  mutate(dx_covid_estudio = if_else(dx_covid_estudio=="Si", 1, 0))

# Análisis exploratorio datos ---------------------------------------------
### Explora valores faltantes
data %>% plot_na_pareto(only_na = T)

### Frecuencia variables muestra completa
data %>% select(edad, edad_cat, sexo, starts_with("comorb"), icd10_cat_1, 
              tipo_CA_1, evol_CA_1, ecog, gravedad_CA, metastasis, mas_un_tumor, 
              trat_quimio, trat_rtx, trat_quimio_rtx, n_ciclos_quimio, dx_covid_estudio) %>% 
  tbl_summary(missing = "no")

### Test asociación según diagnóstico de COVID
data %>% select(edad, edad_cat, sexo, 
                comorb_met, comorb_cvg, comorb_res, comorb_hiv, comorb_alc, comorb_tab,
                tipo_CA_1, evol_CA_1, ecog, gravedad_CA, metastasis, mas_un_tumor, 
                trat_quimio, trat_rtx, trat_quimio_rtx, dx_covid_estudio) %>% 
  tbl_summary(by = dx_covid_estudio, percent = "col", missing = "no") %>% 
  add_p() %>% 
  bold_p() %>% 
  bold_labels()


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
df = import("datos_CA_COVID_clean.xlsx") %>% 
  ### Variables categóricas a factor
  mutate(across(where(is.character)|matches("idpte"),
                as.factor))

# Análisis exploratorio datos ---------------------------------------------
### Explora valores faltantes
df %>% plot_na_pareto(only_na = T)

### Frecuencia variables muestra completa
df %>% select(edad, edad_cat, sexo, starts_with("comorb"), icd10_cat_1, 
              tipo_CA_1, evol_CA_1, ecog, gravedad_CA, metastasis, 
              mas_un_tumor, cat_dx_covid) %>% 
  tbl_summary(missing = "no")

### Frecuencia variables según diagnóstico COVID
df %>% select(edad, edad_cat, sexo, starts_with("comorb"),  
              tipo_CA_1, evol_CA_1, ecog, gravedad_CA, metastasis, 
              mas_un_tumor, cat_dx_covid) %>% 
  tbl_summary(by = cat_dx_covid, percent = "col", missing = "no") %>% 
  add_p() %>% 
  bold_p() %>% 
  bold_labels()

# Filtra datos con información comorbilidades -----------------------------
df_comorb <- df %>% select(idpte, edad, edad_cat, sexo, starts_with("comorb"),
                          icd10_cat_1, tipo_CA_1, evol_CA_1, ecog, gravedad_CA, 
                          metastasis, mas_un_tumor, cat_dx_covid) %>% 
  drop_na()

### Frecuencia variables según diagnóstico COVID
df %>% select(edad, edad_cat, sexo, starts_with("comorb"),  
              tipo_CA_1, evol_CA_1, ecog, gravedad_CA, metastasis, 
              mas_un_tumor, cat_dx_covid) %>% 
  tbl_summary(by = cat_dx_covid, percent = "col", missing = "no") %>% 
  add_p() %>% 
  bold_p() %>% 
  bold_labels()

### Regresión logística univariada
fit = glm(cat_dx_covid ~ edad, data = df_comorb, family = "binomial")

tbl_regression(fit, exponentiate = T) # P = 0.14

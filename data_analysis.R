### Análisis de datos: COVID-19 en pacientes con cáncer incluidos en el Registro
### Institucional de Tumores de Argentina del HIGA Alende, Mar del Plata, año 2020:
### incidencia, gravedad y mortalidad
### Autor: Tamara Ricardo
### Última modificación: 2023-10-04

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
df = import("base_cov_cancer.xlsx") %>% 
  ### Variables categóricas a factor
  mutate(across(where(is.character)|matches("idpte")|matches("ecog_cat"),
                as.factor))

# Análisis exploratorio datos ---------------------------------------------
### Explora valores faltantes
df %>% plot_na_pareto(only_na = T)

### Frecuencia variables muestra completa
df %>% select(edad, edad_cat, sexo, starts_with("comorb"), icd10_cat_1, 
              tipo_CA_1, evol_CA_1, ecog_cat, grav_CA, metast_CA, 
              dos_mas_CA, dx_covid) %>% 
  tbl_summary(missing = "no")

### Frecuencia variables según diagnóstico COVID
df %>% select(edad, edad_cat, sexo, starts_with("comorb"),  
              tipo_CA_1, evol_CA_1, ecog_cat, grav_CA, metast_CA, 
              mas_un_CA, dx_covid) %>% 
  tbl_summary(by = dx_covid, percent = "col", missing = "no") %>% 
  add_p() %>% 
  bold_p() %>% 
  bold_labels()

# Filtra datos con información comorbilidades -----------------------------
df_comorb <- df %>% select(idpte, edad, edad_cat, sexo, starts_with("comorb"),
                          icd10_cat_1, tipo_CA_1, evol_CA_1, ecog_cat, grav_CA, 
                          metast_CA, dos_mas_CA, dx_covid) %>% 
  drop_na()


### Regresiones logísticas univariadas
df_comorb %>% select(edad, edad_cat, sexo, starts_with("comorb"),  
              tipo_CA_1, evol_CA_1, ecog_cat, grav_CA, metast_CA, 
              dos_mas_CA, dx_covid) %>% 
  tbl_uvregression(y = dx_covid, method = glm, exponentiate = T,
                   method.args = list(family = "binomial")) %>% 
  bold_labels() %>% 
  bold_p()
  
# Incidencia COVID-19 -----------------------------------------------------
### Agrupa datos
df_incid <- df %>% 
  count(edad_cat, sexo, tipo_CA_1, grav_CA,
                            metast_CA, mas_un_CA, dx_covid) %>% 
  filter(dx_covid=="POS")
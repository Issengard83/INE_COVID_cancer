### Limpieza de datos: COVID-19 en pacientes con cáncer incluidos en el Registro
### Institucional de Tumores de Argentina del HIGA Alende, Mar del Plata, 
### año 2020: incidencia, gravedad y mortalidad
### Autor: Tamara Ricardo
### Última modificación: 2023-10-12

# Carga paquetes ----------------------------------------------------------
pacman::p_load(
  # Read files
  rio,
  # Age categories
  epikit,
  # Data cleaning
  janitor,
  # Data management
  tidyverse
)

# Carga categorías icd10 --------------------------------------------------
icd10 <- icd.data::icd10cm2016 %>% 
  # Cambia nombre variables
  rename(icd10_cat = sub_chapter) %>% 
  # Filtra datos cáncer
  filter(grepl("C", three_digit)) %>% 
  # Elimina duplicados  
  distinct_at("three_digit", .keep_all = T)


# Carga fechas diagnóstico ------------------------------------------------
fechas_dx <- import("raw/Base CC Tamara.xlsx", na = "NA") %>% 
  # Limpia nombres variables
  clean_names() %>% 
  rename(fecha_nac = fenac) %>% 
  # Agrupa datos
  group_by(idpte, fecha_nac) %>% 
  mutate(n = if_else(duplicated(idpte), 2, 1)) %>% 
  # Crea columna para fecha segundo diagnóstico
  pivot_wider(values_from = fedg,
              names_from = n,
              names_prefix = "fecha_dx_CA_") %>% 
  # Filtra fechas diagnóstico posteriores a marzo 2020
  filter(fecha_dx_CA_1<"2020-03-21")

# # Guarda datos limpios
# export(fechas_dx_clean, "fechas_dx.xlsx")

# Carga datos pacientes ---------------------------------------------------
data_raw <- import("raw/Base CC.xlsx", 
                   ## Define valores datos ausentes
                   na = c("NA","N/D","Seleccione...", "No aplica",
                          "Desconocido", 999, -999))

# Limpia datos pacientes --------------------------------------------------
data_clean <- data_raw %>% 
  ## Limpia nombres de variables
  clean_names() %>% 
  ## Modifica nombres de variables
  rename_with(.cols = starts_with("co") & !matches("compn"), 
              .fn = ~str_replace_all(.x, "co", "comorb_")) %>% 
  rename(sexo = ptesxn_x,
         evol_CA = evolucion,
         icd10_cod = tpgf,
         icd10_desc = tpgfn,
         tipo_CA = tipo_de_cancer,
         fecha_dx_covid = fecha_apertura,
         cat_dx_covid = clasificacion_manual) %>% 
  ## Filtra casos de tumores benignos
  filter(compn!="Benigno" ) %>% 
  ## Filtra casos COVID posteriores a diciembre 2020
  filter(fecha_dx_covid<"2021-01-01"|is.na(fecha_dx_covid)) %>% 
  ## Filtra casos con múltiples duplicados o errores de carga
  filter(idpte!="87095" & !(idpte=="100779" & grepl("[Dd]esc", cat_dx_covid))) %>% 
  ## Elimina columnas innecesarias
  select(-ptenm, -pteap, -doc, -ndoc, -dged, -mfgn, -compn, -difhi, -difhin)

# Modifica niveles variables ----------------------------------------------
data_clean <- data_clean %>% 
  ## Cambia niveles sexo biológico
  mutate_at("sexo", 
            .funs = ~ if_else(.x=="Mujer", "Femenino", "Masculino")) %>% 
  ## Tipo de tumor
  mutate_at(.vars = "tipo_CA", 
            .funs = ~ if_else(grepl("S", .x), "Sólido", "Hematológico")) %>% 
  ## Comorbilidades
  mutate_at(vars(starts_with("co")), .funs = ~if_else(.x==1, "Si", "No")) %>% 
  ## Código topografía icd10
  mutate_at(.vars = "icd10_cod",
            .funs = ~ str_sub(.x, 1, 3)) %>% 
  ## Convierte ecog a numérica
  mutate_at("ecog", .funs = ~ str_sub(.x, 1, 1) %>% as.numeric()) %>% 
  ## Categoriza diagnóstico COVID
  mutate_at("cat_dx_covid", 
            .funs = ~ if_else(grepl("[Cc]onf", .x), "POS", "NEG")) %>% 
  ## Añade categorías ICD10
  left_join(., icd10 %>% select(three_digit, icd10_cat),
            by = c("icd10_cod"="three_digit")) %>% 
  # Corrige nivel faltante icd10
  mutate_at("icd10_cat", .funs = ~if_else(
    icd10_cod=="C42", "Leukemias and related conditions", str_to_sentence(.x))) %>% 
  ## Variables categóricas a factor
  mutate(across(where(is.character), as.factor))

# Quita duplicados --------------------------------------------------------
data_clean_nd <- data_clean %>% 
  ## Pacientes con más de una muestra para COVID
  distinct(idpte, sexo, across(starts_with("comorb")), 
           across(starts_with("icd")), tipo_CA, evol_CA, ecog, .keep_all = T) %>% 
  ## Agrupa datos
  group_by(idpte, sexo, across(starts_with("comorb")), cat_dx_covid, fecha_dx_covid) %>% 
  mutate(count = if_else(!duplicated(idpte), 1, 2)) %>% 
  ## Columna para segundo tumor
  pivot_wider(names_from = count,
              values_from = c(icd10_cod, icd10_cat, icd10_desc, 
                              tipo_CA, evol_CA, ecog)) %>% 
  ## Filtra pacientes sin fecha de diagnóstico cáncer
  inner_join(fechas_dx) %>% 
  ## Desagrupa datos
  ungroup() %>% 
  ## Descarta niveles vacíos
  mutate_if(is.factor, fct_drop) %>% 
  ## Crea nuevas variables
  mutate(
    # Edad y grupo etario
    edad = 2020-year(fecha_nac),
    edad_cat = age_categories(edad, lower = 0, upper = 80, by = 20),
    # Gravedad del cáncer
    gravedad_CA = if_else(grepl("Dist|Reg", evol_CA_1)|grepl("Dist|Reg", evol_CA_2),
                          "Grave", "Leve"),
    mas_un_tumor = if_else(!is.na(fecha_dx_CA_2), "Si", "No"),
    metastasis = if_else(grepl("Dist", evol_CA_1)|grepl("Dist", evol_CA_2),
                         "Si", "No"),
    # Tiempo entre diagnóstico cáncer principal y COVID
    meses_dx_covid = interval(fecha_dx_CA_1, fecha_dx_covid) %>% 
      as.duration() %/% dmonths()) %>% 
  ## Promedia ecog
  rowwise() %>% mutate(ecog = if_else(is.na(ecog_1), 
                                      NA, max(c(ecog_1, ecog_2), na.rm = T))) %>% 
  ## Ordena variables
  select(idpte, fecha_nac, edad, edad_cat, sexo, starts_with("comorb"),
         fecha_dx_CA_1, ends_with("_1"), fecha_dx_CA_2, ends_with("_2"), ecog, 
         mas_un_tumor, metastasis, gravedad_CA, 
         fecha_dx_covid, cat_dx_covid, meses_dx_covid, -ecog_1, -ecog_2)

# Guarda datos ------------------------------------------------------------
export(data_clean_nd, "datos_CA_COVID_clean.xlsx")

### Limpia environment
rm(list = ls())

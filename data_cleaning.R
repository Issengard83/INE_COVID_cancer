### Limpieza de datos: COVID-19 en pacientes con cáncer incluidos en el Registro
### Institucional de Tumores de Argentina del HIGA Alende, Mar del Plata, 
### año 2020: incidencia, gravedad y mortalidad
### Autor: Tamara Ricardo
### Última modificación:
# Tue Oct 24 11:21:50 2023 ------------------------------

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


# Carga y limpia categorías ICD10 -----------------------------------------
icd10 <- icd.data::icd10cm2016 %>% 
  # Cambia nombre variables
  rename(icd10_cat = sub_chapter) %>% 
  # Filtra datos cáncer
  filter(grepl("C", three_digit)) %>% 
  # Elimina duplicados  
  distinct_at("three_digit", .keep_all = T)


# Carga y limpia fechas diagnóstico ---------------------------------------
fechas_dx <- import("raw/Base CC Tamara.xlsx", na = "NA") %>% 
  # Limpia nombres variables
  clean_names() %>% 
  rename(fecha_nacimiento = fenac) %>% 
  # Agrupa datos
  group_by(idpte, fecha_nacimiento) %>% 
  mutate(n = if_else(duplicated(idpte), 2, 1)) %>% 
  # Crea columna para fecha segundo diagnóstico
  pivot_wider(values_from = fedg,
              names_from = n,
              names_prefix = "fecha_dx_CA_") %>% 
  # Filtra fechas diagnóstico posteriores a marzo 2020
  filter(fecha_dx_CA_1<"2020-03-21")

# Carga y limpia datos tratamientos ---------------------------------------
trat_clean <- import("raw/pedido MDQ 2020 envío.xlsx", sheet = 1) %>% 
  ## Añade fechas radioterapia
  left_join(import("raw/pedido MDQ 2020 envío.xlsx", sheet = 2)) %>% 
  ## Limpia nombres variables
  clean_names() %>% 
  rename(fecha_ini_rtx = fecha_inicio_tratramiento,
         trat_rtx = radio,
         trat_quimio = quimio) %>% 
  ## Modifica niveles variables
  mutate_if(is.character, str_to_title) %>% 
  mutate_at("dni", as.numeric) %>%
  ## Crea variable para quimio y radioterapia
  mutate(trat_quimio_rtx = if_else(trat_quimio=="Si" & trat_rtx=="Si", "Si", "No")) %>% 
  ## Filtra datos fuera del período 2018-2020
  filter((fecha_ini_rtx>="2018-01-01" & fecha_ini_rtx<="2020-12-31")|is.na(fecha_ini_rtx)) %>% 
  ## Filtra variables innecesarias
  select(-sexo, -tipo_radioterapia) %>% 
  ## Filtra duplicados
  distinct() %>% 
  filter(!(dni=="94114884" & trat_quimio=="No")) %>% 
  ## Añade filas nuevas
  bind_rows(
    tibble(dni = c(12363058, 24914304, 28295672, 40301188),
           trat_quimio = rep("Si", 4),
           trat_rtx = rep("Si", 4),
           trat_quimio_rtx = rep("Si", 4),
           fecha_ini_rtx = c("2020-09-10", "2020-09-10", "2020-10-14", "2020-12-10") %>% 
             convert_to_date()
  ))

# Carga y limpia datos quimioterapia --------------------------------------
quim_clean <- import("raw/pedido MDQ 2020 envío.xlsx", sheet = 4) %>% 
  # Limpia nombres variables
  clean_names() %>% 
  rename(dni = nro_doc) %>% 
  # Corrige formato fechas
  mutate_at("fecha_carga", .funs = ~ convert_to_date(.x)) %>% 
  # Modifica niveles variables
  mutate_if(is.character, str_to_title) %>% 
  mutate_at("dni", as.numeric) %>%
  # Filtra fechas tratamiento fuera del período 2018-2020
  filter(fecha_carga>="2018-01-01" & fecha_carga<="2020-12-31") %>%
  # Filtra variables innecesarias
  select(dni, fecha_carga) %>% 
  # Quita duplicados
  distinct()

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
         dni = ndoc,
         evol_CA = evolucion,
         icd10_cod = tpgf,
         icd10_desc = tpgfn,
         tipo_CA = tipo_de_cancer,
         fecha_dx_covid = fecha_apertura,
         dx_covid_estudio = clasificacion_manual) %>% 
  ## Filtra casos de tumores benignos
  filter(compn!="Benigno" ) %>% 
  ## Filtra casos COVID posteriores a diciembre 2020
  filter(fecha_dx_covid<="2020-12-31"|is.na(fecha_dx_covid)) %>% 
  ## Filtra casos con múltiples duplicados o errores de carga
  filter(idpte!="87095" & !(idpte=="100779" & grepl("[Dd]esc", dx_covid_estudio)))

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
  mutate_at("dx_covid_estudio", 
            .funs = ~ if_else(grepl("[Cc]onf", .x), "Si", "No")) 


# Agrupa datos quimio -----------------------------------------------------
## Crea base fechas dx covid
quim_clean_gr <- quim_clean%>%  
  # Añade fechas covid
  left_join(data_clean %>% filter(dx_covid_estudio=="Si") %>% 
              select(dni, fecha_dx_covid)) %>% 
  # Filtra quimios posteriores a diagnóstico covid
  filter(!fecha_carga>fecha_dx_covid|is.na(fecha_dx_covid)) %>% 
  # Agrupa datos
  group_by(dni) %>% 
  # Crea nuevas variables
  # Crea variables nuevas
  summarise(fecha_ini_quimio = min(fecha_carga, na.rm = T),
            fecha_ult_quimio = max(fecha_carga, na.rm = T),
            fecha_dx_covid = max(fecha_dx_covid),
            meses_covid_quimio = interval(fecha_ult_quimio, fecha_dx_covid) %>% 
              as.duration() %/% dmonths(),
            n_ciclos_quimio = n()) %>% 
  mutate(covid_30d_quimio = if_else(meses_covid_quimio<=1, "Si", "No"))


# Añade datos ICD10 y tratamientos ----------------------------------------
data_clean <- data_clean %>% 
  ## Añade categorías ICD10
  left_join(., icd10 %>% select(three_digit, icd10_cat),
            by = c("icd10_cod"="three_digit")) %>% 
  # Corrige nivel faltante icd10
  mutate_at("icd10_cat", .funs = ~if_else(
    icd10_cod=="C42", "Leukemias and related conditions", str_to_sentence(.x))) %>% 
  ## Añade datos tratamientos cáncer y fechas radioterapia
  left_join(., trat_clean) %>% 
  ## Añade datos quimio
  left_join(., quim_clean_gr) %>% 
  ## Variables categóricas a factor
  mutate(across(where(is.character), as.factor))


# Quita duplicados --------------------------------------------------------
data_clean_nd <- data_clean %>% 
  ## Pacientes con más de una muestra para COVID
  distinct(idpte, sexo, across(starts_with("comorb")), 
           across(starts_with("icd")), tipo_CA, evol_CA, ecog, 
           across(ends_with("_quimio")), across(ends_with("_rtx")), .keep_all = T) %>% 
  ## Agrupa datos
  group_by(idpte, sexo, across(starts_with("comorb")), across(contains("quimio")), 
           across(contains("rtx")), dx_covid_estudio, fecha_dx_covid) %>% 
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
    edad = 2020-year(fecha_nacimiento),
    edad_cat = age_categories(edad, lower = 0, upper = 80, by = 20),
    # Gravedad del cáncer
    gravedad_CA = if_else(grepl("Dist|Reg", evol_CA_1)|grepl("Dist|Reg", evol_CA_2),
                          "Grave", "Leve"),
    mas_un_tumor = if_else(!is.na(fecha_dx_CA_2), "Si", "No"),
    metastasis = if_else(grepl("Dist", evol_CA_1)|grepl("Dist", evol_CA_2),
                         "Si", "No"),
    # Tiempo entre diagnóstico cáncer principal y COVID
    meses_CA1_covid = interval(fecha_dx_CA_1, fecha_dx_covid) %>% 
      as.duration() %/% dmonths()) %>% 
  ## Promedia ecog
  rowwise() %>% mutate(ecog = if_else(is.na(ecog_1), 
                                      NA, max(c(ecog_1, ecog_2), na.rm = T))) %>% 
  ## Ordena variables
  select(idpte, fecha_nacimiento, starts_with("edad"), sexo, starts_with("comorb"),
         fecha_dx_CA_1, ends_with("_1"), fecha_dx_CA_2, ends_with("_2"),
         ecog, mas_un_tumor, metastasis, gravedad_CA,
         trat_quimio, fecha_ini_quimio, fecha_ult_quimio, n_ciclos_quimio,
         ends_with("_rtx"), fecha_dx_covid, dx_covid_estudio, meses_CA1_covid,
         meses_covid_quimio, covid_30d_quimio, -ecog_1, -ecog_2)

# Guarda datos ------------------------------------------------------------
export(data_clean_nd, "datos_CA_COVID_clean.xlsx")


# # Diccionario de datos ----------------------------------------------------
# ## Crea nuevo diccionario de datos
# desc_data_clean <- tibble(variable = colnames(data_clean_nd), 
#                           descripcion = NA_character_)
# 
# # Guarda
# export(desc_data_clean, "diccionario_datos_clean.xlsx")

### Limpia environment
rm(list = ls())

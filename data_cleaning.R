### Limpieza de datos: COVID-19 en pacientes con cáncer incluidos en el Registro
### Institucional de Tumores de Argentina del HIGA Alende, Mar del Plata, 
### año 2020: incidencia, gravedad y mortalidad
### Autor: Tamara Ricardo
### Última modificación:
# Tue Nov 21 12:22:27 2023 ------------------------------

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


# Carga/limpia datos pacientes --------------------------------------------
### Datos crudos
data_raw <- import("raw/Base CC.xlsx", 
                   ## Define valores datos ausentes
                   na = c("NA","N/D","Seleccione...", "No aplica",
                          "Desconocido", 999, -999))

### Datos limpios
data_clean <- data_raw %>% 
  ## Limpia nombres de variables
  clean_names() %>% 
  ## Renombra variables
  rename_with(.cols = starts_with("co") & !matches("compn"), 
              .fn = ~str_replace_all(.x, "co", "comorb_")) %>% 
  rename(sexo = ptesxn_x,
         dni = ndoc,
         evol_CA = evolucion,
         icd10_cod = tpgf,
         icd10_desc = tpgfn,
         tipo_CA = tipo_de_cancer,
         fecha_dx_covid = fecha_apertura,
         dx_covid = clasificacion_manual) %>% 
  ## Filtra casos de tumores benignos
  filter(compn!="Benigno" ) %>% 
  ## Filtra casos con múltiples duplicados o errores de carga
  filter(idpte!="87095" & !(idpte=="100779" & grepl("[Dd]esc", dx_covid))) %>% 
  ## Modifica niveles sexo biológico
  mutate_at("sexo", .funs = ~if_else(.x=="Mujer", "Femenino", "Masculino")) %>% 
  ## Modifica niveles tipo de tumor
  mutate_at(.vars = "tipo_CA", 
            .funs = ~if_else(grepl("S", .x), "Sólido", "Hematológico")) %>% 
  ## Modifica niveles comorbilidades
  mutate_at(vars(starts_with("co")), .funs = ~if_else(.x==1, 1, 0)) %>%
  ## Código morfología icdO3
  separate(mfgn, into = c("icdO3_cod", "icdO3_cat"), sep = "-", extra = "merge") %>% 
  mutate_at("icdO3_cod", .funs = ~str_sub(.x, start = 2, end = 5)) %>%
  ## Código topografía icd10
  mutate_at(.vars = "icd10_cod", .funs = ~str_sub(.x, start = 1, end = 3)) %>% 
  ## Ecog numérica
  mutate(ecog_num = str_sub(ecog, start = 1, end = 1) %>% as.numeric()) %>% 
  ## Modifica niveles diagnóstico de COVID
  mutate_at("dx_covid", .funs = ~if_else(grepl("[Cc]onf", .x), "Si", "No")) %>% 
  ## COVID durante el estudio
  mutate(dx_covid_estudio = if_else(dx_covid=="Si" & year(fecha_dx_covid)==2020,
                                    "Si", "No"))

# Carga/limpia fechas dx cáncer -------------------------------------------
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
              names_prefix = "fecha_dx_CA") %>% 
  # Filtra fechas diagnóstico posteriores a marzo 2020
  filter(fecha_dx_CA1<"2020-03-03")


# Carga/limpia categorías icd10 -------------------------------------------
icd10 <- icd.data::icd10cm2016 %>% 
  # Filtra datos cáncer
  filter(chapter=="Neoplasms") %>% 
  # Elimina duplicados  
  distinct_at("three_digit", .keep_all = T) %>% 
  droplevels()


# Carga/limpia datos tratamientos -----------------------------------------
### Radioterapia
trat_rtx <- import("raw/pedido MDQ 2020 envío.xlsx", sheet = 2) %>% 
  select(-SEXO) %>% 
  ## Limpia nombres variables
  clean_names() %>% 
  rename(trat_quimio = quimio,
         trat_rtx = radio,
         fecha_ini_rtx = fecha_inicio_tratramiento) %>% 
  ## Corrige formato variables
  mutate_at("fecha_ini_rtx", .funs = ~convert_to_date(.x)) %>% 
  mutate_at("dni", as.numeric) %>% 
  mutate_if(is.character, str_to_title)

### Quimioterapia
trat_quim <- import("raw/pedido MDQ 2020 envío.xlsx", sheet = 4) %>% 
  ## Limpia nombres variables
  clean_names() %>% 
  rename(dni = nro_doc) %>% 
  ## Corrige formato variables
  mutate_at("fecha_carga", .funs = ~convert_to_date(.x)) %>% 
  mutate_at("dni", as.numeric) %>% 
  mutate_if(is.character, str_to_title) %>% 
  ## Quita duplicados
  select(dni, edad_prescripcion, fecha_carga) %>% 
  distinct() %>% 
  # ## Filtra fechas tratamiento fuera del período 2018-2020
  # filter(fecha_carga>="2018-01-01" & fecha_carga<="2020-12-31") %>% 
  ## Filtra fechas tratamiento fuera del período 2020
  filter(fecha_carga>="2020-01-01" & fecha_carga<="2020-12-31") %>% 
  # ## Días entre quimio y diagnóstico COVID
  # mutate(dias_quim_covid = if_else(fecha_dx_covid>fecha_carga,
  #                                  interval(fecha_carga, fecha_dx_covid) %>% 
  #                                    as.duration() %/% ddays(), NA)) %>% 
  ## Agrupa datos
  group_by(dni) %>% 
  ## Sumariza datos
  summarise(edad_ini_quimio = min(edad_prescripcion, na.rm = T),
            fecha_ini_quimio = min(fecha_carga, na.rm = T),
            fecha_fin_quimio = max(fecha_carga, na.rm = T),
            ciclos_quimio = n())

### Une bases tratamientos
trat_quim_rtx <- import("raw/pedido MDQ 2020 envío.xlsx", sheet = 1) %>% 
  select(-SEXO) %>% 
  ## Limpia nombres variables
  clean_names() %>% 
  rename(trat_quimio = quimio,
         trat_rtx = radio) %>% 
  ## Corrige formato variables
  mutate_at("dni", as.numeric) %>% 
  mutate_if(is.character, str_to_title) %>% 
  ## Elimina duplicados
  group_by(dni) %>% 
  filter(row_number() == 1) %>% 
  ## Añade datos quimioterapia
  left_join(trat_quim) %>% 
  ## Añade datos radioterapia
  left_join(trat_rtx) %>% 
  # Corrige fechas faltantes radioterapia
  mutate(fecha_ini_rtx = case_when(dni=="12363058" ~ ymd("2020-09-10"),
                                   dni=="24914304" ~ ymd("2020-09-19"),
                                   dni=="28295672" ~ ymd("2020-10-14"),
                                   dni=="40301188" ~ ymd("2020-12-10"),
                                   fecha_ini_rtx>ymd("2020-12-31") ~ NA,
                                   TRUE ~fecha_ini_rtx)) %>% 
  ## Crea variable para quimio y radioterapia
  mutate(trat_quimio_rtx = case_when(
    trat_quimio=="Si" & trat_rtx=="Si" ~ "Quimioterapia y radioterapia",
    trat_quimio=="Si" & trat_rtx=="No" ~ "Quimioterapia",
    trat_quimio=="No" & trat_rtx=="Si" ~ "Radioterapia",
    TRUE ~ "Ninguno"))

### Clean environment
rm(trat_quim, trat_rtx)


# Añade datos tratamientos y quita duplicados -----------------------------
data_clean_nd <- data_clean %>% 
  ## Añade categorías ICD10
  left_join(., icd10 %>% select(three_digit, sub_chapter),
            by = c("icd10_cod"="three_digit")) %>% 
  # Corrige nivel faltante icd10
  mutate(icd10_cat = ifelse(icd10_cod=="C42", 
                            "Leukemias and related conditions", 
                            str_to_sentence(sub_chapter))) %>% 
  ## Filtra pacientes sin fecha de diagnóstico cáncer
  inner_join(fechas_dx) %>% 
  ## Añade datos tratamientos cáncer y fechas
  inner_join(., trat_quim_rtx) %>%
  ## Variables categóricas a factor
  mutate(across(where(is.character)|matches("idpte"), .fns = as.factor)) %>% 
  ### Ordena por fecha diagnóstico COVID
  arrange(idpte, desc(dx_covid), desc(fecha_dx_covid)) %>% 
  ### Quita múltiples tests de COVID
  group_by(across(idpte:dni), fecha_nacimiento,
           across(starts_with("comorb")),
           across(starts_with("icd10")),
           across(starts_with("icdO3")),
           across(tipo_CA:ecog), 
           across(contains("quimio")),
           across(contains("rtx"))) %>% 
  filter(row_number()==1) %>% 
  ungroup() %>% 
  ### Corrige dato faltante evolución cáncer
  mutate(evol_CA = if_else(idpte=="83480" & is.na(evol_CA), 
                           "Localizado", evol_CA)) %>% 
  ### Sumariza datos de múltiples tumores
  reframe(icd10_cod1 = first(icd10_cod),
          icd10_cat1 = first(icd10_cat),
          icd10_desc1 = first(icd10_desc),
          icdO3_cod1 = first(icdO3_cod),
          icdO3_cat1 = first(icdO3_cat),
          tipo_CA1 = first(tipo_CA),
          evol_CA1 = first(evol_CA),
          icd10_cod2 = if_else(duplicated(idpte), last(icd10_cod), NA),
          icd10_cat2 = if_else(duplicated(idpte), last(icd10_cat), NA),
          icd10_desc2 = if_else(duplicated(idpte), last(icd10_desc), NA),
          icdO3_cod2 = if_else(duplicated(idpte), last(icdO3_cod), NA),
          icdO3_cat2 = if_else(duplicated(idpte), last(icdO3_cat), NA),
          tipo_CA2 = if_else(duplicated(idpte), last(tipo_CA), NA),
          evol_CA2 = if_else(duplicated(idpte), last(evol_CA), NA),
          ecog_num = max(ecog_num, na.rm = F),
          .by = c(idpte:dni, fecha_nacimiento,
                  starts_with("comorb"),
                  fecha_dx_CA1, fecha_dx_CA2,
                  contains("quimio"),
                  contains("rtx"),
                  contains("covid"))) %>% 
  ## Quita duplicados
  arrange(idpte, desc(dx_covid_estudio)) %>% 
  group_by(idpte) %>% 
  filter(row_number()==1) %>% 
  ungroup() %>% 
  ## Descarta niveles vacíos
  mutate_if(is.factor, fct_drop) 

# Crea nuevas variables ---------------------------------------------------
data_clean_nv <- data_clean_nd %>% 
  ## Edad y grupo etario
  mutate(edad = 2020 - year(fecha_nacimiento),
         edad_cat = age_categories(edad, breakers = c(18, 40, 60, 80)),
         edad_dx = interval(fecha_nacimiento, fecha_dx_CA1) %>% 
           as.duration() %/% dyears()) %>% 
  ## Gravedad del cáncer
  mutate(
    mas_un_tumor = if_else(!is.na(fecha_dx_CA2), "Si", "No"), 
    metastasis = case_when(
      grepl("metas", icdO3_cat1) & !grepl("inc", icdO3_cat1) ~ "Si",
      grepl("metas", icdO3_cat2) & !grepl("inc", icdO3_cat2) ~ "Si",
      grepl("Dist", evol_CA1) | grepl("Dist", evol_CA2) ~ "Si",
      TRUE ~ "No"),
    gravedad_CA = if_else(mas_un_tumor=="Si"|
                            metastasis=="Si"|
                            grepl("Dist|Reg", evol_CA1), "Grave", "Leve")
  ) %>% 
  ## Tiempo entre diagnóstico de cáncer y diagnóstico COVID
  mutate(dias_CA_covid = if_else(dx_covid_estudio=="Si",
                                 interval(fecha_dx_CA1, fecha_dx_covid) %>% 
                                   as.duration() %/% ddays(), NA)) %>% 
  ## Tiempo entre última quimio y diagnóstico COVID
  mutate(dias_quim_covid = if_else(dx_covid_estudio=="Si",
                                   interval(fecha_fin_quimio, fecha_dx_covid) %>% 
                                     as.duration() %/% ddays(), NA)) %>% 
  ## Tiempo entre inicio pandemia y diagnóstico COVID
  mutate(dias_ini_covid = case_when(
    dx_covid_estudio=="No" ~ interval("2020-03-03", "2020-12-31") %>% as.duration() %/% ddays(),
    TRUE ~ interval("2020-03-03", fecha_dx_covid) %>% as.duration() %/% ddays()
  )) %>% 
  ## Formato fechas
  mutate(across(starts_with("fecha"), .fns = convert_to_date)) %>% 
  ## Comorbilidades
  rowwise() %>%
  mutate(comorb_na = sum(c_across(comorb_met:comorb_tab) %>% is.na),
         comorb_n = if_else(comorb_na<11, sum(c_across(comorb_met:comorb_tab), na.rm = T), NA),
         comorb_n_cat = case_when(
           comorb_n==0 ~ "Sin comorbilidades",
           comorb_n==1 ~ "Una comorbilidad",
           comorb_n>1 ~ "Dos o más comorbilidades",
           is.na(comorb_n) ~ "Desconocido"),
         across(c("comorb_met":"comorb_tab"), ~ if_else(.x==1, "Si", "No"))) %>% 
  ## Ordena variables
  select(idpte, fecha_nacimiento, edad, edad_cat, sexo, starts_with("comorb"), -comorb_na,
         fecha_dx_CA1, edad_dx, ends_with("1"), fecha_dx_CA2, ends_with("2"),
         ecog_num, mas_un_tumor, metastasis, gravedad_CA,
         ends_with("_rtx"), ends_with("_quimio"), trat_quimio_rtx,
         fecha_dx_covid, dx_covid, dx_covid_estudio, 
         dias_CA_covid, dias_quim_covid, dias_ini_covid)

# Guarda datos ------------------------------------------------------------
export(data_clean_nv, "datos_CA_COVID_clean.xlsx")


# # Diccionario de datos ----------------------------------------------------
# ## Crea nuevo diccionario de datos
# desc_data_clean <- tibble(variable = colnames(data_clean_nd),
#                           descripcion = NA_character_)
# 
# # Guarda
# export(desc_data_clean, "diccionario_datos_clean.xlsx")

### Limpia environment
rm(list = ls())

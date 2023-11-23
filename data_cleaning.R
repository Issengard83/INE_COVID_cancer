### Limpieza de datos: COVID-19 en pacientes con cáncer incluidos en el Registro
### Institucional de Tumores de Argentina del HIGA Alende, Mar del Plata, 
### año 2020: incidencia, gravedad y mortalidad
### Autor: Tamara Ricardo
### Última modificación:
# Wed Nov 22 14:16:46 2023 ------------------------------


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
  rename(sexo = ptesxn_x,
         dni = ndoc,
         co_metabolica = comet,
         co_cardiovascular = cocvg,
         co_respiratoria = cores,
         co_renal = corin,
         co_neurologica = coneu,
         co_HIV = cohiv,
         co_hepat_B = coheb,
         co_hepat_C = cohec,
         co_H_pylori = cohep,
         co_alcoholismo = coalc,
         co_tabaquismo = cotab,
         icd10_cod = tpgf,
         icd10_desc = tpgfn,
         tipo_cancer = tipo_de_cancer,
         evol_cancer = evolucion,
         fecha_dx_covid = fecha_apertura,
         dx_covid = clasificacion_manual) %>% 
  ## Filtra casos de tumores benignos
  filter(compn!="Benigno" ) %>% 
  ## Filtra casos con múltiples duplicados o errores de carga
  filter(idpte!="87095" & !(idpte=="100779" & grepl("[Dd]esc", dx_covid))) %>% 
  ## Modifica niveles sexo biológico
  mutate_at("sexo", .funs = ~if_else(.x=="Mujer", "Femenino", "Masculino")) %>% 
  ## Modifica niveles tipo de tumor
  mutate_at(.vars = "tipo_cancer", 
            .funs = ~if_else(grepl("S", .x), "Sólido", "Hematológico")) %>% 
  ## Modifica niveles comorbilidades
  mutate_at(vars(starts_with("co_")), .funs = ~if_else(.x==1, 1, 0)) %>%
  ## Código morfología icdo3
  separate(mfgn, into = c("icdo3_cod", "icdo3_desc"), sep = "-", extra = "merge") %>% 
  mutate_at("icdo3_cod", .funs = ~str_sub(.x, start = 2, end = 5)) %>%
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
              names_prefix = "fecha_dx_cancer_") %>% 
  # Filtra fechas diagnóstico posteriores a marzo 2020
  filter(fecha_dx_cancer_1<"2020-03-03")


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
  ## Filtra fechas tratamiento fuera del período 2020
  filter(fecha_carga>="2020-01-01" & fecha_carga<="2020-12-31") %>% 
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


# Añade datos tratamientos ------------------------------------------------
data_clean <- data_clean %>% 
  ## Filtra pacientes sin fecha diagnóstico cáncer
  inner_join(fechas_dx) %>% 
  ## Añade datos tratamientos cáncer y fechas
  inner_join(., trat_quim_rtx) %>%
  ## Variables categóricas a factor
  mutate(across(where(is.character)|matches("idpte"), .fns = as.factor)) %>% 
  ## Ordena por fecha diagnóstico COVID
  arrange(idpte, desc(dx_covid), desc(fecha_dx_covid))


# Quita duplicados covid --------------------------------------------------
data_clean_nd <- data_clean %>% 
  ## Quita duplicados test COVID
  group_by(across(idpte:dni), 
           fecha_nacimiento, 
           across(starts_with("co_")), 
           across(starts_with("icd")),
           across(tipo_cancer:ecog),
           across(contains("quimio")),
           across(contains("rtx"))) %>% 
  filter(row_number()==1) %>% 
  ungroup() %>% 
  ### Corrige dato faltante evolución cáncer
  mutate(evol_cancer = if_else(idpte=="83480" & is.na(evol_cancer), 
                             "Localizado", evol_cancer)) %>% 
  ### Corrige error de carga tipo cáncer
  mutate(tipo_cancer = if_else(icd10_cod=="C42", "Hematológico", 
                               tipo_cancer))


# Sumariza pacientes con múltiples tumores --------------------------------
data_clean_mt <- data_clean_nd %>% 
  reframe(
    ## Clasificación ICD10
    icd10_cod_1 = first(icd10_cod),
    icd10_cod_2 = if_else(duplicated(idpte), last(icd10_cod), NA),
    icd10_desc_1 = first(icd10_desc),
    icd10_desc_2 = if_else(duplicated(idpte), last(icd10_desc), NA),
    ## Clasificación icdo3
    icdo3_cod_1 = first(icdo3_cod),
    icdo3_cod_2 = if_else(duplicated(idpte), last(icdo3_cod), NA),
    icdo3_desc_1 = first(icdo3_desc),
    icdo3_desc_2 = if_else(duplicated(idpte), last(icdo3_desc), NA),
    ## Tipo tumor
    tipo_cancer = if_else(idpte %in% c("78952","91477"), 
                          "Ambos", first(tipo_cancer)) %>% as.factor(),
    ## Evolución del cáncer
    evol_cancer = first(evol_cancer) %>% as.factor(),
    ## Escala ECOG
    ecog_num = max(ecog_num, na.rm = F),
    ## Variables de agrupamiento
    .by = c(idpte:dni, 
            fecha_nacimiento,
            starts_with("co_"),
            fecha_dx_cancer_1, 
            fecha_dx_cancer_2,
            contains("quimio"),
            contains("rtx"),
            contains("covid"))) %>% 
  ## Quita duplicados
  arrange(idpte, desc(dx_covid_estudio), icd10_cod_2) %>% 
  group_by(idpte) %>% 
  mutate(n = row_number()) %>% 
  filter(n==1) %>% 
  ## Variables categóricas a factor
  mutate_if(is.factor, fct_drop)

### Limpia environment
rm(list = setdiff(ls(), "data_clean_mt"))

### Tabla exploratoria icd10
tab<- fct_count(data_clean_mt$icd10_cod_1) %>% 
  arrange(desc(f))

# Crea variables para el análisis -----------------------------------------
data_clean_nv <- data_clean_mt %>% 
  ## Edad y grupo etario
  mutate(edad = 2020 - year(fecha_nacimiento),
         edad_cat = age_categories(edad, breakers = c(18, 40, 60, 80)),
         edad_dx = interval(fecha_nacimiento, fecha_dx_cancer_1) %>% 
           as.duration() %/% dyears()) %>% 
  ## Gravedad del cáncer
  mutate(
    ## Más de un tumor
    mas_un_tumor = if_else(!is.na(icd10_cod_2), "Si", "No"),
    ## Metástasis
    metastasis = case_when(
      evol_cancer=="Distante" ~ "Si",
      grepl("metas", icdo3_desc_1) & !grepl("inc", icdo3_desc_1) ~ "Si",
      grepl("metas", icdo3_desc_2) & !grepl("inc", icdo3_desc_2) ~ "Si",
      TRUE ~ "No"),
    ## Cáncer severo
    gravedad = if_else(mas_un_tumor=="Si"|
                          metastasis=="Si"|
                          evol_cancer %in% c("Distante","Regionalizado")|
                          ecog_num>1,
                        "Grave", "Leve")) %>% 
  ## Tiempo entre diagnóstico de cáncer y diagnóstico COVID
  mutate(dias_cancer_covid = if_else(dx_covid_estudio=="Si",
                                 interval(fecha_dx_cancer_1, fecha_dx_covid) %>% 
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
  mutate(
    comorb_na = sum(c_across(co_metabolica:co_tabaquismo) %>% is.na),
    comorb_n = if_else(comorb_na<11, sum(c_across(co_metabolica:co_tabaquismo), na.rm = T), NA),
    comorb_n = case_when(
           comorb_n==0 ~ "Sin comorbilidades",
           comorb_n==1 ~ "Una comorbilidad",
           comorb_n>1 ~ "Dos o más comorbilidades",
           is.na(comorb_n) ~ "Desconocido"),
    across(c("co_metabolica":"co_tabaquismo"), ~ if_else(.x==1, "Si", "No"))) %>% 
  ## Clasificación ICD10
  mutate(icd10_loc_ppal = case_when(
    icd10_cod_1=="C16" ~ "Estómago",
    icd10_cod_1=="C18" ~ "Colon",
    icd10_cod_1=="C20" ~ "Recto",
    icd10_cod_1=="C34" ~ "Bronquio y pulmón",
    icd10_cod_1=="C44" ~ "Piel",
    icd10_cod_1=="C50" ~ "Mama",
    icd10_cod_1=="C53" ~ "Cuello uterino",
    icd10_cod_1=="C77" ~ "Ganglios linfáticos",
    icd10_cod_1 %in% c("C01","C02","C03","C06","C08","C11","C12","C13") ~ "Boca, cavidad bucal y faringe",
    icd10_cod_1 %in% c("C60","C61","C62") ~ "Genitales masculinos",
    icd10_cod_1 %in% c("C64","C68") ~ "Aparato urinario",
    icd10_cod_1 %in% c("C73","C75") ~ "Tiroides y glándulas endócrinas",
    TRUE ~ "Otro/s"
  ))


# Ordena variables --------------------------------------------------------
data_clean <- data_clean_nv %>% 
  select(idpte, sexo, fecha_nacimiento, edad, edad_cat, 
         starts_with("co_"), comorb_n, ends_with("_1"), ends_with("_2"), icd10_loc_ppal,
         tipo_cancer, evol_cancer, ecog_num, mas_un_tumor, metastasis, gravedad,
         ends_with("quimio"), ends_with("rtx"), contains("covid"))


# Guarda datos ------------------------------------------------------------
export(data_clean, "datos_CA_COVID_clean.xlsx")


### Limpia environment
rm(list = ls())

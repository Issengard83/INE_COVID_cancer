### Limpieza de datos: COVID-19 en pacientes con cáncer incluidos en el Registro
### Institucional de Tumores de Argentina del HIGA Alende, Mar del Plata, 
### año 2020: incidencia, gravedad y mortalidad
### Autor: Tamara Ricardo
### Última modificación:


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


# Carga datos RITA/SISA ---------------------------------------------------
data_rita_raw <- import("raw/rita con cruce sisa posta posta.xlsx", 
                        ## Define valores datos ausentes
                        na = c("NA","N/D","Seleccione...", "No aplica",
                               "Desconocido", 999, -999))

names(data_rita_raw)


# Selecciona variables relevantes -----------------------------------------
data_rita_clean <- data_rita_raw %>% 
  ### Limpia nombre de variables
  clean_names() %>% 
  ### Selecciona variables para el análisis
  select(
    ## Identificadores del paciente
    idpte, 
    dni = ndoc, 
    nombre = ptenm, 
    apellido = pteap, 
    sexo = ptesxn_x, 
    fecha_nacimiento = fenac,
    ## Comorbilidades
    com_metabolica = comet,
    com_diabetes = dbt,
    com_obesidad = obesidad,
    com_cardiovascular = cocvg,
    com_HTA = hta,
    com_respiratoria = cores,
    com_asma = asma,
    com_EPOC = epoc,
    com_renal = corin,
    com_dialisis = dialisis_cronica,
    com_neurologica = coneu,
    com_HIV = cohiv,
    com_hepat_B = coheb,
    com_hepat_C = cohec,
    com_H_pylori = cohep,
    com_HPV = cohpv,
    com_Epstein_Barr = coepbar,
    com_alcoholismo = coalc,
    com_tabaquismo = cotab,
    ## Clasificación del tumor
    fecha_dx_cancer = fedg,
    icd10_cod = tpgf,
    icd10_desc = tpgfn,
    icdo3_cod = mfg,
    icdo3_desc = mfgn,
    dif_histologica = difhin,
    tipo_cancer = tipo_de_cancer,
    evol_cancer = evolucion,
    comp_tumor = compn,
    ecog,
    ## Clasificación COVID
    fecha_ini_sint_covid = fis,
    fecha_dx_covid = fecha_apertura,
    dx_covid = clasif_resumen,
    internado_covid = internado, 
    fecha_inter_covid = fecha_internacion, 
    cui_int_covid = cuidado_intensivo, 
    fecha_cui_int_covid = fecha_cui_intensivos, 
    fallecido_covid = fallecido, 
    fecha_fallecimiento_covid = fecha_fallecimiento)


# Carga/limpia datos tratamientos -----------------------------------------
### Radioterapia
trat_rtx <- import("raw/pedido MDQ 2020 envío.xlsx", sheet = 2) %>% 
  select(-SEXO) %>% 
  ## Limpia nombres variables
  clean_names() %>% 
  rename(trat_quim = quimio,
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
  summarise(edad_ini_quim = min(edad_prescripcion, na.rm = T),
            fecha_ini_quim = min(fecha_carga, na.rm = T),
            fecha_fin_quim = max(fecha_carga, na.rm = T),
            ciclos_quim = n())

### Une bases tratamientos
trat_quim_rtx <- import("raw/pedido MDQ 2020 envío.xlsx", sheet = 1) %>% 
  select(-SEXO) %>% 
  ## Limpia nombres variables
  clean_names() %>% 
  rename(trat_quim = quimio,
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
  mutate(trat_quim_rtx = case_when(
    trat_quim=="Si" & trat_rtx=="Si" ~ "Quimioterapia y radioterapia",
    trat_quim=="Si" & trat_rtx=="No" ~ "Quimioterapia",
    trat_quim=="No" & trat_rtx=="Si" ~ "Radioterapia",
    TRUE ~ "Ninguno"))


# Limpia datos pacientes y añade tratamientos -----------------------------
data_rita_trt <- data_rita_clean %>% 
  ## Filtra casos sin fecha dx cáncer
  filter(!is.na(fecha_dx_cancer)) %>% 
  ## Filtra casos de tumores benignos
  filter(comp_tumor!="Benigno" ) %>% 
  ## Filtra casos con múltiples duplicados o errores de carga
  filter(idpte!="87095" & !(idpte=="100779" & grepl("[Dd]esc", dx_covid))) %>% 
  ## Modifica niveles sexo biológico
  mutate_at("sexo", 
            .funs = ~ if_else(.x=="Mujer", "Femenino", "Masculino")) %>% 
  ## Corrige formato fechas
  mutate_at(vars(starts_with("fecha")), 
            .funs = ~ convert_to_date(.x)) %>% 
  ## Modifica niveles comorbilidades
  mutate_at(vars(starts_with("com_")), 
            .funs = ~ if_else(.x==1, 1, 0)) %>% 
  ## Modifica niveles tipo de tumor
  mutate_at(.vars = "tipo_cancer", 
            .funs = ~ if_else(grepl("S", .x), "Sólido", "Hematológico")) %>% 
  ## Separa ECOG en categórica y numérica
  separate(ecog, into = c("ecog_num", "ecog_cat"), sep = "-", 
           extra = "merge", convert = T) %>% 
  ### Corrige dato faltante evolución cáncer
  mutate(evol_cancer = if_else(idpte=="83480" & is.na(evol_cancer), 
                               "Localizado", evol_cancer)) %>% 
  ### Corrige error de carga tipo cáncer
  mutate(tipo_cancer = if_else(grepl("C42", icd10_cod), "Hematológico", 
                               tipo_cancer)) %>% 
  ## COVID durante el estudio
  mutate(dx_covid_estudio = if_else(dx_covid=="Confirmado" &
                                      year(fecha_dx_covid)==2020, 
                                    1, 0, missing = 0)) %>% 
  ### Añade datos tratamientos cáncer
  inner_join(., trat_quim_rtx) %>%
  ### Variables categóricas a factor
  mutate(across(where(is.character)|matches("idpte"), .fns = as.factor)) %>% 
  ### Ordena por fecha diagnóstico COVID
  arrange(idpte, desc(dx_covid), desc(fecha_dx_covid))


# Quita duplicados --------------------------------------------------------
data_rita_nd <- data_rita_trt %>% 
  ### Quita duplicados test COVID
  group_by(across(idpte:ecog_cat),
           across(contains("quim")),
           across(contains("rtx"))) %>% 
  filter(row_number()==1) %>% 
  ungroup() %>% 
  ### Sumariza datos de pacientes con múltiples tumores
  reframe(
    ## Comorbilidades
    across(starts_with("com_"), .fns = ~ mean(.x, na.rm = T) %>% na_if(NaN)),
    ## Fecha diagnóstico cáncer
    fecha_dx_cancer_1 = first(fecha_dx_cancer),
    fecha_dx_cancer_2 = if_else(duplicated(idpte), last(fecha_dx_cancer), NA),
    ## Clasificación ICD10
    icd10_cod_1 = first(icd10_cod),
    icd10_desc_1 = first(icd10_desc),
    icd10_cod_2 = if_else(duplicated(idpte), last(icd10_cod), NA),
    icd10_desc_2 = if_else(duplicated(idpte), last(icd10_desc), NA),
    ## Clasificación icdo3
    icdo3_cod_1 = first(icdo3_cod),
    icdo3_desc_1 = first(icdo3_desc),
    icdo3_cod_2 = if_else(duplicated(idpte), last(icdo3_cod), NA),
    icdo3_desc_2 = if_else(duplicated(idpte), last(icdo3_desc), NA),
    ## Tipo tumor
    tipo_cancer = if_else(idpte %in% c("78952","91477"),
                          "Ambos", first(tipo_cancer)) %>% as.factor(),
    ## Evolución del cáncer
    evol_cancer = first(evol_cancer) %>% as.factor(),
    ## Escala ECOG
    ecog_num = max(ecog_num, na.rm = F),
    ## Variables de agrupamiento
    .by = c(idpte:fecha_nacimiento,
            contains("quim"),
            contains("rtx"),
            contains("covid"),
            )) %>% 
  ## Quita duplicados
  arrange(idpte, desc(dx_covid_estudio), icd10_cod_2) %>% 
  group_by(idpte) %>% 
  filter(row_number()==1) %>% 
  # mutate(n = row_number()) %>% 
  # filter(n==1) %>% 
  ungroup() %>% 
  ## Variables categóricas a factor
  mutate_if(is.factor, fct_drop)

### Tabla exploratoria icd10
tab<- fct_count(data_rita_nd$icd10_cod_1) %>% 
  arrange(desc(f))

# Crea nuevas variables explicativas --------------------------------------
data_rita_ve <- data_rita_nd %>% 
  ## Edad y grupo etario
  mutate(edad = 2020 - year(fecha_nacimiento),
         edad_cat = age_categories(edad, breakers = c(18, 40, 60, 80)),
         edad_dx_cancer = interval(fecha_nacimiento, fecha_dx_cancer_1) %>% 
           as.duration() %/% dyears()) %>% 
  ## Severidad del cáncer
  mutate(
    ## Más de un tumor
    mas_un_tumor = if_else(!is.na(icd10_cod_2), "Si", "No"),
    ## Metástasis
    metastasis = case_when(
      evol_cancer=="Distante" ~ "Si",
      grepl("metas", icdo3_desc_1) & !grepl("inc", icdo3_desc_1) ~ "Si",
      grepl("metas", icdo3_desc_2) & !grepl("inc", icdo3_desc_2) ~ "Si",
      TRUE ~ "No"),
    ## Gravedad
    gravedad = if_else(mas_un_tumor=="Si"|
                         metastasis=="Si"|
                         evol_cancer %in% c("Distante","Regionalizado")|
                         ecog_num>1,
                       "Grave", "Leve")) %>% 
  ## Tiempo entre diagnóstico de cáncer y diagnóstico COVID
  mutate(dias_cancer_covid = if_else(dx_covid_estudio==1,
                                     interval(fecha_dx_cancer_1, fecha_dx_covid) %>% 
                                       as.duration() %/% ddays(), NA)) %>% 
  ## Tiempo entre última quimio y diagnóstico COVID
  mutate(dias_quim_covid = if_else(dx_covid_estudio==1,
                                   interval(fecha_fin_quim, fecha_dx_covid) %>% 
                                     as.duration() %/% ddays(), NA)) %>% 
  ## Tiempo entre inicio pandemia y diagnóstico COVID
  mutate(dias_ini_pand_covid = case_when(
    dx_covid_estudio==0 ~ interval("2020-03-03", "2020-12-31") %>% as.duration() %/% ddays(),
    TRUE ~ interval("2020-03-03", fecha_dx_covid) %>% as.duration() %/% ddays()
  )) %>% 
  ## Comorbilidades
  rowwise() %>%
  mutate(
    # Datos faltantes comorbilidades
    com_na = sum(c_across(com_metabolica:com_tabaquismo) %>% is.na),
    # Cantidad de comorbilidades
    com_sum = if_else(com_na<19, 
                         sum(c_across(com_metabolica:com_tabaquismo), na.rm = T), NA),
    # Cantidad de comorbilidades (categorizada)
    com_cat = case_when(
      com_sum==0 & com_na==0 ~ "Sin comorbilidades",
      com_sum==1 ~ "Una comorbilidad",
      com_sum==2 ~ "Dos comorbilidades",
      com_sum>2 ~ "Más de dos comorbilidades",
      TRUE ~ "Desconocido"),
    # Categoriza presencia/ausencia comorbilidades
    across(c("com_metabolica":"com_tabaquismo"), ~ if_else(.x==1, "Si", "No"))) %>% 
  ## Clasificación ICD10
  mutate(loc_tumor_ppal = case_when(
    grepl("C16", icd10_cod_1) ~ "Estómago",
    grepl("C18", icd10_cod_1) ~ "Colon",
    grepl("C20", icd10_cod_1) ~ "Recto",
    grepl("C34", icd10_cod_1) ~ "Bronquio y pulmón",
    grepl("C44", icd10_cod_1) ~ "Piel",
    grepl("C50", icd10_cod_1) ~ "Mama", 
    grepl("C53", icd10_cod_1) ~ "Cuello uterino",
    grepl("C77", icd10_cod_1) ~ "Ganglios linfáticos",
    grepl("C01|C02|C03|C06|CO8|C11|C12|C13", icd10_cod_1) ~ "Boca, cavidad bucal y faringe",
    grepl("C60|C61|C62", icd10_cod_1) ~ "Genitales masculinos",
    grepl("C64|C68", icd10_cod_1) ~ "Aparato urinario",
    grepl("C73|C75", icd10_cod_1) ~ "Tiroides y glándulas endócrinas",
    TRUE ~ "Otros/No especificado"
  )) %>% 
  ### Ordena columnas
  select(idpte, fecha_nacimiento, edad, edad_cat, starts_with("com_"),
         ends_with("_1"), ends_with("_2"), loc_tumor_ppal, mas_un_tumor,
         metastasis, gravedad, ecog_num, 
         trat_quim, fecha_ini_quim:ciclos_quim,
         trat_rtx, fecha_ini_rtx, trat_quim_rtx,
         fecha_ini_sint_covid:dx_covid, dx_covid_estudio, contains("covid")) %>%
  ### Filtra diagnóstico de cáncer posterior al COVID
  filter(dias_cancer_covid>=0|is.na(dias_cancer_covid))

# Exporta base limpia -----------------------------------------------------
export(data_rita_ve, file = "data_RITA_covid_clean.xlsx")

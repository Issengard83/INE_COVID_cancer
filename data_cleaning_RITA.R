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
  # NA exploration
  dlookr,
  gtsummary,
  # Data cleaning
  janitor,
  # Data management
  tidyverse
)


# Carga datos RITA/SISA ---------------------------------------------------
data_rita_raw <- import("raw/rita con cruce sisa posta posta.xlsx", 
                        ## Define valores datos ausentes
                        na = c("NA", "ND", "N/D", "Seleccione...", "No aplica",
                               "cofaco", "", 999, -999))
  

## Tabla comorbilidades
data_rita_raw %>% 
  select(COMET:COTAB, COHPV, COEPBAR, ASMA:PREMATURO, TBC:DIALISIS_CRONICA) %>% 
  mutate_all(as.factor) %>% 
  tbl_summary()

# Selecciona variables relevantes -----------------------------------------
data_rita_clean <- data_rita_raw %>% 
  ### Limpia nombres variables
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
    com_HTA = hta,
    com_cardiovasc = cocvg, 
    com_asma_epoc = cores, asma, epoc,
    com_tabaquismo = cotab, fumador,
    com_alcoholismo = coalc,
    com_obesidad = obesidad,
    com_insf_renal = corin, insf_renal, dialisis_aguda, dialisis_cronica,
    com_hepat_cronica = coheb, cohec, hepato_cronica,
    com_oncol_previa = enf_onco_previa,
    com_HIV = cohiv,
    cohep,
    cohpv,
    coepbar,
    coneu, enf_neuro_previa,
    nac_previa,
    inmunos_congenita,
    
    ## Clasificación del tumor
    fecha_dx_cancer = fedg,
    icd10_cod = tpgf,
    icd10_desc = tpgfn,
    icdo3_cod = mfg,
    icdo3_desc = mfgn,
    tipo_cancer = tipo_de_cancer,
    evol_cancer = evolucion,
    comp_tumor = compn,
    ecog,
    fallecido,
    fecha_fallec = fecha_fallecimiento,
    
    ## Clasificación COVID
    fecha_ini_covid = fis,
    signo_sintoma,
    fecha_dx_covid = fecha_apertura,
    dx_covid = clasif_resumen,
    intern_covid = internado, 
    fecha_intern_covid = fecha_internacion, 
    UCI_covid = cuidado_intensivo, 
    fecha_UCI_covid = fecha_cui_intensivos, 
    fallec_covid = causa_fallecimiento_vinculada)


# Carga/limpia datos tratamientos -----------------------------------------
### Radioterapia
trat_rtx <- import("raw/pedido MDQ 2020 envío.xlsx", sheet = 2) %>% 
  select(-SEXO) %>% 
  ## Limpia nombres variables
  clean_names() %>% 
  rename(trat_quim_2020 = quimio,
         trat_rtx_2020 = radio,
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
  select(dni, edad_prescripcion, edad_actual, fecha_carga) %>% 
  distinct() %>% 
  
  ## Filtra fechas tratamiento fuera del período 2020
  filter(fecha_carga>="2020-01-01" & fecha_carga<="2020-12-31") %>% 
  
  ## Agrupa datos
  arrange(dni, fecha_carga) %>% 
  group_by(dni) %>% 
  
  ## Sumariza datos
  summarise(
    edad_ini_quim = min(edad_prescripcion),
    fecha_ini_quim_2020 = first(fecha_carga),
    fecha_fin_quim_2020 = last(fecha_carga),
    ciclos_quim_2020 = n())

### Une bases tratamientos
trat_quim_rtx <- import("raw/pedido MDQ 2020 envío.xlsx", sheet = 1) %>% 
  select(-SEXO) %>% 
  
  ## Limpia nombres variables
  clean_names() %>% 
  rename(trat_quim_2020 = quimio,
         trat_rtx_2020 = radio) %>% 
  
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
  mutate(fecha_ini_rtx_2020 = case_when(dni=="12363058" ~ ymd("2020-09-10"),
                                   dni=="24914304" ~ ymd("2020-09-19"),
                                   dni=="28295672" ~ ymd("2020-10-14"),
                                   dni=="40301188" ~ ymd("2020-12-10"),
                                   fecha_ini_rtx>ymd("2020-12-31") ~ NA,
                                   TRUE ~fecha_ini_rtx)) %>% 
  
  ## Crea variable para quimio y radioterapia
  mutate(trat_quim_rtx_2020 = case_when(
    trat_quim_2020=="Si" & trat_rtx_2020=="Si" ~ "Quimioterapia y radioterapia",
    trat_quim_2020=="Si" & trat_rtx_2020=="No" ~ "Quimioterapia",
    trat_quim_2020=="No" & trat_rtx_2020=="Si" ~ "Radioterapia",
    trat_quim_2020=="No" & trat_rtx_2020=="No" ~ "Ninguno"))


# Limpia datos pacientes --------------------------------------------------
data_rita_clean_trt <- data_rita_clean %>% 
  
  ## Filtra casos sin fecha dx cáncer
  filter(!is.na(fecha_dx_cancer)) %>% 
  
  ## Filtra casos de tumores benignos
  filter(comp_tumor!="Benigno") %>% 
  
  ## Filtra casos con múltiples duplicados o errores de carga
  filter(idpte!="87095" & !(idpte=="100779" & grepl("[Dd]esc", dx_covid))) %>% 
  
  ## Modifica niveles sexo biológico
  mutate_at("sexo", 
            .funs = ~ if_else(.x=="Mujer", "Femenino", "Masculino")) %>% 
  
  ## Corrige formato fechas
  mutate_at(vars(starts_with("fecha")), 
            .funs = ~ convert_to_date(.x)) %>% 
  
  ## Unifica categorías presencia/ausencia comorbilidades
  mutate(across(com_metabolica:inmunos_congenita, 
                .fns = ~ if_else(.x==1, 1, 0))) %>% 
  ## Corrige datos comorbilidades
  rowwise() %>% 
  mutate(
    # Enfermedad respiratoria
    com_asma_epoc = if_else(all(is.na(c_across(com_asma_epoc:epoc))),
                            NA, mean(c_across(com_asma_epoc:epoc), 
                                     na.rm = T) %>% ceiling()),
    
    # Tabaquismo
    com_tabaquismo = if_else(is.na(com_tabaquismo) & is.na(fumador), 
                             NA, mean(c_across(com_tabaquismo:fumador), 
                                      na.rm = T) %>% ceiling()),
    
    # Insuficiencia renal
    com_insf_renal = if_else(all(is.na(c_across(com_insf_renal:dialisis_cronica))),
                             NA, mean(c_across(com_insf_renal:dialisis_cronica), 
                                      na.rm = T) %>% ceiling()),
    
    # Enfermedad hepática crónica
    com_hepat_cronica = if_else(all(is.na(c_across(com_hepat_cronica:cohec))),
                                NA, mean(c_across(com_hepat_cronica:cohec), 
                                         na.rm = T) %>% ceiling()),
    
    # Otras comorbilidades
    com_otras = if_else(all(is.na(c_across(cohep:inmunos_congenita))),
                        NA, mean(c_across(cohep:inmunos_congenita), 
                                 na.rm = T) %>% ceiling()),
    
    # Número de comorbilidades
    com_n = if_else(all(is.na(c_across(starts_with("com_")))),
                    NA, sum(c_across(starts_with("com_")), na.rm = T))
  ) %>% 
  
  ## Corrige niveles tipo de tumor
  mutate_at(.vars = "tipo_cancer", 
            .funs = ~ if_else(grepl("[Hh]", tipo_cancer)|grepl("C42", icd10_cod), 
                              "Hematológico", "Sólido")) %>% 
  
  ## ECOG numérica
  mutate_at(.vars = "ecog",
            .funs = ~ if_else(.x=="Desconocido", NA, str_sub(ecog, 1, 1)) %>% as.numeric()) %>% 
  
  ### Corrige dato faltante evolución cáncer
  mutate(evol_cancer = if_else(idpte=="83480" & is.na(evol_cancer), 
                               "Localizado", evol_cancer)) %>% 
  
  # Añade datos tratamientos cáncer -----------------------------------------
  inner_join(., trat_quim_rtx) %>%
  
  ### Variables categóricas a factor
  mutate(across(where(is.character)|matches("idpte"), .fns = as.factor)) %>% 
  
  ### Descarta columnas innecesarias
  select(-asma, -epoc, -fumador, -insf_renal, -dialisis_aguda, -dialisis_cronica,
         -cohec, -hepato_cronica, -c(cohep:inmunos_congenita))


# Quita duplicados --------------------------------------------------------
data_rita_nd <- data_rita_clean_trt %>% 
  ### Ordena datos por fecha diagnóstico COVID
  arrange(idpte, dx_covid, fecha_dx_covid) %>% 
  ### Quita registros duplicados COVID
  group_by(across(idpte:fecha_fallec), across(com_otras:trat_quim_rtx_2020)) %>% 
  filter(row_number() ==1) %>% 
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
    ecog = max(ecog, na.rm = F),
    
    ## Variables de agrupamiento
    .by = c(idpte:fecha_nacimiento,
            contains("quim"),
            contains("rtx"),
            fallecido, fecha_fallec,
            contains("covid"),
    )) %>% 
  
  ## Quita duplicados
  arrange(idpte, dx_covid, icd10_cod_2) %>% 
  group_by(idpte) %>% 
  filter(row_number()==1) %>%
  ungroup() %>% 
  
  ## Descarta niveles vacíos
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
                         ecog>1,
                       "Grave", "Leve"),
    ## Años quimioterapia
    tiempo_quim = edad - edad_ini_quim) %>% 
  
  ## COVID durante el estudio
  mutate(dx_covid_estudio = if_else(dx_covid=="Confirmado" &
                                      year(fecha_dx_covid)==2020, 
                                    1, 0, missing = 0)) %>% 
  
  ## Fallecimientos
  mutate(
    # Fallecido durante el estudio
    fallec_estudio = if_else(fallecido=="SI" & year(fecha_fallec)==2020, 
                                    1, 0, missing = 0),
    # Fallecido por COVID durante el estudio
    fallec_covid_estudio = if_else(dx_covid_estudio==1 & fallec_covid=="SI",
                                   1, 0, missing = 0)) %>% 
  ## Comorbilidades
  mutate(
    # Categoriza número de comorbilidades
    com_n_cat = case_when(
      com_n==0 ~ "Sin comorbilidades",
      com_n==1 ~ "Una comorbilidad",
      com_n==2 ~ "Dos comorbilidades",
      com_n>2 ~ "Más de dos comorbilidades",
      TRUE ~ "Desconocido"),

    # Categoriza presencia/ausencia comorbilidades
    across(starts_with("com_"), ~ if_else(.x==1, "Si", "No"))) %>% 
  
  ## Categoriza sitio principal tumor (ICD10)
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
  
  ### Días entre dx cáncer y COVID
  mutate(dias_cancer_covid = interval(fecha_dx_cancer_1, fecha_dx_covid) %>% 
           as.duration() %/% ddays()) %>% 
  
  ### Filtra pacientes con dx de cáncer posterior al de COVID
  filter(dias_cancer_covid>=0|is.na(dias_cancer_covid))


############## actualizar a partir de esta línea #######################
#   ## Tiempo entre diagnóstico de cáncer y diagnóstico COVID
#   mutate(dias_cancer_covid = if_else(dx_covid_estudio==1,
#                                      interval(fecha_dx_cancer_1, fecha_dx_covid) %>% 
#                                        as.duration() %/% ddays(), NA)) %>%
#   
#   ## Tiempo entre inicio de la pandemia/ingreso a la cohorte y diagnóstico COVID
#   mutate(dias_ini_covid = case_when(
#     # Pacientes con diagnóstico de cáncer previo al inicio de la pandemia
#     fecha_dx_cancer_1<"2023-03-03" & dx_covid_estudio==0 ~ 
#       interval("2020-03-03", "2020-12-31") %>% as.duration() %/% ddays(), 
#     fecha_dx_cancer_1<"2023-03-03" & dx_covid_estudio==1 ~ 
#       interval("2020-03-03", fecha_dx_covid) %>% as.duration() %/% ddays(),
#     
#     # Pacientes con diagnóstico de cáncer posterior al inicio de la pandemia
#     fecha_dx_cancer_1>"2023-03-03" & dx_covid_estudio==0 ~ 
#       interval(fecha_dx_cancer_1 , "2020-12-31") %>% as.duration() %/% ddays(),
#     fecha_dx_cancer_1>"2023-03-03" & dx_covid_estudio==1 ~ 
#       interval(fecha_dx_cancer_1 , fecha_dx_covid) %>% as.duration() %/% ddays())
#   ) 

#   ### Ordena columnas
#   select(idpte, fecha_nacimiento, edad, edad_cat, starts_with("com_"),
#          ends_with("_1"), ends_with("_2"), loc_tumor_ppal, mas_un_tumor,
#          metastasis, gravedad, ecog_num, 
#          trat_quim_2020, fecha_ini_quim_2020:ciclos_quim_2020,
#          trat_rtx_2020, fecha_ini_rtx, trat_quim_rtx, fallecido, fecha_fallec,
#          fecha_ini_covid:dx_covid, dx_covid_estudio, contains("covid")) 

# Exporta base limpia -----------------------------------------------------
export(data_rita_ve, file = "data_RITA_covid_clean.xlsx")
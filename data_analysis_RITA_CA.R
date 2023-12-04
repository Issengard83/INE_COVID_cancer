### Análisis de datos: COVID-19 en pacientes con cáncer incluidos en el Registro
### Institucional de Tumores de Argentina del HIGA Alende, Mar del Plata, año 2020:
### incidencia, gravedad y mortalidad
### Autor: Tamara Ricardo
### Última modificación: 

# Carga paquetes ----------------------------------------------------------
pacman::p_load(
  # Statistical analysis
  survival,
  # Inference and residuals
  DHARMa,
  performance,
  # Exploratory analysis
  gtsummary,
  dlookr,
  # Plot tools
  survminer,
  # Data management
  rio,
  janitor,
  tidyverse
)


# Carga datos RITA --------------------------------------------------------
data_rita <- import("data_RITA_covid_clean.xlsx") %>% 
  ### Variables categóricas a factor
  mutate(across(where(is.character)|matches("idpte"),
                as.factor)) %>% 
  
  ### Cambia niveles de referencia
  ## Tipo cáncer
  mutate_at("tipo_cancer", .funs = ~ fct_relevel(.x, "Sólido")) %>% 
  
  ## Topografía
  mutate_at("loc_tumor_ppal", 
            .funs = ~ fct_infreq(.x) %>% 
              fct_relevel("Otros/No especificado", after = Inf)) %>% 
  
  ## ECOG
  mutate_at("ecog", as.factor) %>% 
  
  ## Comorbilidades
  mutate_at("com_n_cat", 
            .funs = ~ fct_relevel(.x, "Dos comorbilidades", 
                                  "Más de dos comorbilidades", 
                                  "Desconocido",
                                  after = Inf)) %>% 
  
  ### Tiempo inicio y fin
  mutate(start_time = interval("2020-03-03", fecha_ini) %>% 
           as.duration() %/% ddays(),
         
         end_time = interval("2020-03-03", fecha_fin) %>% 
           as.duration() %/% ddays())

### Explora valores faltantes
data_rita %>% plot_na_pareto(only_na = T)

# Análisis exploratorio datos ---------------------------------------------
### Frecuencia variables muestra completa
data_rita %>% 
  mutate_at(c("dx_covid_estudio", "fallec_estudio", "fallec_covid_estudio"), 
            .funs = ~ if_else(.x==1, "Si", "No")) %>% 
  tbl_summary(missing = "no",
              by = dx_covid_estudio,
              include = c(sexo, edad, edad_cat, starts_with("com_"), -com_n,
                          loc_tumor_ppal, tipo_cancer, ecog, mas_un_tumor,
                          metastasis, gravedad, fallec_estudio, 
                          trat_quim_rtx_2020, trat_quim_2020, ciclos_quim_2020,
                          trat_rtx_2020)) %>% 
  add_p() %>% 
  bold_p() %>% 
  bold_labels()


# Objeto supervivencia ----------------------------------------------------
surv <- with(data_rita, Surv(event = dx_covid_estudio,
                             time = start_time,
                             time2 = end_time, type = "interval"))


### Modelo solo intercepto
fit<- survfit(surv ~ 1, data = data_rita)

fit

## Gráfico
ggsurvplot(fit = fit, data = data_rita, conf.int = T, ylim = c(.5,1))



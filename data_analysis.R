### Análisis de datos: COVID-19 en pacientes con cáncer incluidos en el Registro
### Institucional de Tumores de Argentina del HIGA Alende, Mar del Plata, año 2020:
### incidencia, gravedad y mortalidad
### Autor: Tamara Ricardo
### Última modificación: 


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
  # Plot tools
  survminer,
  # Data management
  rio,
  janitor,
  tidyverse
)

# Carga datos -------------------------------------------------------------
data <- import("datos_CA_COVID_clean.xlsx") %>% 
  ### Variables categóricas a factor
  mutate(across(where(is.character)|matches("idpte"),
                as.factor)) %>%
  ### Cambia niveles de referencia
  ## Tipo cáncer
  mutate_at("tipo_cancer", .funs = ~ fct_relevel(.x, "Sólido")) %>% 
  ## Topografía
  mutate_at("icd10_loc_ppal", 
            .funs = ~fct_infreq(.x) %>% fct_relevel("Otro/s", after = Inf)) %>% 
  ## Comorbilidades
  mutate_at("comorb_n", 
            .funs = ~ fct_relevel(.x, "Dos o más comorbilidades", "Desconocido",
                                  after = Inf)) %>% 
  ## COVID durante estudio (numérico)
  mutate(covid_estudio = if_else(dx_covid_estudio=="Si", 1, 0))

### Explora valores faltantes
data %>% plot_na_pareto(only_na = T)

# Análisis exploratorio datos ---------------------------------------------
### Frecuencia variables muestra completa
data %>% 
  tbl_summary(missing = "no",
              include = c(sexo, edad, edad_cat, starts_with("co_"),
                          comorb_n, icd10_loc_ppal, tipo_cancer, ev_cancer,
                          ecog_num, mas_un_tumor, metastasis, gravedad,
                          trat_quimio_rtx, trat_quimio, trat_rtx, ciclos_quimio,
                          dx_covid_estudio))

### Test asociación según diagnóstico de COVID
data %>% 
  tbl_summary(by = dx_covid_estudio,
              missing = "no", percent = "row",
              include = c(sexo, edad, edad_cat, co_metabolica, co_cardiovascular, 
                          co_respiratoria, co_HIV, co_alcoholismo, co_tabaquismo,
                          icd10_loc_ppal, tipo_cancer, ev_cancer, ecog_num,
                          gravedad, mas_un_tumor, metastasis,
                          trat_quimio_rtx, trat_rtx, trat_quimio,
                          ciclos_quimio)) %>% 
  add_p() %>% 
  bold_p() %>% 
  bold_labels()

# Análisis supervivencia (no paramétrico) ---------------------------------
### Crea objeto supervivencia
surv <- with(data, Surv(dias_ini_covid, covid_estudio))

### Modelo solo intercepto
fit<- survfit(surv ~ 1, data = data)

fit

## Gráfico
ggsurvplot(fit = fit, data = data, conf.int = T, ylim = c(.75,1))


# Supervivencia por sexo --------------------------------------------------
fit_sex <- survfit(surv ~ sexo, data = data)

## Gráfico
ggsurvplot(fit = fit_sex, data = data, conf.int = T, ylim = c(.75,1), 
           risk.table = T)

## Compara funciones
survdiff(surv ~ sexo, data = data) # p= 0.4


# Supervivencia por grupo etario ------------------------------------------
fit_edad <- survfit(surv ~ edad_cat, data = data)

## Gráfico
ggsurvplot(fit = fit_edad, data = data, conf.int = T, ylim = c(.75,1), 
           risk.table = T)

## Compara funciones
survdiff(surv ~ edad_cat, data = data) # p= 0.2


# Supervivencia por tipo de cáncer ----------------------------------------
### Sólido vs hematológico
fit_tipo <- survfit(surv ~ tipo_cancer, data = data)

## Gráfico
ggsurvplot(fit = fit_tipo, data = data, conf.int = T, ylim = c(.75,1), 
           risk.table = T)

## Compara funciones
survdiff(surv ~ tipo_cancer, data = data) # p= 0.2

### Sitio principal
fit_topo <- survfit(surv ~ icd10_loc_ppal, data = data)

## Gráfico
ggsurvplot(fit = fit_topo, data = data, conf.int = T, ylim = c(.75,1), 
           risk.table = F)

## Compara funciones
survdiff(surv ~ icd10_loc_ppal, data = data) # p= 0.9

# Supervivencia por tipo tratamiento --------------------------------------
fit_trat <- survfit(surv ~ trat_quimio_rtx, data = data)

## Gráfico
ggsurvplot(fit = fit_trat, data = data, conf.int = T, ylim = c(.75,1), 
           risk.table = T)

## Compara funciones
survdiff(surv ~ trat_quimio_rtx, data = data) # p= 0.04*

### Quimioterapia
fit_quim <- survfit(surv ~ trat_quimio, data = data)

## Gráfico
ggsurvplot(fit = fit_quim, data = data, conf.int = T, ylim = c(.75,1), 
           risk.table = T)

## Compara funciones
survdiff(surv ~ trat_quimio, data = data) # p= 0.01*


# Supervivencia por gravedad ----------------------------------------------
### Más de un tumor
fit_mas <- survfit(surv ~ mas_un_tumor, data = data)

## Gráfico
ggsurvplot(fit = fit_mas, data = data, conf.int = T, ylim = c(.75,1), 
           risk.table = T)

## Compara funciones
survdiff(surv ~ mas_un_tumor, data = data) # p= 0.6

### Metástasis
fit_met <- survfit(surv ~ metastasis, data = data)

## Gráfico
ggsurvplot(fit = fit_met, data = data, conf.int = T, ylim = c(.75,1), 
           risk.table = T)

## Compara funciones
survdiff(surv ~ metastasis, data = data) # p= 0.8

### Gravedad del cáncer
fit_grav <- survfit(surv ~ gravedad, data = data)

## Gráfico
ggsurvplot(fit = fit_grav, data = data, conf.int = T, ylim = c(.75,1), 
           risk.table = T)

## Compara funciones
survdiff(surv ~ gravedad, data = data) # p= 0.9


# Regresión de Cox --------------------------------------------------------
### Modelo full
fit_cox <- coxph(surv ~ trat_quimio, data = data)

summary(fit_cox)

## AIC
performance(fit_cox, metrics = "common")

## Coeficientes
tbl_regression(fit_cox, exponentiate = T)

## Gráfico
ggadjustedcurves(fit = fit_cox, data = data, 
                 variable = "trat_quimio", ylim = c(.75, 1))

## Residuales
ggcoxdiagnostics(fit_cox, type = "schoenfeld")

ggcoxdiagnostics(fit_cox, type = "martingale")

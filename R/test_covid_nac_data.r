needed_packages <- c(
  "tidyverse"
  , "lubridate"
  , "EpiEstim"
  , "incidence"
  , "projections"
  , "epitrix"
  , "distcrete")

lapply(needed_packages, function(x) { if(!require(x,character.only = TRUE)) install.packages(x)} )

csv_fname <- "Data/covid_19_casos.csv"

cor <- read_csv2(csv_fname) 

corConf <- cor %>% filter(clasificacion_resumen=="Confirmado") %>% mutate(fecha=ymd(fecha_fis))

unique(cor$clasificacion_resumen)
unique(cor$provincia_carga)

corConf <- cor %>% filter(provincia_carga=="Tierra del Fuego",clasificacion_resumen=="Confirmado") %>% mutate(fecha=ymd(fecha_fis),importado=grepl("VIAJE", antecedente_epidemiologico)) %>% mutate(group=if_else(importado,"imported","local"))

  
cor_incidence_obj <- corConf  %>% select(group,fecha)

cor_incidence_obj <- incidence::incidence(cor_incidence_obj$fecha,groups=cor_incidence_obj$group, last_date=max(cor$ultima_actualizacion))

estima_Re_from_df(cor_incidence_obj,"TDF")

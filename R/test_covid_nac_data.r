needed_packages <- c(
  "tidyverse"
  , "lubridate"
  , "EpiEstim"
  , "incidence"
  , "projections"
  , "epitrix"
  , "distcrete")

lapply(needed_packages, function(x) { if(!require(x,character.only = TRUE)) install.packages(x)} )


# Datos de Casos de Gobierno Nacional 
#

csv_fname1 <- "Data/Covid19Casos.csv"

url <- "https://sisa.msal.gov.ar/datos/descargas/covid-19/files/Covid19Casos.csv"

if( Sys.which("wget")!="" ) {
  setwd("Data")
  system(paste("wget -q -N", url)) 
  setwd("..")
} else {
  download.file(url, destfile = csv_fname1)
}
cor <- read.csv(csv_fname1,stringsAsFactors = FALSE,fileEncoding = "ISO-8859-1",skipNul = TRUE)
names(cor)
corConf <- cor %>% filter(clasificacion_resumen=="Confirmado") %>% mutate_at(vars(starts_with("fecha")),ymd) %>% mutate( hospitalizado=!is.na(fecha_internacion))


str(corConf)
unique(cor$clasificacion_resumen)
unique(cor$clasificacion)
unique(cor$residencia_provincia_nombre)
unique(cor$residencia_provincia_id)

CABA <- corConf %>% filter(carga_provincia_nombre=="CABA") %>% mutate(dias_hosp = fecha_internacion-fecha_inicio_sintomas, hospitalizado= !is.na(fecha_internacion) )

(corConf %>% filter(hospitalizado) %>% group_by(carga_provincia_nombre,fallecido) %>% summarise(edad=median(edad, na.rm = TRUE),n=n()) %>%mutate(freq = n / sum(n) * 100)) 

(corConf %>% group_by(carga_provincia_nombre,hospitalizado) %>% summarise(n=n()) %>%mutate(freq = n / sum(n) * 100)) %>% filter(hospitalizado)

corConf %>% filter(hospitalizado) %>% group_by(carga_provincia_nombre, cuidado_intensivo) %>% summarise(n=n()) %>%mutate(freq = n / sum(n) * 100)


#%>% mutate( importado=grepl("VIAJE", antecedente_epidemiologico)) %>% mutate(group=if_else(importado,"imported","local"))

  
cor_incidence_obj <- corConf  %>% select(group,fecha)

cor_incidence_obj <- incidence::incidence(cor_incidence_obj$fecha,groups=cor_incidence_obj$group, last_date=max(cor$ultima_actualizacion))

estima_Re_from_df(cor_incidence_obj,"TDF")

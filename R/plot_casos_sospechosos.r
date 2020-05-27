require(lubridate)
require(tidyverse)
theme_set(theme_bw())

# Datos de Casos de Gobierno Nacional 
#
csv_fname <- "Data/covid_19_casos.csv"
cor_nac <- read_csv2(csv_fname)

# Detectar asintomaticos usando la fecha de inicio de sintomas
#
#cor_asi <- filter(cor_nac,is.na(fis),clasificacion_resumen=="Confirmado",grepl("CONTACTO",antecedente_epidemiologico),provincia)

# Datos de Casos de Gobierno Nacional 
#
corConf <- cor_nac %>% filter(clasificacion_resumen!="Descartado")  %>% mutate(nue_clasif=ifelse(clasificacion_resumen=="Confirmado",clasificacion_resumen,clasificacion)) %>% group_by(provincia_residencia,nue_clasif) %>% summarise(n=n()) %>% mutate(porcent= n/sum(n)) # %>% filter(grepl("muestra",clasificacion,ignore.case = TRUE)) 


ggplot(corConf, aes(x=provincia_residencia,y=porcent,fill=nue_clasif)) + geom_bar(stat="identity") +
  theme_minimal() + scale_fill_viridis_d(name="Clasificación") + theme(axis.text.x = element_text( angle=90,hjust=1)) + ggtitle("Casos Confirmados/Sospechosos % Covid-19 al 19/05 by @larysar") + coord_flip()

```

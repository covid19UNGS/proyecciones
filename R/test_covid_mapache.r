needed_packages <- c(
  "tidyverse"
  , "lubridate"
  , "EpiEstim"
  , "incidence"
  , "projections"
  , "epitrix"
  , "distcrete")

lapply(needed_packages, function(x) { if(!require(x,character.only = TRUE)) install.packages(x)} )


cor<-read_csv('https://docs.google.com/spreadsheets/d/16-bnsDdmmgtSxdWbVMboIHo5FRuz76DBxsz_BbsEVWA/export?format=csv&id=16-bnsDdmmgtSxdWbVMboIHo5FRuz76DBxsz_BbsEVWA&gid=0')

unique(cor$osm_admin_level_4)

corConf <- cor %>% filter( osm_admin_level_4 =="Córdoba") %>% mutate(fecha=dmy(fecha)) %>% select(nue_casosconf_diff,fecha,transmision_tipo)

prov <- estima_Re_from_df(corConf,"Prov. Córdoba",end_date=today())


corConf <- cor %>% filter( osm_admin_level_4 =="Tierra del Fuego", osm_admin_level_8!="Malvinas" | is.na(osm_admin_level_8)) %>% mutate(fecha=dmy(fecha)) %>% select(nue_casosconf_diff,fecha,transmision_tipo,osm_admin_level_8)

estima_Re_from_df(corConf,"Prov. Córdoba")

corConf <- cor %>% filter( osm_admin_level_4 =="CABA") %>% mutate(fecha=dmy(fecha)) %>% select(nue_casosconf_diff,nue_fallecidos_diff,fecha,transmision_tipo,osm_admin_level_8)

corConf <- cor %>% filter( osm_admin_level_4 =="Indeterminado") %>% mutate(fecha=dmy(fecha)) %>% select(tot_casosconf,tot_fallecidos,tot_recuperados,tot_terapia,fecha,transmision_tipo)

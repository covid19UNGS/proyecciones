needed_packages <- c(
  "tidyverse"
  , "lubridate"
  , "EpiEstim"
  , "incidence"
  , "projections"
  , "epitrix"
  , "distcrete")

lapply(needed_packages, function(x) { if(!require(x,character.only = TRUE)) install.packages(x)} )

require(lubridate)
require(tidyverse)
theme_set(theme_bw())
source("R/functions.r")


#csv_fname <- "https://raw.githubusercontent.com/lsaravia/covid19ar/master/coronavirus_ar.csv"
csv_fname <- "Data/coronavirus_ar.csv"


cor <- read_csv(csv_fname) %>% dplyr::select(fecha:TDFdia)
cor <- cor  %>% mutate(fecha=ymd(fecha), dias =as.numeric( fecha - min(fecha))) 
cor <- cor %>% mutate(importadosdia=importados-lag(importados))
cor$importadosdia[1] <- 1
cor <- cor %>% mutate(localesdia=casosdia - importadosdia, CABAdia=ifelse(is.na(CABAdia),0,CABAdia))


#
# Ajuste de modelos log-lineales (exponencial)
#
require(incidence)
require(projections)

cor_incidence <- cor  %>% dplyr::select(fecha, CABAdia) %>% uncount(CABAdia)
cor_incidence_obj <- incidence::incidence(cor_incidence$fecha, last_date=max(cor$fecha))

#
# 
#
cor_incidence_peak <- as.Date("2020-03-30")   # Estimo que alli termina la fase exponencial inicial  
cor_incidence_peak <- estimate_peak(cor_incidence_obj)
cor_incidence_peak <- ymd(cor_incidence_peak$estimated)

cor_incidence_fit <- incidence::fit(cor_incidence_obj, 
                                    split = cor_incidence_peak)

cor_incidence_fit

# plot the incidence data and the model fit
plot(cor_incidence_obj) %>% add_incidence_fit(cor_incidence_fit) + ylab("Nro de Casos por Día")+
  labs(title = "Incidencia Observada y ajuste log-lineal para CABA COVID-19", 
       subtitle = "Argentina, 2020 by @larysar") +  
  geom_vline(xintercept = cor_incidence_peak,col = "red", lty = 2) + scale_y_log10()


require(epitrix)
require(distcrete)
#
# Parametros de la distribucion gamma para el intervalos serial
#
mu <- 6.6944741
sigma <- 4.4140809

param <- gamma_mucv2shapescale(mu, sigma / mu)
w <- distcrete("gamma", interval = 1,
               shape = param$shape,
               scale = param$scale, w = 0)

#
# Calculo del R0 
#

dfCABA <- cor %>% dplyr::rename(nue_casosconf_diff=CABAdia)
esProv <- estima_Re_from_df(dfCABA,"CABA",end_date = max(cor$fecha),nombre_fases = c("Cuarentena","Fase 2","Fase 3","Fase 3"))
meanR <- esProv[[2]]$R$`Mean(R)`

growth_R0 <- lm2R0_sample(cor_incidence_fit$before$model, w)

summary(growth_R0)

decay_R0 <- lm2R0_sample(cor_incidence_fit$after$model, w)
summary(decay_R0)

#
#
#
set.seed(1)
pred_fwd_days <- 10

date_range <- 1:(which(get_dates(cor_incidence_obj) == cor_incidence_peak) - pred_fwd_days)
test_pred_growth <- project(cor_incidence_obj[date_range],
                            R = median(growth_R0),
                            si = w,
                            n_days = pred_fwd_days, n_sim = 1000)

# 
#
set.seed(1)
pred_fwd_days <- 10 # 5
if(cor_incidence_peak < "2020-04-30"){
  date_range <- which(get_dates(cor_incidence_obj) == cor_incidence_peak):(length(get_dates(cor_incidence_obj)) - pred_fwd_days)
  
} else {
date_range <- (which(get_dates(cor_incidence_obj) == cor_incidence_peak)-pred_fwd_days):(which(get_dates(cor_incidence_obj) == cor_incidence_peak))
}
#
# Ultimos 7 dias
#
cor_incidence_peak = max(cor$fecha) - 7
date_range <- (which(get_dates(cor_incidence_obj) == cor_incidence_peak)-pred_fwd_days):(which(get_dates(cor_incidence_obj) == cor_incidence_peak))
decay_R0 <- meanR[length(meanR)]
  
test_pred_decay <- project(cor_incidence_obj[date_range],
                           R = median(decay_R0),
                           si = w,
                           n_days = 60, n_sim = 1000)

plot(cor_incidence_obj) %>% add_projections(test_pred_growth, boxplots = FALSE) %>% add_projections(test_pred_decay, boxplots = FALSE) +  geom_vline(xintercept = cor_incidence_peak,col = "red", lty = 2)  

#
# Extraer la info del objeto test_pred_decay (con las predicciones) 
# calcular la relacion entre incidencia y los fallecidos 14 dias despues (se supone que en ese momento entraron en la UTI)
#
preds <- data.frame(test_pred_decay) %>% pivot_longer(-dates,values_to = "pred",names_to = "sim") %>% group_by(dates) %>% 
  summarise(pred_mean=mean(pred), pred_99=quantile(pred,.995),pred_1=quantile(pred,.005)  ) %>% rename(fecha=dates) %>% 
  full_join( cor %>% select(fecha,CABA,CABAdia,CABAFallecido,CABAterapia)) %>% arrange(fecha) %>% 
  mutate(CABAlag14 = lag(CABAdia,14), porcUTI=CABAFallecido/CABAlag14)
 
mporcUTI <-  median(preds$porcUTI[preds$porcUTI>0],na.rm = T)
 
#
# A partir del dato de ocupacion de UTI de CABA del dia 2020-05-30 calculo entradas y salidas usando `mporcUTI`
#
preds <- preds %>% mutate( CABAdia = ifelse(is.na(CABAdia),pred_mean,CABAdia), entraUTI=rpois(n(),CABAdia*mporcUTI), saleUTI=lag(entraUTI,14),
                           entraSale= ifelse(fecha<"2020-05-31",0,entraUTI-saleUTI),
                           permaUTI=ifelse(fecha<"2020-05-31",CABAterapia,152 + cumsum(entraSale)),CABAcum=cumsum(CABAdia))

col <- viridisLite::viridis(3)

ggplot(preds, aes(x = fecha, y = CABAcum) ) +
  geom_line(color = col[1]) +
  geom_point( aes(x=fecha,y=permaUTI), size = .5, color= col[2]) + scale_y_log10() +
  geom_hline(yintercept = 1250, size = .5, color= "red",lty=2) +  theme_bw() + ylab("Casos") +
  annotate("text",x=ymd("20200701"), y=2,label=paste("% a UTI=", round(mporcUTI,2)*100,"\n R0 =", round(median(decay_R0),2), "\nby @larysar"),color="red",size=2) +
      labs(title = paste("Incidencia Observada hasta", today()),
        subtitle ="y modelada para CABA COVID-19")



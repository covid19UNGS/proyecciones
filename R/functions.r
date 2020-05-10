
#' Use EpiEstim to estimate the Efective reproductive Number by week 
#'
#' @param df data.frame with the incidence data to convert to an incidence object
#' @param region name of the region to plot
#' @param pair_data data to estimate the serial interval
#' @param end_date final time of the data set
#'
#' @return a list with the incidence object and the fitted Re
#' @export
#'
#' @examples
estima_Re_from_df <- function(df,region,pair_data=NULL,end_date=NULL){ 
  if(!inherits(df, "data.frame")) stop("Parameter df must be a data.frame")
  require(EpiEstim)
  require(lubridate)
  if(any(names(df)=="nue_casosconf_diff")) {
    
    cor_incidence <- df  %>% dplyr::select(nue_casosconf_diff,fecha) %>% uncount(nue_casosconf_diff)
    if(class(cor_incidence$fecha)!="Date") {
      cor_incidence_obj <- incidence::incidence(dmy(cor_incidence$fecha),last_date=end_date)
    } else {
      cor_incidence_obj <- incidence::incidence(cor_incidence$fecha,last_date=end_date)
    }
      
  } else if(any(names(df)=="localesdia")) { 
    
    cor_incidence_obj <- df %>% dplyr::select(localesdia,importadosdia,fecha) %>% rename(local=localesdia,imported=importadosdia,dates=fecha)
    
  }
  if(is.null(pair_data)){
  obj_res_parametric_si <- estimate_R(cor_incidence_obj, 
                                     method = "uncertain_si", 
                                     config = make_config(list(mean_si = 7.5, std_mean_si = 2, 
                                                               min_mean_si = 1, max_mean_si = 8.4, 
                                                               std_si = 3.4, std_std_si = 1, 
                                                               min_std_si = 0.5, max_std_si = 4, n1 = 1000, n2 = 1000)))
  } else {
    
    ## fixing the random seeds
    MCMC_seed <- 1
    overall_seed <- 2
    mcmc_control <- make_mcmc_control(seed = MCMC_seed, burnin = 1000)
    dist <- "G"  # fitting a Gamma distribution for the SI
    empirical_si_config <- make_config(list(si_parametric_distr = dist, 
                                            mcmc_control = mcmc_control, seed = overall_seed, n1 = 50, 
                                            n2 = 50))
    obj_res_parametric_si <- estimate_R(cor_incidence_obj, method = "si_from_data", 
                                      si_data = pair, config = empirical_si_config)
    
    
  }
  
  cor_quarantine <- ymd("2020-03-20")
  cor_incidence_real_peak <- ifelse(inherits(cor_incidence_obj,"incidence"), incidence::find_peak(cor_incidence_obj),cor_quarantine)
  
  print(
    plot(obj_res_parametric_si, "incid" ) + labs(title = paste(region,"Casos por dia Importados y Locales"), 
                                    subtitle = " COVID-19, Argentina, 2020 by @larysar") + theme_bw() +
                                    geom_vline(xintercept = cor_quarantine, col = "red", lty = 2)  +
                                    geom_vline(xintercept = cor_incidence_real_peak, col = "brown", lty = 2)
  )
  
  # print(plot(obj_res_parametric_si, "SI")+ theme_bw())
  
  print(
    plot(obj_res_parametric_si, "R")+ theme_bw() + labs(title = paste(region,"Nro Reproductivo Efectivo Basado en 7 días"), 
                                                       subtitle = "COVID-19, Argentina, 2020 by @larysar") + theme_bw() +  
                                      geom_vline(xintercept = cor_quarantine, col = "red", lty = 2) +
                                      geom_vline(xintercept = cor_incidence_real_peak, col = "brown", lty = 2) +
                                      scale_y_continuous(trans="log2")
    )

  return(list(cor_incidence_obj,obj_res_parametric_si))
  }


## Packages ##

library(dplyr)
library(magrittr)
library(EpiNow2)

## Scenarios function ##

sim_scenarios <- function(case_data,
                          gen_mean,
                          gen_sd,
                          gen_max,
                          inc_mean,
                          inc_sd,
                          inc_max,
                          rep_meanlog,
                          rep_sdlog,
                          rep_max,
                          freq_fc=4,
                          weeks_inc=12,
                          obs_scale){
  
  ## Scenarios
  
  scen_values <- c(0.24, 0.75, 1, 1.25, 1.75) # no delay needed + removed the extremes to test
  names(scen_values) <- c("very low", "low", "correct", "high", "very high") # removed extremes to test
  
  scen_timepoints <- case_data$date[c(1:(nrow(case_data) %/% (freq_fc*7)))*freq_fc*7]
  names(scen_timepoints) <- c(1:length(scen_timepoints))
  
  results_list <- list()
  results_id <- data.frame()
  
  for(i in 1:length(scen_values)){
    for(j in 1:length(scen_values)){
       for(k in 1:length(scen_timepoints)){
      
     
      
      # Case data 
      case_segment <- case_data %>%
        filter(date <= scen_timepoints[k])
      
      # Use previous 12 weeks of data for forecast
      case_segment <- case_segment %>%
        filter(date > (case_segment$date[nrow(case_segment)] - weeks_inc*7))
      
      print(nrow(case_segment))
      
      # Generation interval
      gen_time <- Gamma(mean=gen_mean*scen_values[i],
                        sd=gen_sd,
                        max=gen_max)
      
      # Incubation period
      inc_period <- LogNormal(meanlog=convert_to_logmean(inc_mean*scen_values[j], inc_sd),
                              sdlog=convert_to_logsd(inc_mean*scen_values[j], inc_sd),
                              max=inc_max)
      
      reporting_delay <- LogNormal(meanlog=rep_meanlog,
                                   sdlog=rep_sdlog,
                                   max=rep_max)
      
      def <- estimate_infections(case_segment,
                                 generation_time = generation_time_opts(gen_time),
                                 delays = delay_opts(inc_period + reporting_delay),
                                 obs=obs_opts(family="poisson", scale=obs_scale),
                                 stan = stan_opts())
      
      results_list[[length(results_list)+1]] <- def$samples
      
      scen_id <- data.frame(result_list=length(results_list),
                            timepoint=k,
                            gen_time=names(scen_values)[i],
                            inc_period=names(scen_values)[j])
      
      results_id <- rbind(results_id, scen_id)
      
      print(paste("timepoint =", k, "gen time =", i, "inc period =", j))
    }
  }
  }
  
  save_warnings <- warnings()
  
  return(list(results_list,
              results_id,
              save_warnings))
}


## Load data ##

cholera_sim_data <- readRDS(paste0("cholera_sim_data", "2024-05-07", ".rds"))

# In required format for EpiNow2

cholera_sim_data_cases <- cholera_sim_data %>% filter(variable=="reported_cases")
cholera_sim_data_cases <- cholera_sim_data_cases %>%
  select(date, value) %>%
  rename(confirm=value)

## Run scenarios ##

res_cholera <- sim_scenarios(case_data=cholera_sim_data_cases,
                           gen_mean=5,
                           gen_sd=8,
                           gen_max=30,# careful in case this changes https://www.thelancet.com/journals/langlo/article/PIIS2214-109X(18)30230-4
                           inc_mean=1.4,
                           inc_sd=1.98,  # from Azman et al. 2013
                           inc_max=30,
                           rep_meanlog=convert_to_logmean(4.4, 1.88),
                           rep_sdlog=convert_to_logsd(4.4, 1.88),
                           rep_max=30, # https://tbiomed.biomedcentral.com/articles/10.1186/s12976-017-0061-x 
                           freq_fc=2,
                           weeks_inc=12,
                           obs_scale=0.28) # https://tbiomed.biomedcentral.com/articles/10.1186/s12976-017-0061-x

saveRDS(res_cholera[[1]], paste0("res_cholera", Sys.Date(), ".rds"))
saveRDS(res_cholera[[2]], paste0("res_cholera_id", Sys.Date(), ".rds"))
saveRDS(res_cholera[[3]], paste0("res_cholera_warnings", Sys.Date(), ".rds"))

## Saving samples only ##

cholera_samples <- data.frame()
for(i in 1:length(res_cholera)){
  samples_scen <- res_cholera[[1]]res_cholera[[i]][res_cholera[[i]]$variable=="reported_cases"] %>%
    mutate(model="EpiNow2")

# Add ID
samples_scen$result_list <- i

# Bind to dataframe
cholera_samples <- rbind(cholera_samples, samples_scen)}

cholera_samples <- cholera_samples %>%
  rename(prediction=value)

saveRDS(cholera_samples, here(paste0("res_cholera_samples", Sys.Date(), ".rds"))


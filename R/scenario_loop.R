
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
  
  scen_values <- c(0.25, 0.75, 1, 1.25, 1.75) # no delay needed + removed the extremes to test
  names(scen_values) <- c("very low", "low", "correct", "high", "very high") # removed extremes to test
  
  scen_timepoints <- case_data$date[c(1:(nrow(case_data) %/% (freq_fc*7)))*freq_fc*7]
  names(scen_timepoints) <- c(1:length(scen_timepoints))

  res_samples <- list()
  results_id <- list()

 for(i in 1:length(scen_values)){
   for(j in 1:length(scen_values)){
      for(k in 1:length(scen_timepoints)){
     
        # Case data 
        case_segment <- case_data |>
          filter(date <= scen_timepoints[k])
        
        # Use previous 12 weeks of data for forecast
        case_segment <- case_segment |>
          filter(date > (case_segment$date[nrow(case_segment)] - weeks_inc*7))
        
        print(nrow(case_segment))
        
        # Generation interval
        gen_time <- Gamma(mean=gen_mean*scen_values[i],
                          sd=gen_sd,
                          max=gen_max)

        # Incubation period
        inc_period <- LogNormal(mean=inc_mean*scen_values[j],
                                sd=inc_mean*scen_values[j],
                                max=inc_max)
        
        reporting_delay <- LogNormal(meanlog=rep_meanlog,
                                     sdlog=rep_sdlog,
                                     max=rep_max)

        def <- estimate_infections(case_segment,
                                   generation_time = generation_time_opts(gen_time),
                                   delays = delay_opts(inc_period + reporting_delay),
                                   obs=obs_opts(family="poisson", scale=obs_scale),
                                   stan = stan_opts())
        
        res_samples[[length(res_samples)+1]] <- def$samples[variable=="reported_cases"]
        
        results_id[[length(results_id)+1]] <- data.frame(result_list=length(res_samples),
                              timepoint=k,
                              gen_time=names(scen_values)[i],
                              inc_period=names(scen_values)[j])
        
        print(paste("timepoint =", k, "gen time =", i, "inc period =", j))
     }
    }
 }
  
  save_warnings <- warnings()
  
  results_id <- bind_rows(results_id)
  
  res_samples <- lapply(seq_along(res_samples[[1]]), function(i) {
    samples_scen <- res_samples[[1]][[i]] |>
      mutate(model="EpiNow2")
    
    # Add ID
    samples_scen$result_list <- i
    
    # Bind to dataframe
    return(samples_scen)
    
  })
  
  res_samples <- bind_rows(res_samples) |>
    rename(prediction=value)
  
  return(list(res_samples,
              results_id,
              save_warnings))
}

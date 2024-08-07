
sim_scenarios <- function(case_data,
                          var,
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
                          rt_opts_choice,
                          obs_scale){
  
  ## Scenarios
  
  scen_values <- c(0, 0.25, 0.8, 1, 1.25, 2) 
  names(scen_values) <- c("no delay", "very low", "low", "correct", "high", "very high") 
  
  scen_timepoints <- case_data$date[c(1:(nrow(case_data) %/% (freq_fc*7)))*freq_fc*7]
  names(scen_timepoints) <- c(1:length(scen_timepoints))

  res_samples <- list()
  res_R <- list()
  results_id <- list()

   for(j in c(1:length(scen_values))){
      for(k in 1:length(scen_timepoints)){
     
        # Case data 
        case_segment <- case_data |>
          filter(date <= scen_timepoints[k])
        
        # Use previous 12 weeks of data for forecast
        case_segment <- case_segment |>
          filter(date > (case_segment$date[nrow(case_segment)] - weeks_inc*7))
        
        print(nrow(case_segment))
        
        # Generation interval
        if(var!=1){
        gen_time <- Gamma(mean=gen_mean*scen_values[var],
                          sd=gen_sd,
                          max=gen_max)}
        
        # Incubation period
        if(j!=1){
        inc_period <- LogNormal(mean=inc_mean*scen_values[j],
                                sd=inc_mean*scen_values[j],
                                max=inc_max)}
        
        reporting_delay <- LogNormal(meanlog=rep_meanlog,
                                     sdlog=rep_sdlog,
                                     max=rep_max)
        
        if(var!=1 & j!=1){
        def <- estimate_infections(case_segment,
                                   generation_time = generation_time_opts(gen_time),
                                   delays = delay_opts(inc_period + reporting_delay),
                                   obs=obs_opts(family="poisson", scale=obs_scale),
                                   rt=rt_opts(future=rt_opts_choice),
                                   stan = stan_opts(),
                                   horizon=14)}
        # if setting generation time to 1 day
        if(var==1 & j!=1){
          def <- estimate_infections(case_segment,
                                     delays = delay_opts(inc_period + reporting_delay),
                                     obs=obs_opts(family="poisson", scale=obs_scale),
                                     stan = stan_opts(),
                                     horizon=14)
        }
        
        # if setting inc period to "no delay"
        if(var!=1 & j==1){
          def <- estimate_infections(case_segment,
                                     generation_time = generation_time_opts(gen_time),
                                     delays = delay_opts(reporting_delay),
                                     obs=obs_opts(family="poisson", scale=obs_scale),
                                     stan = stan_opts(),
                                     horizon=14)}
        
        # if setting generation time to 1 day and inc period to no delay
        if(var==1 & j==1){
          def <- estimate_infections(case_segment,
                                     delays = delay_opts(reporting_delay),
                                     obs=obs_opts(family="poisson", scale=obs_scale),
                                     stan = stan_opts(),
                                     horizon=14)}
        
        res_samples[[length(res_samples)+1]] <- def$samples[variable=="reported_cases"]
        
        res_R[[length(res_R)+1]] <- def$samples[variable=="R"]
        
        results_id[[length(results_id)+1]] <- data.frame(result_list=length(res_samples),
                              timepoint=k,
                              gen_time=names(scen_values)[var],
                              inc_period=names(scen_values)[j])
        
        print(paste("timepoint =", k, "gen time =", var, "inc period =", j))
     }
    }

  
  save_warnings <- warnings()
  
  results_id <- bind_rows(results_id)
  
  res_samples <- lapply(seq_along(res_samples), function(i) {
    samples_scen <- res_samples[[i]] |>
      mutate(model="EpiNow2")
    
    # Add ID
    samples_scen$result_list <- i
    
    # Bind to dataframe
    return(samples_scen)
    
  })
  
  res_R <- lapply(seq_along(res_R), function(i) {
    samples_scen <- res_R[[i]] |>
      mutate(model="EpiNow2")
    
    # Add ID
    samples_scen$result_list <- i
    
    # Bind to dataframe
    return(samples_scen)
    
  })
  
  res_samples <- bind_rows(res_samples) |>
    rename(prediction=value)
  
  res_R <- bind_rows(res_R)
  
  return(list(res_samples,
              results_id,
              save_warnings,
              res_R))
}

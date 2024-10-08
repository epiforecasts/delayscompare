
sim_scenarios <- function(case_data,
                          var,
                          gen_mean,
                          gen_sd,
                          gen_max,
                          inc_mean,
                          inc_sd,
                          inc_max,
                          rep_mean,
                          rep_sd,
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

  scenarios <- expand.grid(
    j = seq_along(scen_values),
    k = seq_along(scen_timepoints)
  )

  res <- pmap(scenarios, \(j, k) {
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
                          max=gen_max)
        } else {
          gen_time <- Fixed(1)
        }
        
        # Incubation period
        if(j!=1){
        inc_period <- LogNormal(mean=inc_mean*scen_values[j],
                                sd=inc_sd,
                                max=inc_max)
        } else {
          inc_period <- Fixed(0)
        }
        reporting_delay <- LogNormal(mean=rep_mean*scen_values[j],
                                     sd=rep_sd,
                                     max=rep_max)

        
          def <- estimate_infections(case_segment,
                                     generation_time = generation_time_opts(gen_time),
                                     delays = delay_opts(reporting_delay),
                                     obs=obs_opts(family="poisson", scale=obs_scale),
                                     rt=rt_opts(future=rt_opts_choice),
                                     stan = stan_opts(return_fit = FALSE),
                                    horizon=14,
                                    verbose = FALSE)

         res_samples <-
          def$samples[
                variable=="reported_cases" & type != "estimate",
                list(date, sample, value, type)
              ]
        
        res_R <-
          def$samples[
                variable=="R" & type != "estimate",
                list(date, sample, value, type)
              ]

        def$samples <- NULL
        
        res_id <- data.frame(timepoint=k,
                             gen_time=names(scen_values)[var],
                             inc_period=names(scen_values)[j])
        
        print(paste("timepoint =", k, "gen time =", var, "inc period =", j))
        return(list(samples = res_samples,
                    R = res_R,
                    id = res_id,
                    summary = def))
  }, .progress = TRUE)

  
  save_warnings <- warnings()
  res <- transpose(res)
  
  results_id <- bind_rows(res$id)
  
  res_samples <- lapply(seq_along(res$samples), function(i) {
    samples_scen <- res$samples[[i]] |>
      mutate(model="EpiNow2")
    
    # Add ID
    samples_scen$result_list <- i
    
    # Bind to dataframe
    return(samples_scen)
    
  })
  
  res_R <- lapply(seq_along(res$R), function(i) {
    samples_scen <- res$R[[i]] |>
      mutate(model="EpiNow2")
    
    # Add ID
    samples_scen$result_list <- i
    
    # Bind to dataframe
    return(samples_scen)
    
  })
  
  res_samples <- bind_rows(res_samples) |>
    rename(prediction=value)
  
  res_R <- bind_rows(res_R)
  
  return(list(samples = res_samples,
              id = results_id,
              warnings = save_warnings,
              R = res_R,
              summary = res$summary))
}


sim_weightprior <- function(case_data,
                          var,
                          gen_mean_mean,
                          gen_mean_sd,
                          gen_sd_mean,
                          ge_sd_sd,
                          gen_max,
                          inc_mean_mean,
                          inc_mean_sd,
                          inc_sd_mean,
                          inc_sd_sd,
                          inc_max,
                          rep_mean,
                          rep_sd,
                          rep_max,
                          freq_fc=4,
                          weeks_inc=12,
                          rt_opts_choice,
                          obs_scale){
  

  scen_timepoints <- case_data$date[c(1:(nrow(case_data) %/% (freq_fc*7)))*freq_fc*7]
  names(scen_timepoints) <- c(1:length(scen_timepoints))
  
res <- pmap(scen_timepoints, \(k) {
        # Case data
        case_segment <- case_data |>
          filter(date <= scen_timepoints[k])
        
        # Use previous 12 weeks of data for forecast
        case_segment <- case_segment |>
          filter(date > (case_segment$date[nrow(case_segment)] - weeks_inc*7))
        
        print(nrow(case_segment))
        
        # Generation interval
        gen_time <- Gamma(mean=Normal(gen_mean_mean, gen_mean_sd),
                          sd=Normal(gen_sd_mean, gen_sd_sd),
                          max=gen_max)
        
        # Incubation period
        inc_period <- LogNormal(mean=Normal(inc_mean_mean, inc_mean_sd),
                                sd=Normal(inc_sd_mean, inc_sd_sd),
                                max=inc_max)
        
        reporting_delay <- LogNormal(mean==rep_mean,
                                     sd=rep_sd,
                                     max=rep_max)

          def <- estimate_infections(case_segment,
                                     generation_time = generation_time_opts(gen_time, weight_prior=TRUE),
                                     delays = delay_opts(inc_period + reporting_delay, weight_prior=TRUE),
                                     obs=obs_opts(family="poisson", scale=obs_scale),
                                     rt=rt_opts(future=rt_opts_choice),
                                     stan = stan_opts(return_fit = FALSE),
                                    horizon=14,
                                    verbose = FALSE)

         res_samples <-
          def$samples[
                variable=="reported_cases" & type != "estimate",
                list(date, sample, value, type)
              ]
        
        res_R <-
          def$samples[
                variable=="R" & type != "estimate",
                list(date, sample, value, type)
              ]

        def$samples <- NULL
        
        res_id <- data.frame(timepoint=k)
        
        print(paste("timepoint =", k))
        return(list(samples = res_samples,
                    R = res_R,
                    id = res_id,
                    summary = def))
  }, .progress = TRUE)

save_warnings <- warnings()
res <- transpose(res)

results_id <- bind_rows(res$id)

res_samples <- lapply(seq_along(res$samples), function(i) {
    samples_scen <- res$samples[[i]] |>
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
  
  return(list(samples = res_samples,
              id = results_id,
              warnings = save_warnings,
              R = res_R,
              summary = res$summary))
}


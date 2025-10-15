
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
                          obs_scale,
                          report_freq="day"){
  
  ## Scenarios
  
  scen_values <- c(0, 0.25, 0.8, 1, 1.25, 2) 
  names(scen_values) <- c("no delay", "very low", "low", "correct", "high", "very high") 
  
  start_date <- min(case_data$date)
  end_date <- max(case_data$date)
  
  ## Dealing with missing data
  
  if(report_freq=="week"){
    fill_missing(
      case_data,
      missing_dates = c("accumulate"),
      missing_obs = c("accumulate"),
      obs_column = "confirm",
      by = NULL
    )
  } else if (report_freq=="day"){
    fill_missing(
      case_data,
      missing_dates = c("ignore"),
      missing_obs = c("ignore"),
      obs_column = "confirm",
      by = NULL
    )
  }
  

target_dates  <- seq.Date(from=start_date, to=end_date, by=paste(freq_fc, "weeks"))

 # For each target date, take that or the next available
scen_timepoints <- sapply(target_dates, function(target){
available <- case_data$date[case_data$date >= target]
if(length(available)>0){
return(min(available))
}else{
return(NA)}
}) |> as.Date()
# Remove NAs from end of range 
scen_timepoints <- scen_timepoints[!is.na(scen_timepoints)]

# Remove first timepoint
scen_timepoints <- scen_timepoints[-1]

names(scen_timepoints) <- seq_along(scen_timepoints)

# Make sure max number of timepoints is 8 to ensure quicker runtime
if(length(scen_timepoints)>8){scen_timepoints <- scen_timepoints[1:8]}

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
        if(rep_max>0){
        reporting_delay <- LogNormal(mean=rep_mean*scen_values[j],
                                     sd=rep_sd,
                                     max=rep_max)} else {reporting_delay <- Fixed(0)}
        } else {
          inc_period <- Fixed(0)
          reporting_delay <- Fixed(0)
        }

case_segment <- case_segment[order(case_segment$date), ]

start_runtime <- Sys.time()
          def <- estimate_infections(case_segment,
                                     generation_time = generation_time_opts(gen_time),
                                     delays = delay_opts(inc_period + reporting_delay),
                                     obs=obs_opts(family="negbin", scale=Fixed(obs_scale)),
                                     rt=rt_opts(future=rt_opts_choice),
                                     stan = stan_opts(samples = 3000,
                                                      return_fit = FALSE,
                                                      control=list(adapt_delta=0.99,
                                                                   max_treedepth=20)),
                                     forecast = forecast_opts(horizon=14),
                                     verbose = FALSE)
          
        # Recording runtime
          
        end_runtime <- Sys.time()
        elapsed_seconds <- as.numeric(difftime(end_runtime, start_runtime, units = "secs"))
        timing_log <- data.frame(
          timepoint=k, 
          gen_time=names(scen_values)[var],
          inc_period=names(scen_values)[j],
          elapsed_seconds=elapsed_seconds
        )

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
                    summary = def,
                    timing = timing_log))
  }, .progress = TRUE)

  save_warnings <- warnings()
  res <- transpose(res)
  
  res_samples <- lapply(seq_along(res$samples), function(i) {
    samples_scen <- res$samples[[i]] |>
      mutate(model="EpiNow2")
    
    # Add ID
    samples_scen$result_list <- i
    
    # Bind to dataframe
    return(samples_scen)
    
  })
  
  res_id <- lapply(seq_along(res$id), function(i){
    res_id <- res$id[[i]] |>
      mutate(result_list=i)
    return(res_id)
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
  
  res_id <- bind_rows(res_id)
  res_R <- bind_rows(res_R)
  res_timing <- bind_rows(res$timing)
  
  return(list(samples = res_samples,
              id = res_id,
              R = res_R,
              summary = res$summary,
              warnings = save_warnings,
              timing = res_timing))
}


sim_weightprior <- function(case_data,
                          var,
                          gen_mean_mean,
                          gen_mean_sd,
                          gen_sd_mean,
                          gen_sd_sd,
                          gen_max,
                          inc_mean_mean,
                          inc_mean_sd,
                          inc_sd_mean,
                          inc_sd_sd,
                          inc_max,
                          rep_mean_mean,
                          rep_mean_sd,
                          rep_sd_mean,
                          rep_sd_sd,
                          rep_max,
                          freq_fc=4,
                          weeks_inc=12,
                          rt_opts_choice,
                          weight_prior,
                          obs_scale){
  

  # Define start and end dates
start_date <- min(case_data$date)
end_date <- max(case_data$date)

target_dates <- seq.Date(from=start_date, to=end_date, by=paste(freq_fc, "weeks"))

# Find date or next available
scen_timepoints <- sapply(target_dates, function(target){
available <- case_data$date[case_data$date >= target]
if(length(available) > 0) {
return(min(available))
} else {
return(NA)
}
}) |> as.Date()

# Remove NAs from end of range
scen_timepoints <- scen_timepoints[!is.na(scen_timepoints)]

# Remove first timepoint
scen_timepoints <- scen_timepoints[-1]

  names(scen_timepoints) <- c(1:length(scen_timepoints))

# Make sure max number of timepoints is 8 to ensure quicker runtime
if(length(scen_timepoints)>8){scen_timepoints <- scen_timepoints[1:8]}
  
  scenarios <- expand.grid(
    k = seq_along(scen_timepoints)
  )
  
res <- pmap(scenarios, \(k) {
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
        
        if(rep_max>0) {reporting_delay <- LogNormal(mean=Normal(rep_mean_mean, rep_mean_sd),
                                     sd=Normal(rep_sd_mean, rep_sd_sd),
                                     max=rep_max)} else {reporting_delay <- Fixed(0)}

case_segment <- case_segment[order(case_segment$date), ]

start_runtime <- Sys.time()

          def <- estimate_infections(case_segment,
                                     generation_time = generation_time_opts(gen_time, weight_prior=weight_prior),
                                     delays = delay_opts(inc_period + reporting_delay, weight_prior=weight_prior),
                                     obs=obs_opts(family="negbin", scale=obs_scale, na="missing"),
                                     rt=rt_opts(future=rt_opts_choice),
                                     stan = stan_opts(samples = 3000,
                                                      return_fit = FALSE,
                                                      control=list(adapt_delta=0.99,
                                                                   max_treedepth=20)),
                                    horizon=14,
                                    verbose = FALSE)
          
        # Recording runtime
        end_runtime <- Sys.time()
        elapsed_seconds <- as.numeric(difftime(end_runtime, start_runtime, units = "secs"))
        timing_log <- data.frame(
          timepoint=k, 
          elapsed_seconds=elapsed_seconds)

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
                    summary = def,
                    timing = timing_log))
  }, .progress = TRUE)

save_warnings <- warnings()
res <- transpose(res)

res_samples <- lapply(seq_along(res$samples), function(i) {
  samples_scen <- res$samples[[i]] |>
    mutate(model="EpiNow2")
  
  # Add ID
  samples_scen$result_list <- i
  
  # Bind to dataframe
  return(samples_scen)
  
})

res_id <- lapply(seq_along(res$id), function(i){
  res_id <- res$id[[i]] |>
    mutate(result_list=i)
  return(res_id)
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

res_id <- bind_rows(res_id)
res_R <- bind_rows(res_R)
res_timing <- bind_rows(res$timing)

  return(list(samples = res_samples,
              id = res_id,
              R = res_R,
              summary = res$summary,
              warnings = save_warnings,
              timing = res_timing))
}


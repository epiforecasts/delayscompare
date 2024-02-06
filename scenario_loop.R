simulate_scenarios <- function(case_data,
                               baseline_gen_time,
                               baseline_inc_period,
                               reporting_delay,
                               rt_prior, 
                               timesplit=10){

## Scenarios

scen_values <- c(0.25, 0.75, 1, 1.25, 2)
names(scen_values) <- c("very low", "low", "correct", "high", "very high")
  
scen_timepoints <- quantile(case_data$date, probs=seq(from=0.1, to=1, by=1/timesplit), type=1)
names(scen_timepoints) <- c(1:timesplit)

# FOR TESTING:
#scen_values <- scen_values[1:2]
scen_timepoints <- scen_timepoints[1:2]
##

results_list <- list()
results_id <- data.frame()

for(i in 4:length(scen_values)){
  for(j in 1:length(scen_values)){
    for(k in 1:length(scen_timepoints)){
      
    # Case data 
    case_segment <- case_data |>
      filter(date <= scen_timepoints[k])
    
    # Generation interval
    gen_time <- baseline_gen_time
    if(length(gen_time$mean_mean)==0){stop("Generation time needs uncertain distribution")}
    gen_time$mean_mean <- gen_time$mean_mean*scen_values[i]
    
    # Incubation period
    inc_period <- baseline_inc_period
    if(length(inc_period$mean_mean)==0){stop("Incubation period needs uncertain distribution")}
    inc_period$mean_mean <- inc_period$mean_mean*scen_values[j]
    
    def <- estimate_infections(case_segment,
                               generation_time = generation_time_opts(gen_time),
                               delays = delay_opts(inc_period + reporting_delay),
                               rt = rt_opts(prior=rt_prior))
  
    results_list[[length(results_list)+1]] <- def
    
    scen_id <- data.frame(result_list=length(results_list),
                     timepoint=k,
                     gen_time=names(scen_values)[i],
                     inc_period=names(scen_values)[j])
    
    results_id <- rbind(results_id, scen_id)
    }
  }
}

return(list(results_list,
            results_id))
}

simulate_scenarios2 <- function(case_data,
                                gen_mean,
                                gen_sd,
                                inc_mean,
                                inc_sd,
                                reporting_delay,
                                rt_prior, 
                                timesplit=10){
  
  ## Scenarios
  
  scen_values <- c(0.25, 0.75, 1, 1.25, 2)
  names(scen_values) <- c("very low", "low", "correct", "high", "very high")
  
  scen_timepoints <- quantile(case_data$date, probs=seq(from=0.1, to=1, by=1/timesplit), type=1)
  names(scen_timepoints) <- c(1:timesplit)
  
  # FOR TESTING:
  #scen_values <- scen_values[1:2]
  scen_timepoints <- scen_timepoints[1:2]
  ##
  
  results_list <- list()
  results_id <- data.frame()
  
  for(i in 4:length(scen_values)){
    for(j in 1:length(scen_values)){
      for(k in 1:length(scen_timepoints)){
        
        # Case data 
        case_segment <- case_data |>
          filter(date <= scen_timepoints[k])
        
        # Generation interval
        gen_time <- dist_spec(mean=gen_mean*scen_values[i], 
                              sd=gen_sd,
                              max=30, 
                              distribution="gamma")
        
        # Incubation period
        inc_period <- dist_spec(mean=inc_mean*scen_values[j],
                                sd=inc_sd,
                                max=30,
                                distribution="lognormal")
  
        def <- estimate_infections(case_segment,
                                   generation_time = generation_time_opts(gen_time),
                                   delays = delay_opts(inc_period + reporting_delay),
                                   rt = rt_opts(prior=rt_prior))
        
        results_list[[length(results_list)+1]] <- def
        
        scen_id <- data.frame(result_list=length(results_list),
                              timepoint=k,
                              gen_time=names(scen_values)[i],
                              inc_period=names(scen_values)[j])
        
        results_id <- rbind(results_id, scen_id)
      }
    }
  }
  
  return(list(results_list,
              results_id))
}





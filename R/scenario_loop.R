
sim_scenarios <- function(case_data,
                                gen_mean,
                                gen_sd,
                                inc_mean,
                                inc_sd,
                                reporting_delay,
                                rt_prior, 
                                timesplit=10){
  
  ## Scenarios
  
  scen_values <- c(0.25, 0.75, 1, 1.25, 2) # no delay
  names(scen_values) <- c("very low", "low", "correct", "high", "very high")
  
  scen_timepoints <- quantile(case_data$date, probs=seq(from=0.1, to=1, by=1/timesplit), type=1)
  names(scen_timepoints) <- c(1:timesplit)
  
  ########### FOR TESTING: ##################
  
 # scen_timepoints <- scen_timepoints[1:2]
  scen_values <- scen_values[1:2]
  
  ###########################################
  
  results_list <- list()
  results_id <- data.frame()
  
  for(i in 1:length(scen_values)){
    for(j in 1:length(scen_values)){
      for(k in 1:length(scen_timepoints)){
        
        # Case data 
        case_segment <- case_data |>
          filter(date <= scen_timepoints[k])
        
        # Generation interval
        gen_time <- dist_spec(mean=gen_mean*scen_values[i],
                          sd=gen_sd,
                          distribution="gamma",
                          max=30)
        
        # Incubation period
        inc_period <- dist_spec(mean=inc_mean*scen_values[j],
                                sd=inc_sd,
                                distribution="lognormal",
                                max=30)
  
        def <- estimate_infections(case_segment,
                                   generation_time = generation_time_opts(gen_time),
                                   delays = delay_opts(inc_period + reporting_delay))
        
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

###################
#### CRPS PLOT ####
###################

plotcrps <- function(res_samples, 
                     res_id, 
                     sim_data){
  
  # Add simulated data
  res_samples <- sim_data |> 
    filter(variable=="reported_cases") |>
    rename(true_value=value) |>
    select(-variable) |>
    right_join(res_samples, by="date") 
  
  check_forecasts(res_samples)
  
  # Get rid of all columns that aren't date, true_value, prediction, sample
  
  res_samples <- res_samples |>
    select(date, true_value, prediction, sample, model, result_list, type)
  
  # Log transform observations and predicted values
  
  res_samples <- transform_forecasts(res_samples, fun = log_shift, offset=1, label="log")
  
  res_samples |>
    check_forecasts()
  
  ebola_scores <- res_samples |>
    # filtering out what I don't need to save memory
    filter(type=="forecast", scale=="log") |>
    set_forecast_unit(c("date", "model", "result_list", "type")) |>
    score() |>
    summarise_scores(by=c("model", "type", "result_list"))
  
  ## Add the info for each scenario to the plot
  ebola_scores_details <- ebola_scores |>
    left_join(res_id, by="result_list")
  
  ### Heatmap by timepoint ###
  
  # Need to turn inc_period and gen_time into factors to make sure ordering is correct
  ebola_scores_details$inc_period <- factor(ebola_scores_details$inc_period, levels=c("very low", "low", "correct", "high", "very high"))
  ebola_scores_details$gen_time <- factor(ebola_scores_details$gen_time, levels=c("very low", "low", "correct", "high", "very high"))
  
  sim_data_cases <- sim_data |> 
    filter(variable=="reported_cases") 
  
  ebola_timepoints <- sim_data_cases$date[c(1:(nrow(sim_data_cases) %/% (8*7)))*8*7]
  
  sim_data_timepoints <- sim_data_cases |> filter(date %in% ebola_timepoints)
  
  timeseries_ebola <- ggplot() + 
    geom_line(sim_data_cases, mapping=aes(x=date, y=value)) + 
    geom_point(sim_data_timepoints, mapping=aes(x=date,y=value), color="red") +
    xlab("Date") +
    ylab("Reported cases") +
    scale_x_continuous(breaks=sim_data_cases$date[grepl("-01$",sim_data_cases$date)]) +
    lshtm_theme()
  
  timepoints_lab <- data.frame(timepoint=c(1:length(ebola_timepoints)),
                               timepoint_lab=ebola_timepoints)
  
  ebola_scores_details <- ebola_scores_details |>
    left_join(timepoints_lab, by="timepoint")
  
  heatmap_ebola <- 
    ggplot(ebola_scores_details, aes(x=gen_time, y=inc_period)) +
    geom_tile(aes(fill=crps)) +
    facet_wrap(~timepoint_lab, nrow=1) +
    xlab("Generation time") +
    ylab("Incubation period") +
    scale_fill_gradientn(colours = terrain.colors(50), name="CRPS for one-week forecast") +
    theme_classic() +
    theme(axis.text.x = element_text(angle=45, hjust=1)) +
    lshtm_theme()
  
  heatmap_ebola_space <- plot_grid(NULL, heatmap_ebola, NULL, rel_widths=c(0.08, 1, 0.05), nrow=1)
  
  return(plot_grid(timeseries_ebola, heatmap_ebola_space, ncol=1))}

  
#####################################
#### Forecasts by parameter plot ####
#####################################

plotforecasts <- function(res_samples,
                            res_id,
                            sim_data){
    
    res_samples_plot <- res_samples |> 
      filter(type=="forecast") |>
      # add info
      left_join(res_id, by="result_list")
    
    sim_data_cases <- sim_data |> 
      filter(variable=="reported_cases") 
    
    # Add rest of data
    res_samples_plot <- sim_data_cases |>
      rename(true_value=value) |>
      select(date, true_value) |>
      left_join(res_samples_plot, by="date")
    
    res_samples_plot$gen_time <- factor(res_samples_plot$gen_time, levels=c("very low", "low", "correct", "high", "very high"))
    res_samples_plot$inc_period <- factor(res_samples_plot$inc_period, levels=c("very low", "low", "correct", "high", "very high"))
    
    #### Generation time plot ####
    
    res_samples_plot_gentime <- res_samples_plot |>
      filter(is.na(inc_period) | inc_period=="correct")
    
    res_samples_plot_gentime <- sample_to_quantile(res_samples_plot_gentime, quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95), type = 7)
    
    res_samples_plot_gentime <- res_samples_plot_gentime |>
      pivot_wider(names_from="quantile",
                  values_from="prediction")
    
    res_samples_plot_gentime <- res_samples_plot_gentime |>
      rename(lower90="0.05",
             upper90="0.95",
             lower50="0.25",
             upper50="0.75",
             median="0.5") # |>
    # filter(!is.na(gen_time))
    
    # Get rid of NA
    res_samples_plot_gentime <- res_samples_plot_gentime |>
      filter(!is.na(gen_time))
    
    ## Generation time plot ##
    
    # Simulated data
    forecasts_gentime <- ggplot() + geom_line(sim_data_cases, mapping=aes(x=date, y=value))
    # Forecasts
    for(i in 1:max(res_samples_plot_gentime$timepoint)){
      forecasts_gentime <- forecasts_gentime + 
        geom_line(res_samples_plot_gentime |> filter(timepoint==i | is.na(timepoint)), mapping=aes(x=date, y=median, color=gen_time)) +
        geom_ribbon(res_samples_plot_gentime |> filter(timepoint==i | is.na(timepoint)), mapping=aes(x=date, ymin=lower50, ymax=upper50, fill=gen_time), alpha=0.5) +
        geom_ribbon(res_samples_plot_gentime |> filter(timepoint==i | is.na(timepoint)), mapping=aes(x=date, ymin=lower90, ymax=upper90, fill=gen_time), alpha=0.2)
    }
    # Aesthetics
    forecasts_gentime <- forecasts_gentime + 
      scale_fill_viridis(discrete=TRUE, name="Generation time") +
      scale_colour_viridis(discrete=TRUE, name="Generation time") +
      xlab("Date") +
      ylab("Reported casts") +
      facet_wrap(~gen_time, ncol=1) +
      lshtm_theme() +
      ylim(0, max(res_samples_plot_gentime$upper90))
    
    #### Incubation period plot ####
    
    res_samples_plot_incperiod <- res_samples_plot |>
      filter(is.na(gen_time) | gen_time=="correct")
    
    res_samples_plot_incperiod <- sample_to_quantile(res_samples_plot_incperiod, quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95), type = 7)
    
    res_samples_plot_incperiod <- res_samples_plot_incperiod |>
      pivot_wider(names_from="quantile",
                  values_from="prediction")
    
    res_samples_plot_incperiod <- res_samples_plot_incperiod |>
      rename(lower90="0.05",
             upper90="0.95",
             lower50="0.25",
             upper50="0.75",
             median="0.5") 
    
    # Get rid of NA
    res_samples_plot_incperiod <- res_samples_plot_incperiod |>
      filter(!is.na(inc_period))
    
    ## Incubation period plot ##
    
    # Simulated data
    forecasts_incperiod <- ggplot() + geom_line(sim_data_cases, mapping=aes(x=date, y=value))
    # Forecasts
    for(i in 1:max(res_samples_plot_incperiod$timepoint)){
      forecasts_incperiod <- forecasts_incperiod + 
        geom_line(res_samples_plot_incperiod |> filter(timepoint==i | is.na(timepoint)), mapping=aes(x=date, y=median, color=inc_period)) +
        geom_ribbon(res_samples_plot_incperiod |> filter(timepoint==i | is.na(timepoint)), mapping=aes(x=date, ymin=lower50, ymax=upper50, fill=inc_period), alpha=0.5) +
        geom_ribbon(res_samples_plot_incperiod |> filter(timepoint==i | is.na(timepoint)), mapping=aes(x=date, ymin=lower90, ymax=upper90, fill=inc_period), alpha=0.2)
    }
    # Aesthetics
    forecasts_incperiod <- forecasts_incperiod + 
      scale_fill_viridis(discrete=TRUE, name="Incubation period") +
      scale_colour_viridis(discrete=TRUE, name="Incubation period") +
      xlab("Date") +
      ylab("Reported casts") +
      facet_wrap(~inc_period, ncol=1) +
      lshtm_theme() +
      ylim(0, max(res_samples_plot_incperiod$upper90))
    
    
    plot_grid(forecasts_gentime, forecasts_incperiod, ncol=2)
}



  
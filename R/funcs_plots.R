###################
#### CRPS PLOT ####
###################

plotcrps <- function(res_samples, 
                     res_id, 
                     sim_data){
  
  res_samples <- res_samples |> 
    filter(type=="forecast") |>
    # add info
    left_join(res_id, by="result_list")
  
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
    left_join(res_id, by="result_list") |>
    group_by(timepoint) |>
    filter(date == max(date)) |>
    ungroup()
  
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

############################
#### Plots by timepoint ####
############################

plotbytime <- function(res_samples,
                       res_id,
                       sim_data,
                       disease,
                       forecastonly=FALSE){
  
  ## Getting ready to plot ##
  
  res_samples <- res_samples |> 
    # add info
    left_join(res_id, by="result_list")
  
  
  # Make factors so that ordering of facets is right
  res_samples$gen_time <- factor(res_samples$gen_time, levels=c("very low", "low", "correct", "high", "very high"))
  res_samples$inc_period <- factor(res_samples$inc_period, levels=c("very high", "high", "correct", "low", "very low"))
  
  # Get quantiles
  res_samples_plot <- sample_to_quantile(res_samples, quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95), type = 7)
  
  res_samples_plot <- res_samples_plot |>
    pivot_wider(names_from="quantile",
                values_from="prediction")
  
  res_samples_plot <- res_samples_plot |>
    rename(lower90="0.05",
           upper90="0.95",
           lower50="0.25",
           upper50="0.75",
           median="0.5")
  
  ## Optional filter to plot just forecasts
  if(forecastonly==TRUE){
    res_samples_plot <- res_samples_plot |>
      filter(type=="forecast")
  }
  
  # Add observed cases
  res_samples_plot <- sim_data |> 
    filter(variable=="reported_cases") |>
    rename(true_value=value) |>
    select(-variable) |>
    right_join(res_samples_plot, by="date") 
  
  ## Plot by timepoint, faceted by generation time and incubation period ##
  
  results_list <- list()
  
  if(forecastonly==FALSE){
  for(i in 1:max(res_samples_plot$timepoint)){
    bytimepoint <- res_samples_plot |>
      filter(timepoint==i)
    plot_timepoint <- ggplot(bytimepoint) +
      geom_col(aes(x=date, y=true_value), alpha=0.5) +
      geom_line(aes(x=date, y=median, colour=type)) +
      geom_ribbon(aes(x=date, ymin=lower50, ymax=upper50, fill=type), alpha=0.5) +
      # geom_ribbon(aes(x=date, ymin=lower90, ymax=upper90, fill=type), alpha=0.2) +
      facet_grid(inc_period~gen_time) +
      ylab("Observed cases") +
      xlab("Date") +
      lshtm_theme() +
      scale_x_continuous(breaks= bytimepoint$date[grepl("-01$", bytimepoint$date)],
                         sec.axis = sec_axis(~ . , name = "Generation time", breaks = NULL, labels = NULL)) +
      scale_y_continuous(sec.axis = sec_axis(~ . , name = "Incubation period", breaks = NULL, labels = NULL)) 
    
    results_list[[i]] <- assign(paste0("plot_", disease, "timepoint", i), plot_timepoint)
  }
  }
  
  if(forecastonly==TRUE){
    for(i in 1:max(res_samples_plot$timepoint)){
      bytimepoint <- res_samples_plot |>
        filter(timepoint==i)
      plot_timepoint <- ggplot(bytimepoint) +
        geom_col(aes(x=date, y=true_value), alpha=0.5) +
        geom_line(aes(x=date, y=median), colour="#619CFF") +
        geom_ribbon(aes(x=date, ymin=lower50, ymax=upper50), alpha=0.5, fill="#619CFF") +
        # geom_ribbon(aes(x=date, ymin=lower90, ymax=upper90, fill=type), alpha=0.2) +
        facet_grid(inc_period~gen_time) +
        ylab("Observed cases") +
        xlab("Date") +
        lshtm_theme() +
        scale_x_continuous(breaks= bytimepoint$date[grepl("-01$", bytimepoint$date)],
                           sec.axis = sec_axis(~ . , name = "Generation time", breaks = NULL, labels = NULL)) +
        scale_y_continuous(sec.axis = sec_axis(~ . , name = "Incubation period", breaks = NULL, labels = NULL)) 
      
      results_list[[i]] <- assign(paste0("plot_", disease, "timepoint", i), plot_timepoint)
    }
    
  }
  return(results_list)
}

#################################################
#### "Correct" forecasts across time horizon ####
#################################################

plotcorrect <- function(
    res_samples,
    res_id,
    sim_data,
    rt_timeseries){
  
  # Add info
  res_samples <- res_samples |>
    left_join(res_id, by="result_list")
  
  # Add observed data
  res_samples <- sim_data |> 
    filter(variable=="reported_cases") |>
    rename(true_value=value) |>
    select(-variable) |>
    right_join(res_samples, by="date") 
  
  res_samples_correct <- res_samples |>
    filter(inc_period=="correct", gen_time=="correct")
  
  res_samples_correct  <- sample_to_quantile(res_samples_correct, quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95), type = 7)
  res_samples_correct <- res_samples_correct  |>
    pivot_wider(names_from="quantile",
                values_from="prediction")
  
  res_samples_correct <- res_samples_correct |>
    rename(lower90="0.05",
           upper90="0.95",
           lower50="0.25",
           upper50="0.75",
           median="0.5")
  
  plot_correct <- ggplot(res_samples_correct) +
    geom_col(aes(x=date, y=true_value), alpha=0.5) +
    geom_line(aes(x=date, y=median, colour=type)) +
    geom_ribbon(aes(x=date, ymin=lower50, ymax=upper50, fill=type), alpha=0.5) +
    # geom_ribbon(aes(x=date, ymin=lower90, ymax=upper90, fill=type), alpha=0.2) +
    facet_wrap(~timepoint, nrow=1, scale="free") +
    ylab("Observed cases") +
    xlab("Date") +
    lshtm_theme() +
    scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Adding bars showing forecast horizon
  highlight_periods <- res_samples_correct |>
    group_by(timepoint, type) |>
    summarise(start_date=min(date),
              end_date=max(date)) |>
    filter(type=="forecast")
  
  plot_rt <- ggplot(rt_timeseries) + 
    geom_rect(data = highlight_periods, aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf), 
              fill="#619CFF",alpha = 0.5) +
    geom_line(aes(x=date, y=R), colour="firebrick4", size=1.2) +
    geom_ribbon(aes(x=date, ymin=lower_50, ymax=upper_50), alpha=0.5, fill="firebrick4") +
    geom_ribbon(aes(x=date, ymin=lower_90, ymax=upper_90), alpha=0.3, fill="firebrick4") +
    xlab("Date") +
    ylab("Rt") +
    lshtm_theme() +
    scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y", limits = c(min(rt_timeseries$date), max(rt_timeseries$date)), expand=c(0,0))
  
  # Adding the simulated data plot 
  
  sim_data_cases <- sim_data |> 
    filter(variable=="reported_cases") 
  
  plot_simdata <- ggplot() + 
    geom_line(sim_data_cases, mapping=aes(x=date, y=value)) + 
    geom_rect(data = highlight_periods, aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf), 
              fill = "#619CFF", alpha = 0.5) +
    xlab("Date") +
    ylab("Reported cases") +
    scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y", limits = c(min(rt_timeseries$date), max(rt_timeseries$date)), expand=c(0,0)) +
    lshtm_theme()
  
  ## Adding to one plot ##
  
  plots_upper <- plot_grid(plot_rt, plot_simdata, align="v", ncol=1)
  
  plot_correct_space <- plot_grid(NULL, plot_correct, NULL, rel_widths=c(0.035, 1, 0.05), nrow=1)
  
  plot_grid(plots_upper, plot_correct_space, ncol=1)
  
}



  
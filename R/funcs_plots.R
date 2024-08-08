###################
#### CRPS PLOT ####
###################

plotcrps <- function(res_samples, 
                     res_id, 
                     sim_data,
                     forecast_freq){
  
  res_samples <- res_samples |> 
    filter(type=="forecast") |>
    # add info
    left_join(res_id, by=c("gt", "result_list"))
  
  # Add simulated data
  res_samples <- sim_data |> 
    filter(variable=="reported_cases") |>
    rename(true_value=value) |>
    select(-variable) |>
    right_join(res_samples, by="date") 
  
  #check_forecasts(res_samples)
  
  # Get rid of all columns that aren't date, true_value, prediction, sample
  
  res_samples <- res_samples |>
    select(date, true_value, prediction, sample, model, result_list, type, gt)
  
  # Log transform observations and predicted values
  
  gc()
  
  res_samples <- transform_forecasts(res_samples, fun = log_shift, offset=1, label="log") |>
    filter(scale=="log")
  
  #res_samples |>
  #  check_forecasts()
  
  gc()
  
  scores <- res_samples |>
    set_forecast_unit(c("date", "model", "result_list", "type", "gt")) |>
    score() |>
    summarise_scores(by=c("model", "type", "result_list", "date", "gt"))
  
  gc()
  
  ## Add the info for each scenario to the plot
  scores_details <- scores |>
    left_join(res_id, by=c("result_list", "gt")) |>
    group_by(result_list, gt) |>
    filter(date == max(date)) |>
    ungroup()
  
  ### Heatmap by timepoint ###
  
  # Need to turn inc_period and gen_time into factors to make sure ordering is correct
  scores_details$inc_period <- factor(scores_details$inc_period, levels=c("no delay", "very low", "low", "correct", "high", "very high"))
  scores_details$gen_time <- factor(scores_details$gen_time, levels=c("no delay", "very low", "low", "correct", "high", "very high"))
  
  sim_data_cases <- sim_data |> 
    filter(variable=="reported_cases") 
  
  dis_timepoints <- sim_data_cases$date[c(1:(nrow(sim_data_cases) %/% (forecast_freq*7)))*forecast_freq*7]
  
  sim_data_timepoints <- sim_data_cases |> filter(date %in% dis_timepoints)
  
  timeseries_dis <- ggplot() + 
    geom_line(sim_data_cases, mapping=aes(x=date, y=value)) + 
    geom_point(sim_data_timepoints, mapping=aes(x=date,y=value), color="red") +
    xlab("Date") +
    ylab("Reported cases") +
    scale_x_continuous(breaks=sim_data_cases$date[grepl("-01$",sim_data_cases$date)]) +
    lshtm_theme()

  timepoints_lab <- data.frame(timepoint=c(1:length(dis_timepoints)),
                               timepoint_lab=dis_timepoints)
  
  scores_details <- scores_details |>
    left_join(timepoints_lab, by="timepoint")
  
  heatmap_dis <- 
    ggplot(scores_details, aes(x=gen_time, y=inc_period)) +
    geom_tile(aes(fill=crps)) +
    facet_wrap(~timepoint_lab, nrow=1) +
    xlab("Generation time") +
    ylab("Incubation period") +
    scale_fill_gradientn(colours = terrain.colors(50), name="CRPS for two-week forecast") +
    theme_classic() +
    theme(axis.text.x = element_text(angle=45, hjust=1)) +
    lshtm_theme()
  
  heatmap_dis_space <- plot_grid(NULL, heatmap_dis, NULL, rel_widths=c(0.04, 1, 0.02), nrow=1)
  
  return(plot_grid(timeseries_dis, heatmap_dis_space, ncol=1))}

###############################
#### Plot rankings + cases ####
###############################

plotrankcase <- function(res_samples,
                         res_id,
                         sim_data,
                         forecast_freq=4) {
  
  res_samples <- res_samples |>
    filter(type=="forecast") |>
    # add info
    left_join(res_id, by=c("result_list", "gt"))
  
  # Add simulated data
  res_samples <- sim_data |>
    filter(variable=="reported_cases") |>
    rename(true_value=value) |>
    select(-variable) |>
    right_join(res_samples, by="date")
  
  #check_forecasts(res_samples)
  
  # Get rid of all columns that aren't date, true_value, prediction, sample
  
  res_samples <- res_samples |>
    select(date, true_value, prediction, sample, model, result_list, gt, type)
  
  # Log transform observations and predicted values
  
  res_samples <- transform_forecasts(res_samples, fun = log_shift, offset=1, label="log")
  
  res_samples |>
    check_forecasts()
  
  scores <- res_samples |>
    # filtering out what I don't need to save memory
    filter(type=="forecast", scale=="log") |>
    set_forecast_unit(c("date", "model", "result_list", "gt", "type")) |>
    score()
  
  rankings <- scores |>
    left_join(res_id, by = c("result_list", "gt")) |>
    group_by(timepoint) |>
    filter(date == max(date)) |>
    mutate(rank = rank(crps)) |>
    ungroup()
  
  rankings <- rankings |>
    mutate(
      inc_period = factor(
        inc_period, levels=c("no delay", "very low", "low", "correct", "high", "very high")
      ),
      gen_time = factor(
        gen_time, levels=c("no delay", "very low", "low", "correct", "high", "very high")
      ))
  
  ## Add cases 
  
  sim_data_cases <- sim_data |> 
    filter(variable=="reported_cases") 
  
  dis_timepoints <- sim_data_cases$date[c(1:(nrow(sim_data_cases) %/% (forecast_freq*7)))*forecast_freq*7]
  
 sim_data_timepoints <- sim_data_cases |> filter(date %in% dis_timepoints)
  
  timepoint_labels <- setNames(as.character(dis_timepoints), 
                               c(1:length(dis_timepoints)))
  
  timeseries_dis <- ggplot() + 
    geom_line(sim_data_cases, mapping=aes(x=date, y=value)) + 
    geom_point(sim_data_timepoints, mapping=aes(x=date,y=value), color="red") +
    xlab("Date") +
    ylab("Reported cases") +
    scale_x_continuous(breaks=sim_data_cases$date[grepl("-01$",sim_data_cases$date)]) +
    lshtm_theme()
  
  rank_plot <- ggplot(rankings, aes(x=gen_time, y=inc_period)) +
    geom_tile(aes(fill=rank)) +
    xlab("Generation time") +
    ylab("Incubation period") +
    scale_fill_gradientn(colours = terrain.colors(50), name="Ranking of two-week forecast") +
    theme_classic() +
    theme(axis.text.x = element_text(angle=45, hjust=1)) +
    lshtm_theme() +
    facet_wrap(timepoint~., labeller=as_labeller(timepoint_labels), nrow=1)
  
  
  return(plot_grid(timeseries_dis, rank_plot, ncol=1))
}



#######################
#### Plot crps - Rt ####
########################

plotcrps_rt <- function(res_rt_samples, 
                        res_id, 
                        rt_dis,
                        forecast_freq){
  
  dis_timepoints <- rt_dis$date[c(1:(nrow(rt_dis) %/% (forecast_freq*7)))*forecast_freq*7]
  
  ## Filter out Rt estimates at forecasting timepoint
  res_rt_samples <- res_rt_samples |>
    filter(date %in% (dis_timepoints-1)) # dis_timepoints gives time forecast starts, want time of last estimate
  
  res_rt_samples <- res_rt_samples |> 
    filter(type=="estimate") |>
    rename(prediction=value) |>
    # add info
    left_join(res_id, by=c("result_list", "gt"))
  
  # Add simulated data
  res_rt_samples <- rt_dis |> 
    rename(true_value=R) |>
    right_join(res_rt_samples, by="date") 
  
  #check_forecasts(res_samples)
  
  # Get rid of all columns that aren't date, true_value, prediction, sample
  
  res_rt_samples <- res_rt_samples |>
    select(date, true_value, prediction, sample, model, result_list, gt, type)
  
  # Log transform observations and predicted values
  
  res_rt_samples <- transform_forecasts(res_rt_samples, fun = log_shift, offset=1, label="log")
  
  #res_samples |>
  #  check_forecasts()
  
  scores <- res_rt_samples |>
    # filtering out what I don't need to save memory
    filter(type=="estimate", scale=="log") |>
    set_forecast_unit(c("date", "model", "result_list", "gt", "type")) |>
    score() |>
    summarise_scores(by=c("model", "type", "result_list", "gt", "date"))
  
  ## Add the info for each scenario to the plot
  scores_details <- scores |>
    left_join(res_id, by=c("result_list", "gt")) |>
    group_by(result_list) |>
    filter(date == max(date)) |>  # filtering for latest estimate
    ungroup()
  
  ### Heatmap by timepoint ###
  
  # Need to turn inc_period and gen_time into factors to make sure ordering is correct
  scores_details$inc_period <- factor(scores_details$inc_period, levels=c("no delay", "very low", "low", "correct", "high", "very high"))
  scores_details$gen_time <- factor(scores_details$gen_time, levels=c("no delay", "very low", "low", "correct", "high", "very high"))
  
  rt_timepoints <- rt_dis |> filter(date %in% dis_timepoints)
  
  timeseries_dis <- ggplot() + 
    geom_line(rt_dis, mapping=aes(x=date, y=R)) + 
    geom_point(rt_timepoints, mapping=aes(x=date,y=R), color="red") +
    xlab("Date") +
    ylab("Rt") +
    scale_x_continuous(breaks=rt_dis$date[grepl("-01$", rt_dis$date)]) +
    lshtm_theme()
  
  timepoints_lab <- data.frame(timepoint=c(1:length(dis_timepoints)),
                               timepoint_lab=dis_timepoints)
  
  scores_details <- scores_details |>
    left_join(timepoints_lab, by="timepoint")
  
  heatmap_dis <- 
    ggplot(scores_details, aes(x=gen_time, y=inc_period)) +
    geom_tile(aes(fill=crps)) +
    facet_wrap(~timepoint_lab, nrow=1) +
    xlab("Generation time") +
    ylab("Incubation period") +
    scale_fill_gradientn(colours = terrain.colors(50), name="CRPS for Rt estimate") +
    theme_classic() +
    theme(axis.text.x = element_text(angle=45, hjust=1)) +
    lshtm_theme()
  
  heatmap_dis_space <- plot_grid(NULL, heatmap_dis, NULL, rel_widths=c(0.08, 1, 0.05), nrow=1)
  
  return(plot_grid(timeseries_dis, heatmap_dis_space, ncol=1))}


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

plotbytime <- function(res_samples_timepoint,
                       res_id,
                       sim_data,
                       disease,
                       forecastonly=FALSE){
  
  # Make factors so that ordering of facets is right
  res_samples_timepoint$gen_time <- factor(res_samples_timepoint$gen_time, levels=c("very low", "low", "correct", "high", "very high"))
  res_samples_timepoint$inc_period <- factor(res_samples_timepoint$inc_period, levels=c("very high", "high", "correct", "low", "very low"))
  
  # Get quantiles
  res_samples_plot <- sample_to_quantile(res_samples_timepoint, quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95), type = 7)
  
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

    plot_timepoint <- ggplot(res_samples_plot) +
      geom_col(aes(x=date, y=true_value), alpha=0.5) +
      geom_line(aes(x=date, y=median, colour=type)) +
      geom_ribbon(aes(x=date, ymin=lower50, ymax=upper50, fill=type), alpha=0.5) +
      # geom_ribbon(aes(x=date, ymin=lower90, ymax=upper90, fill=type), alpha=0.2) +
      facet_grid(inc_period~gen_time) +
      ylab("Observed cases") +
      xlab("Date") +
      lshtm_theme() +
      scale_x_continuous(breaks= res_samples_plot$date[grepl("-01$", res_samples_plot$date)],
                         sec.axis = sec_axis(~ . , name = "Generation time", breaks = NULL, labels = NULL)) +
      scale_y_continuous(sec.axis = sec_axis(~ . , name = "Incubation period", breaks = NULL, labels = NULL)) 
    
  }

  
  if(forecastonly==TRUE){
    
      plot_timepoint <- ggplot(res_samples_plot) +
        geom_col(aes(x=date, y=true_value), alpha=0.5) +
        geom_line(aes(x=date, y=median), colour="#619CFF") +
        geom_ribbon(aes(x=date, ymin=lower50, ymax=upper50), alpha=0.5, fill="#619CFF") +
        # geom_ribbon(aes(x=date, ymin=lower90, ymax=upper90, fill=type), alpha=0.2) +
        facet_grid(inc_period~gen_time) +
        ylab("Observed cases") +
        xlab("Date") +
        lshtm_theme() +
        scale_x_continuous(breaks= res_samples_plot$date[grepl("-01$", res_samples_plot$date)],
                           sec.axis = sec_axis(~ . , name = "Generation time", breaks = NULL, labels = NULL)) +
        scale_y_continuous(sec.axis = sec_axis(~ . , name = "Incubation period", breaks = NULL, labels = NULL)) 
      
    }
    
return(plot_timepoint)
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

plotrankings <- function(res_samples,
                         res_id,
                         sim_data,
                         oneweek) {
  
  if(oneweek==TRUE){
    res_samples <- res_samples |> 
      group_by(result_list, gt, sample) |>
      slice_head(n=7) |> 
      ungroup()
  }

  res_samples <- res_samples |>
    filter(type=="forecast") |>
    # add info
    left_join(res_id, by=c("result_list", "gt"))

  # Add simulated data
  res_samples <- sim_data |>
    filter(variable=="reported_cases") |>
    rename(true_value=value) |>
    select(-variable) |>
    right_join(res_samples, by="date")

  #check_forecasts(res_samples)

  # Get rid of all columns that aren't date, true_value, prediction, sample

  res_samples <- res_samples |>
    select(date, true_value, prediction, sample, model, result_list, gt, type)

  # Log transform observations and predicted values

  res_samples <- transform_forecasts(res_samples, fun = log_shift, offset=1, label="log")

  res_samples |>
    check_forecasts()

  scores <- res_samples |>
    # filtering out what I don't need to save memory
    filter(type=="forecast", scale=="log") |>
    set_forecast_unit(c("date", "model", "result_list", "gt", "type")) |>
    score()

  rankings <- scores |>
    left_join(res_id, by = c("result_list", "gt")) |>
    group_by(timepoint) |>
    filter(date == max(date)) |>
    mutate(rank = order(crps)) |>
    ungroup()
  
  rankings <- rankings |>
    mutate(
      inc_period = factor(
        inc_period, levels=c("no delay", "very low", "low", "correct", "high", "very high")
      ),
      gen_time = factor(
        gen_time, levels=c("no delay", "very low", "low", "correct", "high", "very high")
      ))
      

  mean_rankings <- rankings |>
    group_by(gen_time, inc_period) |>
    summarise(mean_rank = mean(rank), .groups = "drop") |>
    mutate(
      inc_period = factor(
        inc_period, levels=c("no delay", "very low", "low", "correct", "high", "very high")
      ),
      gen_time = factor(
        gen_time, levels=c("no delay", "very low", "low", "correct", "high", "very high")
      )
    )
  
  forecastlength <- ifelse(oneweek==TRUE, "Ranking of one-week forecast",
                           "Ranking of two-week forecast")

  rank_plot <- ggplot(mean_rankings, aes(x=gen_time, y=inc_period)) +
    geom_tile(aes(fill=mean_rank)) +
    xlab("Generation time") +
    ylab("Incubation period") +
    scale_fill_gradientn(colours = terrain.colors(50), name=forecastlength) +
    theme_classic() +
    theme(axis.text.x = element_text(angle=45, hjust=1)) +
    lshtm_theme()
  
 # rank_plot <- ggplot(rankings, aes(x=gen_time, y=inc_period)) +
 #   geom_tile(aes(fill=rank)) +
 #   xlab("Generation time") +
 #   ylab("Incubation period") +
 #   scale_fill_gradientn(colours = terrain.colors(50), name="Ranking of one-week forecast") +
 #   theme_classic() +
 #   theme(axis.text.x = element_text(angle=45, hjust=1)) +
 #   lshtm_theme() +
 #   facet_wrap(~timepoint)

  return(rank_plot)
}

plotrankrt <- function(res_rt_est, 
                       res_id, 
                       rt_dis,
                       forecast_freq){
  
  dis_timepoints <- rt_dis$date[c(1:(nrow(rt_dis) %/% (forecast_freq*7)))*forecast_freq*7]
  
  ## Filter out Rt estimates at forecasting timepoint
  #res_rt_est <- res_rt_est |>
  #  filter(date %in% dis_timepoints) # dis_timepoints gives time forecast starts, want time of last estimate
  
  res_rt_est <- res_rt_est |> 
    rename(prediction=value) |>
    # add info
    left_join(res_id, by=c("result_list", "gt"))
  
  # Add simulated data
  res_rt_est <- rt_dis |> 
    rename(true_value=R) |>
    right_join(res_rt_est, by="date") 
  
  #check_forecasts(res_samples)
  
  # Get rid of all columns that aren't date, true_value, prediction, sample
  
  res_rt_est <- res_rt_est |>
    select(date, true_value, prediction, sample, model, result_list, gt, type)
  
  # Log transform observations and predicted values
  
  res_rt_est <- transform_forecasts(res_rt_est, fun = log_shift, offset=1, label="log")
  
  #res_samples |>
  #  check_forecasts()
  
  scores <- res_rt_est |>
    # filtering out what I don't need to save memory
    filter(scale=="log") |>
    set_forecast_unit(c("date", "model", "result_list", "gt", "type")) |>
    score() |>
    summarise_scores(by=c("model", "type", "result_list", "gt"))
  
  ## Add the info for each scenario to the plot
  rankings <- scores |>
    left_join(res_id, by = c("result_list", "gt")) |>
    group_by(timepoint) |>
    #filter(date == max(date)) |>
    mutate(rank = order(crps)) |>
    ungroup()
  ##
  
  rankings <- rankings |>
    mutate(
      inc_period = factor(
        inc_period, levels=c("no delay", "very low", "low", "correct", "high", "very high")
      ),
      gen_time = factor(
        gen_time, levels=c("no delay", "very low", "low", "correct", "high", "very high")
      ))
  
  ### Heatmap by timepoint ###
  
  rt_timepoints <- rt_dis |> filter(date %in% dis_timepoints)
  
  timepoint_labels <- setNames(as.character(dis_timepoints), 
                               c(1:length(dis_timepoints)))
  
  timeseries_dis <- ggplot() + 
    geom_line(rt_dis, mapping=aes(x=date, y=R)) + 
    geom_point(rt_timepoints, mapping=aes(x=date,y=R), color="red") +
    xlab("Date") +
    ylab("Rt") +
    scale_x_continuous(breaks=rt_dis$date[grepl("-01$", rt_dis$date)]) +
    lshtm_theme()
  
  rank_plot <- ggplot(rankings, aes(x=gen_time, y=inc_period)) +
    geom_tile(aes(fill=rank)) +
    xlab("Generation time") +
    ylab("Incubation period") +
    scale_fill_gradientn(colours = terrain.colors(50), name="CRPS for Rt estimate") +
    theme_classic() +
    theme(axis.text.x = element_text(angle=45, hjust=1)) +
    lshtm_theme() +
    facet_wrap(timepoint~., labeller=as_labeller(timepoint_labels), nrow=1)
  
  return(plot_grid(timeseries_dis, rank_plot, ncol=1))}

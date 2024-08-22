plotrankrt_tp <- function(res_rt_samples, 
                       res_id, 
                       rt_dis,
                       forecast_freq){
  
  dis_timepoints <- rt_dis$date[c(1:(nrow(rt_dis) %/% (forecast_freq*7)))*forecast_freq*7]
  
  # Take forecasts only
  
  res_rt_samples <- res_rt_samples |>
    filter(type=="forecast") 
  
  # Take 14th day only 
  res_rt_samples <- res_rt_samples |>
    group_by(variable, parameter, sample, type, model, result_list, gt) |>
    filter(date==max(date))
 
  res_rt_samples <- res_rt_samples |> 
    # add info
    left_join(res_id, by=c("result_list", "gt"))
  
  res_rt_samples <- res_rt_samples |>
    left_join(rt_dis, by="date")
  
  res_rt_samples <- as_forecast_sample(
    data=res_rt_samples,
    forecast_unit=c("date", "type", "timepoint", "gen_time", "inc_period", "model"),
    observed='R',
    predicted='value',
    model='model',
    sample_id='sample'
  )

  #check_forecasts(res_samples)
  
  # Log transform observations and predicted values
  
  res_rt_samples <- transform_forecasts(res_rt_samples, fun = log_shift, offset=1, label="log") |> 
    filter(scale=='log')
  
  #res_samples |>
  #  check_forecasts()
  
  scores <- res_rt_samples |>
    score() |>
    summarise_scores(by=c("date", "type", "timepoint", "gen_time", "inc_period", "model"))
  
  gc()
  
  
## Add the info for each scenario to the plot
  rankings <- scores |>
    group_by(timepoint) |>
    filter(date == max(date)) |>
    mutate(rank = rank(crps)) |>
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
  
 # mean_rankings <- rankings |>
 #  group_by(gen_time, inc_period, timepoint) |>
 #  summarise(mean_rank = mean(rank), .groups = "drop")
  
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
    scale_fill_gradientn(colors = terrain.colors(6), name="CRPS for Rt estimate") +
    theme_classic() +
    theme(axis.text.x = element_text(angle=45, hjust=1)) +
    lshtm_theme() +
    facet_wrap(timepoint~., labeller=as_labeller(timepoint_labels), nrow=1)
  
  return(plot_grid(timeseries_dis, rank_plot, ncol=1))}

plotrankcase <- function(res_samples,
                         res_id,
                         sim_data,
                         forecast_freq=4) {
  
  res_samples <- res_samples |>
    filter(type=="forecast") |>
    left_join(res_id, by=c("result_list", "gt"))
  
  # Add simulated data
  res_samples <- sim_data |>
    filter(variable=="reported_cases") |>
    right_join(res_samples, by="date")
  
  #check_forecasts(res_samples)

  res_samples <- as_forecast_sample(
    data=res_samples,
    forecast_unit=c("date", "type", "timepoint", "result_list", "gen_time", "inc_period", "model"),
    observed='value',
    predicted='prediction',
    model='model',
    sample_id='sample'
  )
  
  # Log transform observations and predicted values
  
  res_samples <- transform_forecasts(res_samples, fun = log_shift, offset=1, label="log")
  
  scores <- res_samples |>
    # filtering out what I don't need to save memory
    filter(type=="forecast", scale=="log") |>
    set_forecast_unit(c("date", "type", "timepoint", "result_list", "gen_time", "inc_period", "model")) |>
    score()
  
  rankings <- scores |>
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

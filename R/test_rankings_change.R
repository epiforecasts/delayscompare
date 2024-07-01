plotrankrt <- function(res_rt_est, 
                       res_id, 
                       rt_dis,
                       forecast_freq){
  
  dis_timepoints <- rt_dis$date[c(1:(nrow(rt_dis) %/% (forecast_freq*7)))*forecast_freq*7]
  
  ## Filter out Rt estimates at forecasting timepoint
  res_rt_est <- res_rt_est |>
    filter(date %in% dis_timepoints) # dis_timepoints gives time forecast starts, want time of last estimate
  
  res_rt_est <- res_rt_est |> 
    rename(prediction=value) |>
    # add info
    left_join(res_id, by=c("result_list", "gt"))
  
  rt_dis$change <- NA

  for(i in c(2:nrow(rt_dis))){
    rt_dis$change[i] <- ifelse(rt_dis$R[i] < rt_dis$R[i-1], "decreasing", "increasing")
  }
  
  rt_dis$growth <- ifelse(rt_dis$R > 1, ">1", "<1")
  
  # Add simulated data
  res_rt_est <- rt_dis |> 
    rename(true_value=R) |>
    right_join(res_rt_est, by="date") 
  
  #check_forecasts(res_samples)
  
  # Get rid of all columns that aren't date, true_value, prediction, sample

  
  res_rt_est <- res_rt_est |>
    select(date, true_value, prediction, sample, model, result_list, gt, type, change, growth)
  
  # Log transform observations and predicted values
  
  res_rt_est <- transform_forecasts(res_rt_est, fun = log_shift, offset=1, label="log")
  
  #res_samples |>
  #  check_forecasts()
  
  scores <- res_rt_est |>
    # filtering out what I don't need to save memory
    filter(scale=="log") |>
    set_forecast_unit(c("model", "result_list", "gt", "type", "change", "date", "growth")) |>
    score() |>
    summarise_scores(by=c("model", "type", "result_list", "gt", "change", "date", "growth"))
  
  ## Add the info for each scenario to the plot
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
    group_by(gen_time, inc_period, growth) |>
    summarise(mean_rank = mean(rank), .groups = "drop")
  
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
  
  rank_plot <- ggplot(mean_rankings, aes(x=gen_time, y=inc_period)) +
    geom_tile(aes(fill=mean_rank)) +
    xlab("Generation time") +
    ylab("Incubation period") +
    scale_fill_gradientn(colors = terrain.colors(6), name="CRPS for Rt estimate") +
    theme_classic() +
    theme(axis.text.x = element_text(angle=45, hjust=1)) +
    lshtm_theme() +
    facet_wrap(growth~.,nrow=1)
  
  return(plot_grid(timeseries_dis, rank_plot, ncol=1))}
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
        inc_period, levels=c("very low", "low", "correct", "high", "very high")
      ),
      gen_time = factor(
        gen_time, levels=c("very low", "low", "correct", "high", "very high")
      )
    )
  
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
    scale_fill_gradientn(colours = terrain.colors(50), name="Ranking of one-week forecast") +
    theme_classic() +
    theme(axis.text.x = element_text(angle=45, hjust=1)) +
    lshtm_theme() +
    facet_wrap(timepoint~., labeller=as_labeller(timepoint_labels), nrow=1)
  
  
  return(plot_grid(timeseries_dis, rank_plot, ncol=1))
}
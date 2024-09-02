plotrankrt_tp <- function(res_rt_samples, 
                          res_id, 
                          rt_dis,
                          forecast_freq){
  
  dis_timepoints <- rt_dis$date[c(1:(nrow(rt_dis) %/% (forecast_freq*7)))*forecast_freq*7]
  
  # Take forecasts only
  
  res_rt_samples_end <- res_rt_samples |>
    filter(type=="forecast") 
  
  # Take 14th day only 
  res_rt_samples_end <- res_rt_samples_end |>
    group_by(variable, parameter, sample, type, model, result_list, gt) |>
    filter(date==max(date))
  
  res_rt_samples_end <- res_rt_samples_end |> 
    # add info
    left_join(res_id, by=c("result_list", "gt"))
  
  res_rt_samples_end <- res_rt_samples_end |>
    left_join(rt_dis, by="date")
  
  res_rt_samples_end <- as_forecast_sample(
    data=res_rt_samples_end,
    forecast_unit=c("date", "type", "timepoint", "gen_time", "inc_period", "model"),
    observed='R',
    predicted='value',
    model='model',
    sample_id='sample'
  )
  
  #check_forecasts(res_samples)
  
  # Log transform observations and predicted values
  
  res_rt_samples_end <- transform_forecasts(res_rt_samples_end, fun = log_shift, offset=1, label="log") |> 
    filter(scale=='log')
  
  #res_samples |>
  #  check_forecasts()
  
  scores <- res_rt_samples_end |>
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
  
  ## Best and worst performing forecasts plot over time
  worstbest <- rankings |> 
    filter(rank %in% c(1, max(rank))) |>
    select(timepoint, gen_time, inc_period, rank)
  
  worstbest <- worstbest |>
    mutate(performance=ifelse(rank==1, "Best-performing", "Worst-performing"))
  
  res_rt_performance <- res_rt_samples |>
    left_join(res_id, by=c("result_list", "gt")) |>
    select(time, date, sample, value, type, timepoint, gen_time, inc_period, model) |>
    left_join(worstbest, by=c("timepoint", "gen_time", "inc_period")) |>
    filter(!is.na(performance))
  
  ## Add true values
  res_rt_performance <- res_rt_performance |>
    left_join(rt_dis, by="date")
  
  # Adding median and quantiles
  res_rt_performance <- as_forecast_sample(
    data=res_rt_performance,
    forecast_unit=c("date", "type", "timepoint", "gen_time", "inc_period", "rank", "model", "performance"),
    observed='R',
    predicted='value',
    model='model',
    sample_id='sample'
  )
  
  res_rt_performance <- sample_to_quantile(res_rt_performance)
  
  res_rt_performance <- res_rt_performance |>
    pivot_wider(names_from="quantile_level",
                values_from="predicted")
  
  res_rt_performance <- res_rt_performance  |>
    rename(lower90="0.05",
           upper90="0.95",
           lower50="0.25",
           upper50="0.75",
           median="0.5")
  
  
  # Plot 1: Timeseries Dis (no legend)
  timeseries_dis <- ggplot() + 
    geom_line(rt_dis, mapping=aes(x=date, y=R)) + 
    geom_point(rt_timepoints, mapping=aes(x=date,y=R), color="red") +
    xlab("Date") +
    ylab("Rt") +
    scale_x_continuous(breaks=rt_dis$date[grepl("-01$", rt_dis$date)]) +
    lshtm_theme() +
    theme(legend.position = "none")  # No legend needed
  
  # Plot 2: Timeseries Performance (remove legend from plot)
  timeseries_performance <- ggplot(res_rt_performance) + 
    geom_line(mapping=aes(x=date, y=median, colour=interaction(performance, type))) +
    geom_ribbon(mapping=aes(x=date, ymin=lower50, ymax=upper50, fill=interaction(performance, type)), alpha=0.5) +
    geom_ribbon(mapping=aes(x=date, ymin=lower90, ymax=upper90, fill=interaction(performance, type)), alpha=0.2) +
    geom_line(mapping=aes(x=date, y=observed), colour="black", size=0.5, linetype="dashed", show.legend=FALSE) +
    facet_wrap(~timepoint, nrow=1, scale="free") +
    lshtm_theme() +
    scale_colour_manual(
      name = "Performance",
      values = c(
        "Best-performing.estimate" = "lightblue",
        "Best-performing.estimate based on partial data" = "deepskyblue",
        "Best-performing.forecast" = "darkblue",
        "Worst-performing.estimate" = "lightcoral",
        "Worst-performing.estimate based on partial data" = "indianred",
        "Worst-performing.forecast" = "darkred"
      ),
      breaks = c(
        "Best-performing.estimate",
        "Best-performing.estimate based on partial data",
        "Best-performing.forecast",
        "Worst-performing.estimate",
        "Worst-performing.estimate based on partial data",
        "Worst-performing.forecast"
      )
    ) +
    scale_fill_manual(
      name = "Performance",
      values = c(
        "Best-performing.estimate" = "lightblue",
        "Best-performing.estimate based on partial data" = "deepskyblue",
        "Best-performing.forecast" = "darkblue",
        "Worst-performing.estimate" = "lightcoral",
        "Worst-performing.estimate based on partial data" = "indianred",
        "Worst-performing.forecast" = "darkred"
      ),
      breaks = c(
        "Best-performing.estimate",
        "Best-performing.estimate based on partial data",
        "Best-performing.forecast",
        "Worst-performing.estimate",
        "Worst-performing.estimate based on partial data",
        "Worst-performing.forecast"
      )
    ) +
    theme(legend.position = "none")  # Remove legend from timeseries_performance
  
  # Plot 3: Rank Plot (remove legend from plot)
  rank_plot <- ggplot(rankings, aes(x=gen_time, y=inc_period)) +
    geom_tile(aes(fill=rank)) +
    xlab("Generation time") +
    ylab("Incubation period") +
    scale_fill_gradientn(colors = terrain.colors(6), name="CRPS for Rt estimate") +
    theme_classic() +
    theme(axis.text.x = element_text(angle=45, hjust=1),
          legend.position = "none") +  # Remove legend from rank_plot
    lshtm_theme() +
    facet_wrap(timepoint~., labeller=as_labeller(timepoint_labels), nrow=1)
  
  # Standard legend text size
  legend_text_size <- 10
  
  # Extract legends from `timeseries_performance` and `rank_plot` with consistent text size
  legend_performance <- get_legend(
    ggplot(res_rt_performance) + 
      geom_line(mapping=aes(x=date, y=median, colour=interaction(performance, type))) +
      geom_ribbon(mapping=aes(x=date, ymin=lower50, ymax=upper50, fill=interaction(performance, type)), alpha=0.5) +
      geom_ribbon(mapping=aes(x=date, ymin=lower90, ymax=upper90, fill=interaction(performance, type)), alpha=0.2) +
      scale_colour_manual(
        name = "Performance",
        values = c(
          "Best-performing.estimate" = "lightblue",
          "Best-performing.estimate based on partial data" = "deepskyblue",
          "Best-performing.forecast" = "darkblue",
          "Worst-performing.estimate" = "lightcoral",
          "Worst-performing.estimate based on partial data" = "indianred",
          "Worst-performing.forecast" = "darkred"
        ),
        breaks = c(
          "Best-performing.estimate",
          "Best-performing.estimate based on partial data",
          "Best-performing.forecast",
          "Worst-performing.estimate",
          "Worst-performing.estimate based on partial data",
          "Worst-performing.forecast"
        )
      ) +
      scale_fill_manual(
        name = "Performance",
        values = c(
          "Best-performing.estimate" = "lightblue",
          "Best-performing.estimate based on partial data" = "deepskyblue",
          "Best-performing.forecast" = "darkblue",
          "Worst-performing.estimate" = "lightcoral",
          "Worst-performing.estimate based on partial data" = "indianred",
          "Worst-performing.forecast" = "darkred"
        ),
        breaks = c(
          "Best-performing.estimate",
          "Best-performing.estimate based on partial data",
          "Best-performing.forecast",
          "Worst-performing.estimate",
          "Worst-performing.estimate based on partial data",
          "Worst-performing.forecast"
        )
      ) +
      theme_minimal() +
      theme(legend.position = "right", 
            legend.text = element_text(size = legend_text_size))
#            legend.background = element_rect(fill = "white"))
  )
  
  legend_rank <- get_legend(
    ggplot(rankings, aes(x=gen_time, y=inc_period)) +
      geom_tile(aes(fill=rank)) +
      scale_fill_gradientn(colors = terrain.colors(6), name="CRPS for Rt estimate") +
      theme_classic() +
      theme(axis.text.x = element_text(angle=45, hjust=0),
            legend.position = "right",
            legend.text = element_text(size = legend_text_size),
            legend.background = element_rect(fill = "white"))  # Adjust text size
  )
  
  # Spacer to align legends correctly
  spacer <- ggplot() + theme_void()  # Create an empty plot to act as a spacer
  
  # Combine plots and spacer without legends (in a single column)
  combined_plots <- plot_grid(
    timeseries_dis, timeseries_performance, rank_plot,
    ncol = 1, # Arrange plots in a single column
    align = "v",  # Align plots vertically
    axis = "tblr"  # Align all axes  
  )
  
  padded_legend_rank <- plot_grid(
    legend_rank,
    spacer,
    ncol = 2,
    rel_widths = c(1, 0.9)  # Adjust height to match the size of the other legend
  )
  
  # Combine plots and legends
  final_plot <- plot_grid(
    combined_plots,  # Combined plots
    plot_grid(
      spacer, legend_performance, padded_legend_rank,  # Spacer included in the legends section
      ncol = 1,  # Place legends and spacer in a single column
      rel_heights = c(1, 1, 1),# Adjust spacer height, making it smaller to reduce gaps
      align = "l"
    ),
    ncol = 2,  # Combine plots and legends in two columns
    rel_widths = c(4, 1.7)  # Adjust width ratio (e.g., 4 times wider for plots)
  )
  
  return(final_plot)}

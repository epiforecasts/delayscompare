plotrankcase_tp <- function(res_samples,
                            res_id,
                            sim_data,
                            forecast_freq=4) {
  
  # Add info to res_samples
  res_samples <- res_samples |>
    left_join(res_id, by=c("result_list", "gt"))
  
  res_samples_end <- res_samples |>
    group_by(timepoint) |>
    filter(date == max(date))
  
  # Add simulated data to res_samples_end for rankings
  res_samples_end <- sim_data |>
    filter(variable=="reported_cases") |>
    rename(true_value=value) |>
    select(-variable) |>
    right_join(res_samples_end |> filter(type=="forecast"), by="date")
  
  # Prepare res_samples_end for scoring and ranking
  res_samples_end <- res_samples_end |>
    select(date, true_value, prediction, sample, model, gen_time, inc_period, timepoint, type) 
  
  res_samples_end <- as_forecast_sample(
    data=res_samples_end,
    forecast_unit=c("date", "type", "gen_time", "inc_period", "model", "timepoint"),
    observed="true_value",
    predicted="prediction",
    model='model',
    sample_id='sample'
  )
  
  res_samples_end <- transform_forecasts(res_samples_end, fun = log_shift, offset=1, label="log")
  
  scores <- res_samples_end |>
    filter(type=="forecast", scale=="log") |>
    set_forecast_unit(c("date", "model", "gen_time", "inc_period", "timepoint", "type")) |>
    score()
  
  rankings <- scores |>
    group_by(timepoint) |>
    mutate(rank = rank(crps)) |>
    ungroup()
  
  rankings <- rankings |>
    mutate(
      inc_period = factor(
        inc_period, levels=c("no delay", "very low", "low", "correct", "high", "very high", "weight priors true", "weight priors false")
      ),
      gen_time = factor(
        gen_time, levels=c("no delay", "very low", "low", "correct", "high", "very high", "weight priors true", "weight priors false")
      ))
  
  ## Best and worst performing forecasts plot over time
  worstbest <- rankings |> 
    filter(rank %in% c(1, max(rank))) |>
    select(timepoint, gen_time, inc_period, rank)
  
  worstbest <- worstbest |>
    mutate(performance=ifelse(rank==1, "Best-performing", "Worst-performing"))
  
  res_performance <- res_samples |>
    select(time, date, sample, prediction, type, timepoint, gen_time, inc_period, model) |>
    left_join(worstbest, by=c("timepoint", "gen_time", "inc_period")) |>
    filter(!is.na(performance))
  
  sim_data_cases <- sim_data |>
    filter(variable=="reported_cases") |>
    select(date, value)
  
  ## Add observed data
  res_performance <- res_performance |>
    left_join(sim_data_cases, by="date")
  
  ## Add median and quantiles
  
  res_performance <- as_forecast_sample(
    data=res_performance,
    forecast_unit=c("date", "type", "timepoint", "gen_time", "inc_period", "rank", "model", "performance"),
    observed='value',
    predicted='prediction',
    model='model',
    sample_id='sample'
  )
  
  res_performance <- sample_to_quantile(res_performance)
  
  res_performance <- res_performance |>
    pivot_wider(names_from="quantile_level",
                values_from="predicted") |>
    rename(lower90="0.05",
           upper90="0.95",
           lower50="0.25",
           upper50="0.75",
           median="0.5")

  dis_timepoints <- sim_data_cases$date[c(1:(nrow(sim_data_cases) %/% (forecast_freq*7)))*forecast_freq*7]
  
  sim_data_timepoints <- sim_data_cases |> filter(date %in% dis_timepoints)
  
  timepoint_labels <- setNames(as.character(dis_timepoints), 
                               c(1:length(dis_timepoints)))
  
  # Plot 1: Timeseries of reported cases (no legend)
  timeseries_dis <- ggplot() + 
    geom_line(sim_data_cases, mapping=aes(x=date, y=value)) + 
    geom_point(sim_data_timepoints, mapping=aes(x=date,y=value), color="red") +
    xlab("Date") +
    ylab("Reported cases") +
    scale_x_continuous(breaks=sim_data_cases$date[grepl("-01$",sim_data_cases$date)]) +
    lshtm_theme() +
    theme(legend.position = "none")  # No legend needed
  
  # Plot 2: Forecasted cases + observed cases (best and worst performing)
  timeseries_forecast <- ggplot(res_performance) + 
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
  
  
  # Plot 3: Rank plot (with legend)
  rank_plot <- ggplot(rankings, aes(x=gen_time, y=inc_period)) +
    geom_tile(aes(fill=rank)) +
    xlab("Generation time") +
    ylab("Incubation period") +
    scale_fill_gradientn(colours = terrain.colors(50), name="Ranking of two-week forecast") +
    theme_classic() +
    theme(axis.text.x = element_text(angle=45, hjust=1),
          legend.position = "none") +  # Remove legend from rank_plot
    lshtm_theme() +
    facet_wrap(timepoint~., labeller=as_labeller(timepoint_labels), nrow=1)
  
  # Standard legend text size
  legend_text_size <- 10
  
  # Extract legends
  legend_performance <- get_legend(
    ggplot(res_performance) + 
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
      scale_fill_gradientn(colors = terrain.colors(50), name="Ranking of two-week forecast") +
      theme_classic() +
      theme(axis.text.x = element_text(angle=45, hjust=0),
            legend.position = "right",
            legend.text = element_text(size = legend_text_size),
            legend.background = element_rect(fill = "white"))  
  )
  
  # Spacer to align legends correctly
  spacer <- ggplot() + theme_void()  # Create an empty plot to act as a spacer
  
  # Combine plots and spacer without legends (in a single column)
  combined_plots <- plot_grid(
    timeseries_dis, timeseries_forecast, rank_plot,
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
  
  return(final_plot)
}

plotrankcase <- function(res_samples,
                            res_id,
                            sim_data,
                            forecast_freq=4) {
  
  # Add info to res_samples
  res_samples <- res_samples |>
    left_join(res_id, by=c("result_list", "gt"))
  
  res_samples_end <- res_samples |>
    group_by(timepoint) |>
    filter(date == max(date))
  
  # Add simulated data to res_samples_end for rankings
  res_samples_end <- sim_data |>
    filter(variable=="reported_cases") |>
    rename(true_value=value) |>
    select(-variable) |>
    right_join(res_samples_end |> filter(type=="forecast"), by="date")
  
  # Prepare res_samples_end for scoring and ranking
  res_samples_end <- res_samples_end |>
    select(date, true_value, prediction, sample, model, gen_time, inc_period, timepoint, type) 
  
  res_samples_end <- as_forecast_sample(
    data=res_samples_end,
    forecast_unit=c("date", "type", "gen_time", "inc_period", "model", "timepoint"),
    observed="true_value",
    predicted="prediction",
    model='model',
    sample_id='sample'
  )
  
  res_samples_end <- transform_forecasts(res_samples_end, fun = log_shift, offset=1, label="log")
  
  scores <- res_samples_end |>
    filter(type=="forecast", scale=="log") |>
    set_forecast_unit(c("date", "model", "gen_time", "inc_period", "type", "timepoint")) |>
    score() |>
    # Across timepoints
    summarise_scores(by=c("gen_time", "inc_period"))
  
  rankings <- scores |>
    mutate(rank = rank(crps))
  
  rankings <- rankings |>
    mutate(
      inc_period = factor(
        inc_period, levels=c("no delay", "very low", "low", "correct", "high", "very high")
      ),
      gen_time = factor(
        gen_time, levels=c("no delay", "very low", "low", "correct", "high", "very high")
      ))
  
  ## Best and worst performing forecasts plot over time
  worstbest <- rankings |> 
    filter(rank %in% c(1, max(rank))) |>
    select(timepoint, gen_time, inc_period, rank)
  
  worstbest <- worstbest |>
    mutate(performance=ifelse(rank==1, "Best-performing", "Worst-performing"))
  
  res_performance <- res_samples |>
    select(time, date, sample, prediction, type, timepoint, gen_time, inc_period, model) |>
    left_join(worstbest, by=c("timepoint", "gen_time", "inc_period")) |>
    filter(!is.na(performance))
  
  sim_data_cases <- sim_data |>
    filter(variable=="reported_cases") |>
    select(date, value)
  
  ## Add observed data
  res_performance <- res_performance |>
    left_join(sim_data_cases, by="date")
  
  ## Add median and quantiles
  
  res_performance <- as_forecast_sample(
    data=res_performance,
    forecast_unit=c("date", "type", "timepoint", "gen_time", "inc_period", "rank", "model", "performance"),
    observed='value',
    predicted='prediction',
    model='model',
    sample_id='sample'
  )
  
  res_performance <- sample_to_quantile(res_performance)
  
  res_performance <- res_performance |>
    pivot_wider(names_from="quantile_level",
                values_from="predicted") |>
    rename(lower90="0.05",
           upper90="0.95",
           lower50="0.25",
           upper50="0.75",
           median="0.5")
  
  dis_timepoints <- sim_data_cases$date[c(1:(nrow(sim_data_cases) %/% (forecast_freq*7)))*forecast_freq*7]
  
  sim_data_timepoints <- sim_data_cases |> filter(date %in% dis_timepoints)
  
  timepoint_labels <- setNames(as.character(dis_timepoints), 
                               c(1:length(dis_timepoints)))
  
  # Plot 1: Timeseries of reported cases (no legend)
  timeseries_dis <- ggplot() + 
    geom_line(sim_data_cases, mapping=aes(x=date, y=value)) + 
    geom_point(sim_data_timepoints, mapping=aes(x=date,y=value), color="red") +
    xlab("Date") +
    ylab("Reported cases") +
    scale_x_continuous(breaks=sim_data_cases$date[grepl("-01$",sim_data_cases$date)]) +
    lshtm_theme() +
    theme(legend.position = "none")  # No legend needed
  
  # Plot 2: Forecasted cases + observed cases (best and worst performing)
  timeseries_forecast <- ggplot(res_performance) + 
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
  
  
  # Plot 3: Rank plot (with legend)
  rank_plot <- ggplot(rankings, aes(x=gen_time, y=inc_period)) +
    geom_tile(aes(fill=rank)) +
    xlab("Generation time") +
    ylab("Incubation period") +
    scale_fill_gradientn(colours = terrain.colors(50), name="Ranking of two-week forecast") +
    theme_classic() +
    theme(axis.text.x = element_text(angle=45, hjust=1),
          legend.position = "none") +  # Remove legend from rank_plot
    lshtm_theme() 
  
  # Standard legend text size
  legend_text_size <- 10
  
  # Extract legends
  legend_performance <- get_legend(
    ggplot(res_performance) + 
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
      scale_fill_gradientn(colors = terrain.colors(50), name="Ranking of two-week forecast") +
      theme_classic() +
      theme(axis.text.x = element_text(angle=45, hjust=0),
            legend.position = "right",
            legend.text = element_text(size = legend_text_size),
            legend.background = element_rect(fill = "white"))  
  )
  
  # Spacer to align legends correctly
  spacer <- ggplot() + theme_void()  # Create an empty plot to act as a spacer
  
  # Combine plots and spacer without legends (in a single column)
  combined_plots <- plot_grid(
    timeseries_dis, timeseries_forecast, rank_plot,
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
  
  return(final_plot)
}

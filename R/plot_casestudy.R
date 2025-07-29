plot_cs_triple <- function(res_samples,
                            sim_data,
                            forecast_freq=4,
                           data_freq="day") {
  
  # Going to trim res_samples to only have up to 8 timepoints - otherwise too long/difficult to interpret.
  res_samples <- res_samples |> filter(timepoint<=8)
  
  if(data_freq=="week"){
  
  # Need to turn samples into weekly cumulative counts that match sim_data dates
  # Create a new week column in res_samples ending on the same date as sim_data
  res_samples <- res_samples |>
    mutate(
      week = sim_data$date[findInterval(date, sim_data$date, left.open = TRUE) + 1]
    )
  
  # Use this to get the weekly cumulative prediction
  res_samples <- res_samples |>
    group_by(week, gen_time, inc_period, timepoint, type, model, sample) |>
    summarise(
      prediction = sum(prediction),
      date = max(date)
    ) |>
    ungroup() |>
    select(-week)}
  

  res_samples_end <- res_samples |>
    filter(type=="forecast") |>
    group_by(timepoint, gen_time, inc_period) |>
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
    forecast_unit=c("date", "type", "gen_time", "inc_period", "timepoint"),
    observed="true_value",
    predicted="prediction",
    sample_id='sample'
  )
  
  res_samples_end <- transform_forecasts(res_samples_end, fun = log_shift, offset=1, label="log")
  
  # Scoring
  scores <- res_samples_end |>
    filter(type=="forecast", scale=="log") |>
    score()
  
  rankings <- scores |>
    group_by(timepoint) |>
    mutate(rank = rank(crps)) |>
    ungroup()
  
  rankings <- rankings |>
    mutate(
      inc_period = factor(
        inc_period, levels=c("no delay", "very low", "low", "correct", "high", "very high", "weight_prior_true", "weight_prior_false")
      ),
      gen_time = factor(
        gen_time, levels=c("no delay", "very low", "low", "correct", "high", "very high", "weight_prior_true", "weight_prior_false")
      ))
  
  ## Best and worst performing forecasts plot over time
  worstbest <- rankings |> 
    filter(rank %in% c(1, max(rank))) |>
    select(timepoint, gen_time, inc_period, rank)
  
  worstbest <- worstbest |>
    mutate(performance=ifelse(rank==1, "Best-performing", "Worst-performing"))
  
  # Forecasts only
  res_samples <- res_samples |> filter(type=="forecast")

  # Get quantiles
  res_samples <- res_samples |>
    group_by(timepoint, date, gen_time, inc_period, type) |>
    summarise(
      q0.025 = quantile(prediction, 0.025),
      q0.25 = quantile(prediction, 0.25),
      q0.5 = quantile(prediction, 0.5),
      q0.75 = quantile(prediction, 0.75),
      q0.975 = quantile(prediction, 0.975),
    )
  
  res_performance <- res_samples |>
    left_join(worstbest, by=c("timepoint", "gen_time", "inc_period")) |>
    filter(!is.na(performance))
 
 ## Create a full sequence of dates
  if(data_freq == "week") {
 all_dates <- expand_grid(
   date = seq(min(res_performance$date), max(res_performance$date), by = "1 week"),
   performance = unique(res_performance$performance)
 )
  } else if(data_freq == "day") {
    all_dates <- expand_grid(
      date = seq(min(res_performance$date), max(res_performance$date), by = "1 day"),
      performance = unique(res_performance$performance)
    )
  } else {
    stop("data_freq must be either 'day' or 'week'")
  }
  
  # Merge with existing data, filling gaps with NA
  res_performance_full <- merge(all_dates, res_performance, by = c("date", "performance"), all.x = TRUE)
  
  sim_data_cases <- sim_data |> filter(variable=="reported_cases")
  
  ## Add observed data
  res_performance_full <- res_performance_full |>
    left_join(sim_data_cases, by="date")

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
  timeseries_forecast <- ggplot() + 
    geom_line(res_performance_full, mapping=aes(x=date, y=q0.5, colour=performance)) +
    geom_ribbon(res_performance_full, mapping=aes(x=date, ymin=q0.25, ymax=q0.75, fill=performance), alpha=0.5) +
    # geom_ribbon(res_performance, mapping=aes(x=date, ymin=q0.025, ymax=q0.975, fill=interaction(performance, type)), alpha=0.2) +
    geom_line(sim_data_cases, mapping=aes(x=date, y=value), colour="black", size=0.5, linetype="dashed", show.legend=FALSE) +
    xlab("Date") +
    ylab("Reported cases") +
    lshtm_theme() +
    scale_colour_manual(
      name = "Performance",
      values = c(
        "Best-performing" = "darkblue",
        "Worst-performing" = "darkred"
      ),
      breaks = c(
        "Best-performing",
        "Worst-performing"
      )
    ) +
    scale_fill_manual(
      name = "Performance",
      values = c(
        "Best-performing" = "darkblue",
        "Worst-performing" = "darkred"
      ),
      breaks = c(
        "Best-performing",
        "Worst-performing"
      )
    ) +
    theme(axis.text.x = element_text(angle=45, hjust=1), legend.position = "none")  # Remove legend from timeseries_performance
  
  
  # Plot 3: CRPS barchart
  scores_cases_long <- scores |>
    select(date, type, gen_time, inc_period, timepoint, scale, crps, overprediction, underprediction, dispersion) |>
    pivot_longer(cols=c("crps", "overprediction", "underprediction", "dispersion"), names_to="measure", values_to="value")
  
  # Make sure gen_time and inc_period are factors
  # Need to make gen_time and inc_period are factors so that ordering is correct:
  scores_cases_long$gen_time <- factor(scores_cases_long$gen_time, levels=c("no delay",
                                                                            "very low",
                                                                            "low",
                                                                            "correct",
                                                                            "high",
                                                                            "very high",
                                                                            "weight_prior_true",
                                                                            "weight_prior_false"))
  
  scores_cases_long$inc_period <- factor(scores_cases_long$inc_period, levels=c("no delay",
                                                                                "very low",
                                                                                "low",
                                                                                "correct",
                                                                                "high",
                                                                                "very high",
                                                                                "weight_prior_true",
                                                                                "weight_prior_false"))
  
  scores_cases_gen_time <- scores_cases_long |>
    group_by(gen_time, measure, timepoint) |>
    summarise(value=mean(value))
  
  scores_cases_inc_period <- scores_cases_long |>
    group_by(inc_period, measure, timepoint) |>
    summarise(value=mean(value))
  
  # And the mean scores
  mean_scores_cases_gen_time <- scores_cases_long |>
    group_by(gen_time, measure) |>
    summarise(value=mean(value))
  
  mean_scores_cases_inc_period <- scores_cases_long |>
    group_by(inc_period, measure) |>
    summarise(value=mean(value))
  
  barchart_gen_time <- ggplot(scores_cases_gen_time |> filter(measure!="crps")) + 
    geom_bar(aes(x=gen_time, y=value, fill=measure), stat="identity") +
    facet_wrap(~timepoint, nrow=1) +
    xlab("Generation time") +
    ylab("CRPS") +
    lshtm_theme() +
    theme(axis.text.x=element_text(angle=45, hjust=1),
          legend.position = "none") # Remove legend from barchart_gen_time 
  
  barchart_inc_period <- ggplot(scores_cases_inc_period |> filter(measure!="crps")) +
    geom_bar(aes(x=inc_period, y=value, fill=measure), stat="identity") +
    facet_wrap(~timepoint, nrow=1) +
    xlab("Incubation period") +
    ylab("CRPS") +
    lshtm_theme()  +
    theme(axis.text.x=element_text(angle=45, hjust=1),
          legend.position = "none")
  
  barchart_mean_gen_time <- ggplot(mean_scores_cases_gen_time |> filter(measure!="crps")) + 
    geom_bar(aes(x=gen_time, y=value, fill=measure), stat="identity") +
    xlab("Generation time") +
    ylab("CRPS") +
    lshtm_theme() +
    theme(axis.text.x=element_text(angle=45, hjust=1),
          legend.position = "none",
          #axis.title.x=element_blank(),
          axis.title.y=element_blank()) +
    xlab(" ")
  
  barchart_mean_inc_period <- ggplot(mean_scores_cases_inc_period |> filter(measure!="crps")) +
    geom_bar(aes(x=inc_period, y=value, fill=measure), stat="identity") +
    xlab("Incubation period") +
    ylab("CRPS") +
    lshtm_theme()  +
    theme(axis.text.x=element_text(angle=45, hjust=1),
          legend.position = "none",
          #axis.title.x=element_blank(),
          axis.title.y=element_blank()) +
    xlab(" ")
  
  # Standard legend text size
  legend_text_size <- 10
  
  # Extract legends from `timeseries_performance` and `rank_plot` with consistent text size
  legend_performance <- get_legend(
    ggplot(res_performance_full) + 
      geom_line(mapping=aes(x=date, y=q0.5, colour=performance)) +
      geom_ribbon(mapping=aes(x=date, ymin=q0.25, ymax=q0.75, fill=performance), alpha=0.5) +
      geom_ribbon(mapping=aes(x=date, ymin=q0.025, ymax=q0.975, fill=performance), alpha=0.2) +
      scale_colour_manual(
        name = "Performance",
        values = c(
          # "Best-performing.estimate" = "lightblue",
          #  "Best-performing.estimate based on partial data" = "deepskyblue",
          "Best-performing" = "darkblue",
          # "Worst-performing.estimate" = "lightcoral",
          #  "Worst-performing.estimate based on partial data" = "indianred",
          "Worst-performing" = "darkred"
        ),
        breaks = c(
          # "Best-performing.estimate",
          #  "Best-performing.estimate based on partial data",
          "Best-performing",
          #"Worst-performing.estimate",
          #"Worst-performing.estimate based on partial data",
          "Worst-performing"
        )
      ) +
      scale_fill_manual(
        name = "Performance",
        values = c(
          #"Best-performing" = "lightblue",
          #"Best-performing.estimate based on partial data" = "deepskyblue",
          "Best-performing" = "darkblue",
          # "Worst-performing.estimate" = "lightcoral",
          #  "Worst-performing.estimate based on partial data" = "indianred",
          "Worst-performing" = "darkred"
        ),
        breaks = c(
          #"Best-performing.estimate",
          #"Best-performing.estimate based on partial data",
          "Best-performing",
          #"Worst-performing.estimate",
          #"Worst-performing.estimate based on partial data",
          "Worst-performing"
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
      scale_fill_gradientn(colors = terrain.colors(6), name="CRPS for case forecast") +
      theme_classic() +
      theme(axis.text.x = element_text(angle=45, hjust=0),
            legend.position = "right",
            legend.text = element_text(size = legend_text_size),
            legend.background = element_rect(fill = "white"))  # Adjust text size
  )
  
  legend_bar <- get_legend(
    ggplot(scores_cases_gen_time |> filter(measure!="crps")) + 
      geom_bar(aes(x=gen_time, y=value, fill=measure), stat="identity") +
      facet_wrap(~timepoint, nrow=1) +
      xlab("Generation time") +
      ylab("CRPS") +
      lshtm_theme() +
      theme(axis.text.x=element_text(angle=45, hjust=1),
            legend.position = "right",
            legend.text = element_text(size = legend_text_size),
            legend.background = element_rect(fill = "white")) 
  )
  
  # Spacer to align legends correctly
  spacer <- ggplot() + theme_void()  # Create an empty plot to act as a spacer
  
  # Combine plots and spacer without legends (in a single column)
    combined_plots_ts <- plot_grid(
    timeseries_forecast, barchart_gen_time, barchart_inc_period,
    ncol = 1, # Arrange plots in a single column
    align = "v",  # Align plots vertically
    axis = "tblr"  # Align all axes  
  )
  
  # Add mean plots w/ spacer at the top so aligns with timeseries plots
  combined_plots_overall <- plot_grid(
    spacer, barchart_mean_gen_time, barchart_mean_inc_period,
    ncol = 1, # Arrange plots in a single column
    align = "v",  # Align plots vertically
    axis = "tblr"  # Align all axes  
  )
  
  combined_plots <- plot_grid(combined_plots_ts, combined_plots_overall, ncol = 2, rel_widths=c(6,1.25),
                              align = "v", axis = "tblr")
  
  padded_legend_bar <- plot_grid(
    legend_bar,
    spacer,
    ncol = 2,
    rel_widths = c(1, 0.9)  # Adjust height to match the size of the other legend
  )
  
  # Combine plots and legends
  final_plot <-plot_grid(
    combined_plots,  # Combined plots
    plot_grid(
      legend_performance, legend_bar, spacer,  # Spacer included in the legends section
      ncol = 1,  # Place legends and spacer in a single column
      rel_heights = c(1, 1, 1),# Adjust spacer height, making it smaller to reduce gaps
      align = "l"
    ),
    ncol = 2,  # Combine plots and legends in two columns
    rel_widths = c(8, 1)  # Adjust width ratio (e.g., 4 times wider for plots)
  )
  
  return(final_plot)
}

plot_cs_single <- function(res_samples,
                           sim_data,
                           forecast_freq=4,
                           data_freq="day") {
  
  # Going to trim res_samples to only have up to 8 timepoints - otherwise too long/difficult to interpret.
  res_samples <- res_samples |> filter(timepoint<=8)
  
  if(data_freq=="week"){
    
    # Need to turn samples into weekly cumulative counts that match sim_data dates
    # Create a new week column in res_samples ending on the same date as sim_data
    res_samples <- res_samples |>
      mutate(
        week = sim_data$date[findInterval(date, sim_data$date, left.open = TRUE) + 1]
      )
    
    # Use this to get the weekly cumulative prediction
    res_samples <- res_samples |>
      group_by(week, gen_time, inc_period, timepoint, type, model, sample) |>
      summarise(
        prediction = sum(prediction),
        date = max(date)
      ) |>
      ungroup() |>
      select(-week)}
  
  
  res_samples_end <- res_samples |>
    filter(type=="forecast") |>
    group_by(timepoint, gen_time, inc_period) |>
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
    forecast_unit=c("date", "type", "gen_time", "inc_period", "timepoint"),
    observed="true_value",
    predicted="prediction",
    sample_id='sample'
  )
  
  res_samples_end <- transform_forecasts(res_samples_end, fun = log_shift, offset=1, label="log")
  
  # Scoring
  scores <- res_samples_end |>
    filter(type=="forecast", scale=="log") |>
    score()
  
  rankings <- scores |>
    group_by(timepoint) |>
    mutate(rank = rank(crps)) |>
    ungroup()
  
  rankings <- rankings |>
    mutate(
      inc_period = factor(
        inc_period, levels=c("no delay", "very low", "low", "correct", "high", "very high", "weight_prior_true", "weight_prior_false")
      ),
      gen_time = factor(
        gen_time, levels=c("no delay", "very low", "low", "correct", "high", "very high", "weight_prior_true", "weight_prior_false")
      ))
  
  ## Best and worst performing forecasts plot over time
  worstbest <- rankings |> 
    filter(rank %in% c(1, max(rank))) |>
    select(timepoint, gen_time, inc_period, rank)
  
  worstbest <- worstbest |>
    mutate(performance=ifelse(rank==1, "Best-performing", "Worst-performing"))
  
  # Forecasts only
  res_samples <- res_samples |> filter(type=="forecast")
  
  # Get quantiles
  res_samples <- res_samples |>
    group_by(timepoint, date, gen_time, inc_period, type) |>
    summarise(
      q0.025 = quantile(prediction, 0.025),
      q0.25 = quantile(prediction, 0.25),
      q0.5 = quantile(prediction, 0.5),
      q0.75 = quantile(prediction, 0.75),
      q0.975 = quantile(prediction, 0.975),
    )
  
  res_performance <- res_samples |>
    left_join(worstbest, by=c("timepoint", "gen_time", "inc_period")) |>
    filter(!is.na(performance))
  
  ## Create a full sequence of dates
  if(data_freq == "week") {
    all_dates <- expand_grid(
      date = seq(min(res_performance$date), max(res_performance$date), by = "1 week"),
      performance = unique(res_performance$performance)
    )
  } else if(data_freq == "day") {
    all_dates <- expand_grid(
      date = seq(min(res_performance$date), max(res_performance$date), by = "1 day"),
      performance = unique(res_performance$performance)
    )
  } else {
    stop("data_freq must be either 'day' or 'week'")
  }
  
  # Merge with existing data, filling gaps with NA
  res_performance_full <- merge(all_dates, res_performance, by = c("date", "performance"), all.x = TRUE)
  
  sim_data_cases <- sim_data |> filter(variable=="reported_cases") 
  
  # Remove rest of timeseries after last forecast
  max_date <- max(unique(res_samples$date))
  sim_data_cases <- sim_data_cases |> filter(date <= max_date)
  
  ## Add observed data
  res_performance_full <- res_performance_full |>
    left_join(sim_data_cases, by="date")
  
  dis_timepoints <- sim_data_cases$date[c(1:(nrow(sim_data_cases) %/% (forecast_freq*7)))*forecast_freq*7]
  
  sim_data_timepoints <- sim_data_cases |> filter(date %in% dis_timepoints)
  
  timepoint_labels <- setNames(as.character(dis_timepoints), 
                               c(1:length(dis_timepoints)))
  
  # Plot 1: Forecasted cases + observed cases (best and worst performing)
  timeseries_forecast <- ggplot() + 
    geom_line(res_performance_full, mapping=aes(x=date, y=q0.5, colour=performance)) +
    geom_ribbon(res_performance_full, mapping=aes(x=date, ymin=q0.25, ymax=q0.75, fill=performance), alpha=0.5) +
    # geom_ribbon(res_performance, mapping=aes(x=date, ymin=q0.025, ymax=q0.975, fill=interaction(performance, type)), alpha=0.2) +
    geom_line(sim_data_cases, mapping=aes(x=date, y=value), colour="black", size=0.5, linetype="dashed", show.legend=FALSE) +
    xlab("Date") +
    ylab("Reported cases") +
    lshtm_theme() +
    scale_colour_manual(
      name = "Performance",
      values = c(
        "Best-performing" = "darkblue",
        "Worst-performing" = "darkred"
      ),
      breaks = c(
        "Best-performing",
        "Worst-performing"
      )
    ) +
    scale_fill_manual(
      name = "Performance",
      values = c(
        "Best-performing" = "darkblue",
        "Worst-performing" = "darkred"
      ),
      breaks = c(
        "Best-performing",
        "Worst-performing"
      )
    ) +
    theme(axis.text.x = element_text(angle=45, hjust=1), legend.position = "none")  # Remove legend from timeseries_performance
  
  
  # Plot 2: CRPS barchart
  scores_cases_long <- scores |>
    select(date, type, gen_time, inc_period, timepoint, scale, crps, overprediction, underprediction, dispersion) |>
    pivot_longer(cols=c("crps", "overprediction", "underprediction", "dispersion"), names_to="measure", values_to="value")
  
  # Make sure gen_time and inc_period are factors
  # Need to make gen_time and inc_period are factors so that ordering is correct:
  scores_cases_long$gen_time <- factor(scores_cases_long$gen_time, levels=c("no delay",
                                                                            "very low",
                                                                            "low",
                                                                            "correct",
                                                                            "high",
                                                                            "very high",
                                                                            "weight_prior_true",
                                                                            "weight_prior_false"))
  
  scores_cases_long$inc_period <- factor(scores_cases_long$inc_period, levels=c("no delay",
                                                                                "very low",
                                                                                "low",
                                                                                "correct",
                                                                                "high",
                                                                                "very high",
                                                                                "weight_prior_true",
                                                                                "weight_prior_false"))
  
  scores_cases_gen_time <- scores_cases_long |>
    group_by(gen_time, measure, timepoint) |>
    summarise(value=mean(value))
  
  scores_cases_inc_period <- scores_cases_long |>
    group_by(inc_period, measure, timepoint) |>
    summarise(value=mean(value))
  
  # And the mean scores
  mean_scores_cases_gen_time <- scores_cases_long |>
    group_by(gen_time, measure) |>
    summarise(value=mean(value))
  
  mean_scores_cases_inc_period <- scores_cases_long |>
    group_by(inc_period, measure) |>
    summarise(value=mean(value))
  
  barchart_mean_gen_time <- ggplot(mean_scores_cases_gen_time |> filter(measure!="crps")) + 
    geom_bar(aes(x=gen_time, y=value, fill=measure), stat="identity") +
    xlab("Generation time") +
    ylab("CRPS") +
    lshtm_theme() +
    theme(axis.text.x=element_text(angle=45, hjust=1),
          legend.position = "none",
          #axis.title.x=element_blank(),
          axis.title.y=element_blank())
  
  barchart_mean_inc_period <- ggplot(mean_scores_cases_inc_period |> filter(measure!="crps")) +
    geom_bar(aes(x=inc_period, y=value, fill=measure), stat="identity") +
    xlab("Incubation period") +
    ylab("CRPS") +
    lshtm_theme()  +
    theme(axis.text.x=element_text(angle=45, hjust=1),
          legend.position = "none",
          #axis.title.x=element_blank(),
          axis.title.y=element_blank())
  # Standard legend text size
  legend_text_size <- 10
  
  # Extract legends from `timeseries_performance` and `rank_plot` with consistent text size
  legend_performance <- get_legend(
    ggplot(res_performance_full) + 
      geom_line(mapping=aes(x=date, y=q0.5, colour=performance)) +
      geom_ribbon(mapping=aes(x=date, ymin=q0.25, ymax=q0.75, fill=performance), alpha=0.5) +
      geom_ribbon(mapping=aes(x=date, ymin=q0.025, ymax=q0.975, fill=performance), alpha=0.2) +
      scale_colour_manual(
        name = "Performance",
        values = c(
          # "Best-performing.estimate" = "lightblue",
          #  "Best-performing.estimate based on partial data" = "deepskyblue",
          "Best-performing" = "darkblue",
          # "Worst-performing.estimate" = "lightcoral",
          #  "Worst-performing.estimate based on partial data" = "indianred",
          "Worst-performing" = "darkred"
        ),
        breaks = c(
          # "Best-performing.estimate",
          #  "Best-performing.estimate based on partial data",
          "Best-performing",
          #"Worst-performing.estimate",
          #"Worst-performing.estimate based on partial data",
          "Worst-performing"
        )
      ) +
      scale_fill_manual(
        name = "Performance",
        values = c(
          #"Best-performing" = "lightblue",
          #"Best-performing.estimate based on partial data" = "deepskyblue",
          "Best-performing" = "darkblue",
          # "Worst-performing.estimate" = "lightcoral",
          #  "Worst-performing.estimate based on partial data" = "indianred",
          "Worst-performing" = "darkred"
        ),
        breaks = c(
          #"Best-performing.estimate",
          #"Best-performing.estimate based on partial data",
          "Best-performing",
          #"Worst-performing.estimate",
          #"Worst-performing.estimate based on partial data",
          "Worst-performing"
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
      scale_fill_gradientn(colors = terrain.colors(6), name="CRPS for case forecast") +
      theme_classic() +
      theme(axis.text.x = element_text(angle=45, hjust=0),
            legend.position = "right",
            legend.text = element_text(size = legend_text_size),
            legend.background = element_rect(fill = "white"))  # Adjust text size
  )
  
  legend_bar <- get_legend(
    ggplot(scores_cases_gen_time |> filter(measure!="crps")) + 
      geom_bar(aes(x=gen_time, y=value, fill=measure), stat="identity") +
      facet_wrap(~timepoint, nrow=1) +
      xlab("Generation time") +
      ylab("CRPS") +
      lshtm_theme() +
      theme(axis.text.x=element_text(angle=45, hjust=1),
            legend.position = "right",
            legend.text = element_text(size = legend_text_size),
            legend.background = element_rect(fill = "white")) 
  )
  
  combined_legends <- plot_grid(
    legend_rank, legend_bar,
    ncol = 1,  # Arrange legends in a single column
    align = "v",  # Align legends vertically
    axis = "tblr"  # Align all axes
  )
  
  # Combine plots and spacer without legends (in a single column)
  combined_plots <- plot_grid(
    timeseries_forecast, barchart_mean_gen_time, barchart_mean_inc_period,
    nrow = 1, # Arrange plots in a single column
    align = "v",  # Align plots vertically
    axis = "tblr",  # Align all axes  ,
    rel_widths = c(4, 1, 1)  # Adjust relative widths of plots
  )
  
  return(list(combined_plots = combined_plots, 
              combined_legends = combined_legends))
}


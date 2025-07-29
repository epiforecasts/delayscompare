plot_baseline_rt <- function(res_rt_samples, 
                          res_id, 
                          rt_dis,
                          scores_scen,
                          forecast_freq){
  
  dis_timepoints <- rt_dis$date[c(1:(nrow(rt_dis) %/% (forecast_freq*7)))*forecast_freq*7]
  
  res_rt_samples <- res_rt_samples |> left_join(res_id, by=c("gt", "result_list"))

  ## Rankings scores 
  
  # Rankings for each timepoint
  
  rankings <- scores_scen |>
    group_by(timepoint) |>
    filter(date == max(date)) |>
    mutate(rank = rank(crps)) |>
    ungroup()
  
  ## Overall rankings
  
  overall_rankings <- scores_scen |>
    group_by(timepoint) |>
    filter(date==max(date)) |>
    ungroup() |>
    group_by(gen_time, inc_period) |>
    summarise(crps = mean(crps), .groups = "drop") |>
    mutate(overall_rank = rank(crps),
           timepoint = "Overall")
    
  rankings <- rankings |>
    mutate(
      inc_period = factor(
        inc_period, levels=c("no delay", "very low", "low", "correct", "high", "very high")
      ),
      gen_time = factor(
        gen_time, levels=c("no delay", "very low", "low", "correct", "high", "very high")
      ))
  
  overall_rankings <- overall_rankings |>
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
  
  ## Best and worst performing forecasts plot over time
  worstbest <- rankings |> 
    filter(rank %in% c(1, max(rank))) |>
    select(timepoint, gen_time, inc_period, rank)
  
  worstbest <- worstbest |>
    mutate(performance=ifelse(rank==1, "Best-performing", "Worst-performing"))
  
  res_rt_performance <- res_rt_samples |>
    select(date, sample, value, type, timepoint, gen_time, inc_period, model) |>
    left_join(worstbest, by=c("timepoint", "gen_time", "inc_period")) |>
    filter(!is.na(performance))
  
  # Adding median and quantiles
  res_rt_performance <- res_rt_performance |>
    group_by(timepoint, date, gen_time, inc_period, type, performance) |>
    filter(type=="forecast") |>
    summarise(
      q0.025 = quantile(value, 0.025),
      q0.25 = quantile(value, 0.25),
      q0.5 = quantile(value, 0.5),
      q0.75 = quantile(value, 0.75),
      q0.975 = quantile(value, 0.975),
    )
  
  # Add all dates for plotting
  
  all_dates <- expand_grid(
    date = seq(min(res_rt_samples$date), max(res_rt_samples$date), by = "1 day"),
    performance = unique(res_rt_performance$performance)
  )
  
  # Merge with existing data, filling gaps with NA
  res_rt_performance_full <- merge(all_dates, res_rt_performance, by = c("date", "performance"), all.x = TRUE)
  
  ## Add true values
  res_rt_performance_full <- res_rt_performance_full |>
    left_join(rt_dis, by="date")
  
  # Plot 1: Timeseries Performance (remove legend from plot)
  timeseries_performance <- ggplot(res_rt_performance_full) + 
    geom_line(mapping=aes(x=date, y=q0.5, colour=performance)) +
    geom_ribbon(mapping=aes(x=date, ymin=q0.25, ymax=q0.75, fill=performance), alpha=0.5) +
    geom_ribbon(mapping=aes(x=date, ymin=q0.025, ymax=q0.975, fill=performance), alpha=0.2) +
    geom_line(mapping=aes(x=date, y=R), colour="black", size=0.5, linetype="dashed", show.legend=FALSE) +
    lshtm_theme() +
    scale_colour_manual(
      name = "Performance",
      values = c(
        #"Best-performing.estimate" = "lightblue",
        #"Best-performing.estimate based on partial data" = "deepskyblue",
        "Best-performing" = "darkblue",
        #"Worst-performing.estimate" = "lightcoral",
        #"Worst-performing.estimate based on partial data" = "indianred",
        "Worst-performing" = "darkred"
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
        "Best-performing" = "darkblue",
        "Worst-performing" = "darkred"
      ),
      breaks = c(
        "Best-performing",
        "Worst-performing"
      )
    ) +
    theme(legend.position = "none") + # Remove legend from timeseries_performance 
    xlab("Date") +
    ylab("Rt")
  
  # Plot 2a: Rank plot (remove legend from plot)
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
  
  # Plot 2b: Overall rank plot
  overall_rank_plot <- ggplot(overall_rankings, aes(x=gen_time, y=inc_period)) +
    geom_tile(aes(fill=overall_rank)) +
    xlab("Generation time") +
    ylab("Incubation period") +
    scale_fill_gradientn(colors = terrain.colors(6), name="CRPS for Rt estimate") +
    theme_classic() +
    facet_wrap(~timepoint, nrow=1) +
    theme(axis.text.x = element_text(angle=45, hjust=1),
          legend.position = "none",
          #axis.title.x=element_blank(),
          axis.title.y=element_blank()) +  # Remove legend from rank_plot
    lshtm_theme() +
    xlab(" ")
  
  # Plot 3: CRPS barchart
  scores_rt_long <- scores_scen |>
    select(date, type, gen_time, inc_period, timepoint, scale, scen, rt_traj, rt_opts, ur, crps, overprediction, underprediction, dispersion) |>
    pivot_longer(cols=c("crps", "overprediction", "underprediction", "dispersion"), names_to="measure", values_to="value")
  
  # Make sure gen_time and inc_period are factors
  # Need to make gen_time and inc_period are factors so that ordering is correct:
  scores_rt_long$gen_time <- factor(scores_rt_long$gen_time, levels=c("no delay",
                                                                                        "very low",
                                                                                        "low",
                                                                                        "correct",
                                                                                        "high",
                                                                                        "very high"))
  
  scores_rt_long$inc_period <- factor(scores_rt_long$inc_period, levels=c("no delay",
                                                                                                "very low",
                                                                                                "low",
                                                                                                "correct",
                                                                                                "high",
                                                                                                "very high"))
  
  scores_rt_gen_time <- scores_rt_long |>
    group_by(gen_time, rt_traj, measure, timepoint) |>
    filter(inc_period=="correct") |>
    summarise(value=mean(value))
  
  scores_rt_inc_period <- scores_rt_long |>
    group_by(inc_period, rt_traj, measure, timepoint) |>
    filter(gen_time=="correct") |>
    summarise(value=mean(value))
  
 # And the mean scores
  mean_scores_rt_gen_time <- scores_rt_long |>
    group_by(gen_time, rt_traj, measure) |>
    filter(inc_period=="correct") |>
    summarise(value=mean(value))
  
  mean_scores_rt_inc_period <- scores_rt_long |>
    group_by(inc_period, rt_traj, measure) |>
    filter(gen_time=="correct") |>
    summarise(value=mean(value))
  
  barchart_gen_time <- ggplot(scores_rt_gen_time |> filter(measure!="crps")) + 
    geom_bar(aes(x=gen_time, y=value, fill=measure), stat="identity") +
    facet_wrap(~timepoint, nrow=1) +
    xlab("Generation time") +
    ylab("CRPS") +
    lshtm_theme() +
    theme(axis.text.x=element_text(angle=45, hjust=1),
          legend.position = "none") # Remove legend from barchart_gen_time 
  
  barchart_inc_period <- ggplot(scores_rt_inc_period |> filter(measure!="crps")) +
    geom_bar(aes(x=inc_period, y=value, fill=measure), stat="identity") +
    facet_wrap(~timepoint, nrow=1) +
    xlab("Incubation period") +
    ylab("CRPS") +
    lshtm_theme()  +
    theme(axis.text.x=element_text(angle=45, hjust=1),
          legend.position = "none")
  
  barchart_mean_gen_time <- ggplot(mean_scores_rt_gen_time |> filter(measure!="crps")) + 
    geom_bar(aes(x=gen_time, y=value, fill=measure), stat="identity") +
    facet_wrap(~rt_traj, nrow=1) +
    xlab("Generation time") +
    ylab("CRPS") +
    lshtm_theme() +
    theme(axis.text.x=element_text(angle=45, hjust=1),
          legend.position = "none",
          #axis.title.x=element_blank(),
          axis.title.y=element_blank()) +
    xlab(" ")
  
  barchart_mean_inc_period <- ggplot(mean_scores_rt_inc_period |> filter(measure!="crps")) +
    geom_bar(aes(x=inc_period, y=value, fill=measure), stat="identity") +
    facet_wrap(~rt_traj, nrow=1) +
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
    ggplot(res_rt_performance_full) + 
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
      scale_fill_gradientn(colors = terrain.colors(6), name="CRPS for Rt estimate") +
      theme_classic() +
      theme(axis.text.x = element_text(angle=45, hjust=0),
            legend.position = "right",
            legend.text = element_text(size = legend_text_size),
            legend.background = element_rect(fill = "white"))  # Adjust text size
  )
  
  legend_bar <- get_legend(
    ggplot(scores_rt_gen_time |> filter(measure!="crps")) + 
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
    timeseries_performance, rank_plot, barchart_gen_time, barchart_inc_period,
    ncol = 1, # Arrange plots in a single column
    align = "v",  # Align plots vertically
    axis = "tblr"  # Align all axes  
  )
  
  # Add mean plots w/ spacer at the top so aligns with timeseries plots
  combined_plots_overall <- plot_grid(
    spacer, overall_rank_plot, barchart_mean_gen_time, barchart_mean_inc_period,
    ncol = 1, # Arrange plots in a single column
    align = "v",  # Align plots vertically
    axis = "tblr"  # Align all axes  
  )
  
  combined_plots <- plot_grid(combined_plots_ts, combined_plots_overall, ncol = 2, rel_widths=c(6,1.25),
                 align = "v", axis = "tblr")
  
  padded_legend_rank <- plot_grid(
    legend_rank,
    spacer,
    ncol = 2,
    rel_widths = c(1, 0.9)  # Adjust height to match the size of the other legend
  )
  
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
      legend_performance, legend_rank, legend_bar, spacer,  # Spacer included in the legends section
      ncol = 1,  # Place legends and spacer in a single column
      rel_heights = c(1, 1, 1),# Adjust spacer height, making it smaller to reduce gaps
      align = "l"
    ),
    ncol = 2,  # Combine plots and legends in two columns
    rel_widths = c(8, 1)  # Adjust width ratio (e.g., 4 times wider for plots)
  )
  
  return(final_plot)}

plot_baseline_cases <- function(res_samples, 
                             res_id, 
                             sim_data,
                             scores_scen,
                             forecast_freq){
  
  dis_timepoints <- sim_data$date[c(1:(nrow(sim_data) %/% (forecast_freq*7)))*forecast_freq*7]
  
  res_samples <- res_samples |> 
    # Make sure I have forecasts only
    filter(type=="forecast") |>
    left_join(res_id, by=c("gt", "result_list"))

  ## Rankings scores 
  
  # Rankings for each timepoint
  
  rankings <- scores_scen |>
    group_by(timepoint) |>
    filter(date == max(date)) |>
    mutate(rank = rank(crps)) |>
    ungroup()
  
  ## Overall rankings
  
  overall_rankings <- scores_scen |>
    group_by(timepoint) |>
    filter(date==max(date)) |>
    ungroup() |>
    group_by(gen_time, inc_period) |>
    summarise(crps = mean(crps), .groups = "drop") |>
    mutate(overall_rank = rank(crps),
           timepoint = "Overall")
  
  rankings <- rankings |>
    mutate(
      inc_period = factor(
        inc_period, levels=c("no delay", "very low", "low", "correct", "high", "very high")
      ),
      gen_time = factor(
        gen_time, levels=c("no delay", "very low", "low", "correct", "high", "very high")
      ))
  
  overall_rankings <- overall_rankings |>
    mutate(
      inc_period = factor(
        inc_period, levels=c("no delay", "very low", "low", "correct", "high", "very high")
      ),
      gen_time = factor(
        gen_time, levels=c("no delay", "very low", "low", "correct", "high", "very high")
      ))
  
  ### Heatmap by timepoint ###
  
  case_timepoints <- sim_data |> filter(date %in% dis_timepoints)
  
  timepoint_labels <- setNames(as.character(dis_timepoints), 
                               c(1:length(dis_timepoints)))
  
  ## Best and worst performing forecasts plot over time
  worstbest <- rankings |> 
    filter(rank %in% c(1, max(rank))) |>
    select(timepoint, gen_time, inc_period, rank)
  
  worstbest <- worstbest |>
    mutate(performance=ifelse(rank==1, "Best-performing", "Worst-performing"))
  
  res_performance <- res_samples |>
    select(date, sample, prediction, type, timepoint, gen_time, inc_period, model) |>
    left_join(worstbest, by=c("timepoint", "gen_time", "inc_period")) |>
    filter(!is.na(performance))
  
  # Adding median and quantiles
  res_performance <- res_performance |>
    group_by(timepoint, date, gen_time, inc_period, type, performance) |>
    filter(type=="forecast") |>
    summarise(
      q0.025 = quantile(prediction, 0.025),
      q0.25 = quantile(prediction, 0.25),
      q0.5 = quantile(prediction, 0.5),
      q0.75 = quantile(prediction, 0.75),
      q0.975 = quantile(prediction, 0.975),
    )
  
  # Add all dates for plotting
  
  all_dates <- expand_grid(
    date = seq(min(res_samples$date), max(res_samples$date), by = "1 day"),
    performance = unique(res_performance$performance)
  )
  
  # Merge with existing data, filling gaps with NA
  res_performance_full <- merge(all_dates, res_performance, by = c("date", "performance"), all.x = TRUE)
  
  ## Add true values
  
   # Want reported_cases only
  sim_data <- sim_data |>
    filter(variable=="reported_cases")
  
  res_performance_full <- res_performance_full |>
    left_join(sim_data, by="date")
  
  # Plot 1: Timeseries Performance (remove legend from plot)
  timeseries_performance <- ggplot(res_performance_full) + 
    geom_line(mapping=aes(x=date, y=q0.5, colour=performance)) +
    geom_ribbon(mapping=aes(x=date, ymin=q0.25, ymax=q0.75, fill=performance), alpha=0.5) +
    #geom_ribbon(mapping=aes(x=date, ymin=q0.025, ymax=q0.975, fill=performance), alpha=0.2) +
    geom_point(mapping=aes(x=date, y=value), colour="black", size=0.5, show.legend=FALSE) +
    lshtm_theme() +
    scale_colour_manual(
      name = "Performance",
      values = c(
        #"Best-performing.estimate" = "lightblue",
        #"Best-performing.estimate based on partial data" = "deepskyblue",
        "Best-performing" = "darkblue",
        #"Worst-performing.estimate" = "lightcoral",
        #"Worst-performing.estimate based on partial data" = "indianred",
        "Worst-performing" = "darkred"
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
        "Best-performing" = "darkblue",
        "Worst-performing" = "darkred"
      ),
      breaks = c(
        "Best-performing",
        "Worst-performing"
      )
    ) +
    theme(legend.position = "none") + # Remove legend from timeseries_performance 
    xlab("Date") +
    ylab("Rt")
  
  # Plot 2a: Rank plot (remove legend from plot)
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
  
  # Plot 2b: Overall rank plot
  overall_rank_plot <- ggplot(overall_rankings, aes(x=gen_time, y=inc_period)) +
    geom_tile(aes(fill=overall_rank)) +
    xlab("Generation time") +
    ylab("Incubation period") +
    scale_fill_gradientn(colors = terrain.colors(6), name="CRPS for Rt estimate") +
    theme_classic() +
    facet_wrap(~timepoint, nrow=1) +
    theme(axis.text.x = element_text(angle=45, hjust=1),
          legend.position = "none",
          #axis.title.x=element_blank(),
          axis.title.y=element_blank()) +  # Remove legend from rank_plot
    lshtm_theme() +
    xlab(" ")
  
  # Plot 3: CRPS barchart
  scores_cases_long <- scores_scen |>
    select(date, type, gen_time, inc_period, timepoint, scale, scen, rt_traj, rt_opts, ur, crps, overprediction, underprediction, dispersion) |>
    pivot_longer(cols=c("crps", "overprediction", "underprediction", "dispersion"), names_to="measure", values_to="value")
  
  # Make sure gen_time and inc_period are factors
  # Need to make gen_time and inc_period are factors so that ordering is correct:
  scores_cases_long$gen_time <- factor(scores_cases_long$gen_time, levels=c("no delay",
                                                                      "very low",
                                                                      "low",
                                                                      "correct",
                                                                      "high",
                                                                      "very high"))
  
  scores_cases_long$inc_period <- factor(scores_cases_long$inc_period, levels=c("no delay",
                                                                          "very low",
                                                                          "low",
                                                                          "correct",
                                                                          "high",
                                                                          "very high"))
  
  scores_cases_gen_time <- scores_cases_long |>
    group_by(gen_time, rt_traj, measure, timepoint) |>
    filter(inc_period=="correct") |>
    summarise(value=mean(value))
  
  scores_cases_inc_period <- scores_cases_long |>
    group_by(inc_period, rt_traj, measure, timepoint) |>
    filter(gen_time=="correct") |>
    summarise(value=mean(value))
  
  # And the mean scores
  mean_scores_cases_gen_time <- scores_cases_long |>
    group_by(gen_time, rt_traj, measure) |>
    filter(inc_period=="correct") |>
    summarise(value=mean(value))
  
  mean_scores_cases_inc_period <- scores_cases_long |>
    group_by(inc_period, rt_traj, measure) |>
    filter(gen_time=="correct") |>
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
    facet_wrap(~rt_traj, nrow=1) +
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
    facet_wrap(~rt_traj, nrow=1) +
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
    timeseries_performance, rank_plot, barchart_gen_time, barchart_inc_period,
    ncol = 1, # Arrange plots in a single column
    align = "v",  # Align plots vertically
    axis = "tblr"  # Align all axes  
  )
  
  # Add mean plots w/ spacer at the top so aligns with timeseries plots
  combined_plots_overall <- plot_grid(
    spacer, overall_rank_plot, barchart_mean_gen_time, barchart_mean_inc_period,
    ncol = 1, # Arrange plots in a single column
    align = "v",  # Align plots vertically
    axis = "tblr"  # Align all axes  
  )
  
  combined_plots <- plot_grid(combined_plots_ts, combined_plots_overall, ncol = 2, rel_widths=c(6,1.25),
                              align = "v", axis = "tblr")
  
  padded_legend_rank <- plot_grid(
    legend_rank,
    spacer,
    ncol = 2,
    rel_widths = c(1, 0.9)  # Adjust height to match the size of the other legend
  )
  
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
      legend_performance, legend_rank, legend_bar, spacer,  # Spacer included in the legends section
      ncol = 1,  # Place legends and spacer in a single column
      rel_heights = c(1, 1, 1),# Adjust spacer height, making it smaller to reduce gaps
      align = "l"
    ),
    ncol = 2,  # Combine plots and legends in two columns
    rel_widths = c(8, 1)  # Adjust width ratio (e.g., 4 times wider for plots)
  )
  
  return(final_plot)}


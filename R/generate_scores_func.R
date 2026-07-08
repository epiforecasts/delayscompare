## Need to generate scores as we go, in order to save memory ##
generate_scores_cases <- function(res_samples, res_id, sim_data_scen) {
  
# Add info to res_samples
join_keys <- c("result_list", "gt")
if ("inc" %in% names(res_samples) && "inc" %in% names(res_id)) {
  join_keys <- c(join_keys, "inc")
}
res_samples <- res_samples |>
  left_join(res_id, by = join_keys)

# Keep 1- and 2-week forecast horizons (day 7 and 14 relative to origin)
res_samples <- res_samples |>
  mutate(horizon = as.numeric(date - forecast_origin) / 7) |>
  filter(horizon %in% c(1, 2))

# Add simulated data to samples for rankings
res_samples <- sim_data_scen |>
  filter(variable=="reported_cases") |>
  rename(true_value=value) |>
  select(-variable) |>
  right_join(res_samples |> filter(type=="forecast"), by="date")

# Prepare samples for scoring and ranking
res_samples <- res_samples |>
  select(date, true_value, prediction, sample, model, gen_time, inc_period, timepoint, horizon, type)

res_samples <- as_forecast_sample(
  data=res_samples,
  forecast_unit=c("date", "type", "gen_time", "inc_period", "model", "timepoint", "horizon"),
  observed="true_value",
  predicted="prediction",
  sample_id='sample'
)

res_samples <- transform_forecasts(res_samples, fun = log_shift, offset = 1, label="log")

# Score on both natural and log scales
scores <- res_samples |>
  filter(type=="forecast") |>
  score()

return(scores)

}

## Need to generate scores as we go, in order to save memory ##
generate_scores_rt <- function(res_R, res_id, rt_traj_scen) {
  
  # Add result_list to res_id if not already present
  if (!"result_list" %in% names(res_id)) {
    res_id <- res_id |>
      group_by(gt) |>
      mutate(result_list = 1:n())
  }
  
  # Add info to res_R
  join_keys <- c("result_list", "gt")
  if ("inc" %in% names(res_R) && "inc" %in% names(res_id)) {
    join_keys <- c(join_keys, "inc")
  }
  res_R <- res_R |>
    left_join(res_id, by = join_keys)

  # Keep 1- and 2-week forecast horizons (day 7 and 14 relative to origin)
  res_R <- res_R |>
    mutate(horizon = as.numeric(date - forecast_origin) / 7) |>
    filter(horizon %in% c(1, 2))

  # Add simulated data to samples for rankings
  res_R <- rt_traj_scen |>
    right_join(res_R |> filter(type=="forecast"), by="date")

  # Prepare samples for scoring and ranking
  res_R <- res_R |>
    select(date, R, value, sample, model, gen_time, inc_period, timepoint, horizon, type)

  res_R <- as_forecast_sample(
    data=res_R,
    forecast_unit=c("date", "type", "gen_time", "inc_period", "model", "timepoint", "horizon"),
    observed="R",
    predicted="value",
    sample_id='sample'
  )
  
  res_R <- transform_forecasts(res_R, fun = log_shift, offset=1, label="log")
  
  scores <- res_R |>
    filter(type=="forecast", scale=="natural") |>
    score() 
  
  return(scores)
  
}


plot_heatmaps <- function(scores){

  ## Plotting overall, by rt_opts and by under-reporting status ##

# Add factors for plotting
scores <- scores |>
  mutate(
    inc_period = factor(
      inc_period, levels=c("no delay", "very low", "low", "correct", "high", "very high")
    ),
    gen_time = factor(
      gen_time, levels=c("no delay", "very low", "low", "correct", "high", "very high")
    ))

# Overall plot
rank_all <- scores |>
  summarise_scores(by=c("gen_time", "inc_period", "rt_traj")) |>
  group_by(rt_traj) |>
  mutate(rank=rank(crps))

rank_all_plot <- ggplot(rank_all, aes(x=gen_time, y=inc_period)) +
  geom_tile(aes(fill=rank)) +
  xlab("Generation time") +
  ylab("Incubation period") +
  scale_fill_gradientn(colours = terrain.colors(50), name="Ranking of two-week forecast") +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust=1), legend.position="none") +  
  facet_wrap(~rt_traj, strip.position="right", ncol=1) +
  lshtm_theme() 

# By rt_opts
rank_rtopts <- scores |>
  summarise_scores(by=c("gen_time", "inc_period", "rt_opts", "rt_traj")) |>
  group_by(rt_opts, rt_traj) |>
  mutate(rank=rank(crps))

rank_rtopts_plot <- ggplot(rank_rtopts, aes(x=gen_time, y=inc_period)) +
  geom_tile(aes(fill=rank)) +
  xlab("Generation time") +
  ylab("Incubation period") +
  scale_fill_gradientn(colours = terrain.colors(50), name="Ranking of two-week forecast") +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust=1), legend.position="none") +  
  facet_grid(rt_traj~rt_opts) +
  lshtm_theme() 

# By under-reporting status
rank_ur <- scores |>
  summarise_scores(by=c("gen_time", "inc_period", "ur", "rt_traj")) |>
  group_by(ur, rt_traj) |>
  mutate(rank=rank(crps))

rank_ur_plot <- ggplot(rank_ur, aes(x=gen_time, y=inc_period)) +
  geom_tile(aes(fill=rank)) +
  xlab("Generation time") +
  ylab("Incubation period") +
  scale_fill_gradientn(colours = terrain.colors(50), name="Ranking of two-week forecast") +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust=1), legend.position="none") +
  facet_grid(rt_traj~ur) +
  lshtm_theme()

# Legend

legend_rank <- get_legend(
  ggplot(rank_all, aes(x=gen_time, y=inc_period)) +
    geom_tile(aes(fill=rank)) +
    scale_fill_gradientn(colors = terrain.colors(50), name="Ranking of two-week forecast") +
    theme_classic() +
    theme(axis.text.x = element_text(angle=45, hjust=0),
          legend.position = "right",
          legend.text = element_text(size = 10))
)

final_heatmaps <- plot_grid(rank_all_plot, rank_rtopts_plot, rank_ur_plot, legend_rank, ncol=4, rel_widths=c(0.6, 1, 1, 0.5))

return(final_heatmaps)}

plot_boxplots <- function(scores, predictor, measure){
  
  # Add factors for plotting (same as original)
  scores <- scores |>
    mutate(
      inc_period = factor(
        inc_period, levels=c("no delay", "very low", "low", "correct", "high", "very high")
      ),
      gen_time = factor(
        gen_time, levels=c("no delay", "very low", "low", "correct", "high", "very high")
      )
    )
  
  # Convert the measure string to a symbol
  measure_sym <- sym(measure)
  
  # Overall plot (replacing heatmap with boxplot)
  rank_all <- scores |>
    summarise_scores(by=c("gen_time", "inc_period", "rt_traj"))
  
  if(predictor=="gen_time"){
  
  rank_all_plot <- ggplot(rank_all, aes(x=gen_time, y=!!measure_sym)) +
    geom_boxplot() +
    xlab("Generation time") +
    ylab(measure) +
    theme_classic() +
    theme(axis.text.x = element_text(angle=45, hjust=1)) +
    facet_wrap(~rt_traj, strip.position="right",ncol=1, scales = "free_y") +
    lshtm_theme()
  } else {
    rank_all_plot <- ggplot(rank_all, aes(x=inc_period, y=!!measure_sym)) +
      geom_boxplot() +
      xlab("Incubation period") +
      ylab(measure) +
      theme_classic() +
      theme(axis.text.x = element_text(angle=45, hjust=1)) +
      facet_wrap(~rt_traj, strip.position="right",ncol=1, scales = "free_y") +
      lshtm_theme()
  }
  
  # By rt_opts (replacing heatmap with boxplot)
  rank_rtopts <- scores |>
    summarise_scores(by=c("gen_time", "inc_period", "rt_opts", "rt_traj"))
  
  if(predictor=="gen_time"){
  
  rank_rtopts_plot <- ggplot(rank_rtopts, aes(x=gen_time, y=!!measure_sym)) +
    geom_boxplot() +
    xlab("Generation time") +
    ylab(paste0(measure)) +
    theme_classic() +
    theme(axis.text.x = element_text(angle=45, hjust=1)) +
    facet_grid(rt_traj~rt_opts, scales="free_y")  +
    lshtm_theme()
  } else {
    rank_rtopts_plot <- ggplot(rank_rtopts, aes(x=inc_period, y=!!measure_sym)) +
      geom_boxplot() +
      xlab("Incubation period") +
      ylab(paste0(measure)) +
      theme_classic() +
      theme(axis.text.x = element_text(angle=45, hjust=1)) +
      facet_grid(rt_traj~rt_opts, scales="free_y")  +
      lshtm_theme()
  }
  
  # By under-reporting status (replacing heatmap with boxplot)
  rank_ur <- scores |>
    summarise_scores(by=c("gen_time", "inc_period", "ur", "rt_traj"))
  
  if(predictor=="gen_time"){
  
  rank_ur_plot <- ggplot(rank_ur, aes(x=gen_time, y=!!measure_sym)) +
    geom_boxplot() +
    xlab("Generation time") +
    ylab(paste0(measure)) +
    theme_classic() +
    theme(axis.text.x = element_text(angle=45, hjust=1)) +
    facet_grid(rt_traj~ur, scales="free_y")  +
    lshtm_theme()
  } else {
    rank_ur_plot <- ggplot(rank_ur, aes(x=inc_period, y=!!measure_sym)) +
      geom_boxplot() +
      xlab("Incubation period") +
      ylab(paste0(measure)) +
      theme_classic() +
      theme(axis.text.x = element_text(angle=45, hjust=1)) +
      facet_grid(rt_traj~ur, scales="free_y")  +
      lshtm_theme()
  }
  
  # Combine plots
  final_boxplots <- plot_grid(rank_all_plot, rank_rtopts_plot, rank_ur_plot, ncol=3, rel_widths=c(0.6, 1, 1))
  
  return(final_boxplots)
}

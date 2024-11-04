## Need to generate scores as we go, in order to save memory ##
generate_scores_cases <- function(res_samples, res_id, sim_data_scen) {
  
  # Need to add result_list to res_id
  res_id <- res_id |> 
    group_by(gt) |>
    mutate(result_list=1:n())
    
# Add info to res_samples
res_samples <- res_samples |>
  left_join(res_id, by=c("result_list", "gt"))

# Get 2-week forecast only
res_samples <- res_samples |>
  group_by(timepoint) |>
  filter(date == max(date))                                                                           

# Add simulated data to samples for rankings
res_samples <- sim_data_scen |>
  filter(variable=="reported_cases") |>
  rename(true_value=value) |>
  select(-variable) |>
  right_join(res_samples |> filter(type=="forecast"), by="date")

# Prepare samples for scoring and ranking
res_samples <- res_samples |>
  select(date, true_value, prediction, sample, model, gen_time, inc_period, timepoint, type) 

res_samples <- as_forecast_sample(
  data=res_samples,
  forecast_unit=c("date", "type", "gen_time", "inc_period", "model", "timepoint"),
  observed="true_value",
  predicted="prediction",
  sample_id='sample'
)

res_samples <- transform_forecasts(res_samples, fun = log_shift, offset = 1, label="log")

scores <- res_samples |>
  filter(type=="forecast", scale=="log") |>
  score() 

return(scores)

}

## Need to generate scores as we go, in order to save memory ##
generate_scores_rt <- function(res_R, res_id, rt_traj_scen) {
  
  # Need to add result_list to res_id
  res_id <- res_id |> 
    group_by(gt) |>
    mutate(result_list=1:n())
  
  # Add info to res_R
  res_R <- res_R |>
    left_join(res_id, by=c("result_list", "gt"))
  
  # Get 2-week forecast only
  res_R <- res_R |>
    group_by(timepoint) |>
    filter(date == max(date))
  
  # Add simulated data to samples for rankings
  res_R <- rt_traj_scen |>
    right_join(res_R |> filter(type=="forecast"), by="date")
  
  # Prepare samples for scoring and ranking
  res_R <- res_R |>
    select(date, R, value, sample, model, gen_time, inc_period, timepoint, type) 
  
  res_R <- as_forecast_sample(
    data=res_R,
    forecast_unit=c("date", "type", "gen_time", "inc_period", "model", "timepoint"),
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


generate_plots <- function(disease, predictor, startdate){
  
  ## Loading all data - collect Rt trajectories and simulated data in a list ##
  
  rt_traj <- read_latest(here("data"), paste0("rt_traj_list_", disease))
  sim_data <- read_latest(here("data"), paste0("sim_data_list_", disease))
  
  # Scenario labels
  scen_labs <- data.frame(scen=c(1:16),
                          rt_traj=c(rep("const_low", 4), rep("const_high", 4), rep("inc", 4), rep("dec", 4)),
                          rt_opts=rep(c("latest", "latest", "project", "project"), 4),
                          ur=rep(c("No under-reporting", "Under-reporting", "No under-reporting", "Under-reporting"), 4))
  
  rt_traj_scen <- list()
  sim_data_scen <- list()
  scores_cases <- list()
  scores_rt <- list()
  
  for(i in 1:16){
    
    ## Loading data ##
    rt_traj_scen[[i]] <- rt_traj[[i]] |> 
      as.data.frame() |> 
      mutate(scen=i)
    
    sim_data_scen[[i]] <- sim_data[[i]] |> 
      as.data.frame()|> 
      mutate(scen=i)
    
    ## Loading results & generating scores as we go in order to save memory ##
    res_samples <- read_latest(here(paste0("results/", disease, "/", disease)), paste0("res_", disease, "scen", i, "_all_samples"))
    res_R <- read_latest(here(paste0("results/", disease, "/", disease)), paste0("res_", disease, "scen", i, "_all_R"))
    res_id <- read_latest(here(paste0("results/", disease, "/",  disease)), paste0("res_", disease, "scen", i, "_all_id")) 
    
    res_samples <- res_samples |> filter(date <= as.Date(startdate) + 6*4*7)
    res_R <- res_R |> filter(date <= as.Date(startdate) + 6*4*7)
    
    scores_cases[[i]] <- generate_scores_cases(res_samples, res_id, sim_data_scen[[i]]) |> mutate(scen=i)
    scores_rt[[i]] <- generate_scores_rt(res_R, res_id, rt_traj_scen[[i]]) |> mutate(scen=i)
    
  }
  
  rt_traj_scen <- bind_rows(rt_traj_scen)
  sim_data_scen <- bind_rows(sim_data_scen)
  scores_cases <- bind_rows(scores_cases)
  scores_rt <- bind_rows(scores_rt)
  
  # Add scenario labels
  rt_traj_scen <- rt_traj_scen |> 
    left_join(scen_labs, by="scen")
  
  sim_data_scen <- sim_data_scen |>
    left_join(scen_labs, by="scen")
  
  scores_cases <- scores_cases |>
    left_join(scen_labs, by="scen")
  
  scores_rt <- scores_rt |>
    left_join(scen_labs, by="scen")
  
  box_overpredict_cases <- plot_boxplots(scores_cases, predictor, "overprediction")
  box_overpredict_rt <- plot_boxplots(scores_rt, predictor, "overprediction")
  
  ggsave(paste0("results/plot", disease, predictor, "_cases_overprediction_boxplot.png"), box_overpredict_cases, width=33, height=15, units="cm")
  ggsave(paste0("results/plot", disease, predictor,"_rt_overprediction_boxplot.png"), box_overpredict_rt, width=33, height=15, units="cm")
  
  box_underpredict_cases <- plot_boxplots(scores_cases, predictor, "underprediction")
  box_underpredict_rt <- plot_boxplots(scores_rt, predictor, "underprediction")
  
  ggsave(paste0("results/plot", disease, predictor, "_cases_underprediction_boxplot.png"), box_underpredict_cases, width=33, height=15, units="cm")
  ggsave(paste0("results/plot", disease, predictor,"_rt_underprediction_boxplot.png"), box_underpredict_rt, width=33, height=15, units="cm")
  
  box_crps_cases <- plot_boxplots(scores_cases, predictor, "crps")
  box_crps_rt <- plot_boxplots(scores_rt, predictor, "crps")
  
  ggsave(paste0("results/plot", disease, predictor, "_cases_crps_boxplot.png"), box_crps_cases, width=33, height=15, units="cm")
  ggsave(paste0("results/plot", disease, predictor, "_rt_crps_boxplot.png"), box_crps_rt, width=33, height=15, units="cm")
  
  box_disp_cases <- plot_boxplots(scores_cases, predictor, "dispersion")
  box_disp_rt <- plot_boxplots(scores_rt, predictor, "dispersion")
  
  ggsave(paste0("results/plot", disease, predictor, "_cases_disp_boxplot.png"), box_disp_cases, width=33, height=15, units="cm")
  ggsave(paste0("results/plot", disease, predictor, "_rt_disp_boxplot.png"), box_disp_rt, width=33, height=15, units="cm")
}

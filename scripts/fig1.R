##################
#### Figure 1 ####
##################

library(here)
source(here("scripts", "01_packages.R"))
source(here("R", "funcs_plots.R"))
source(here("R", "funcs_data.R"))
source(here("R", "generate_scores_func.R"))
source(here("R", "lshtm_theme.R"))

# Make sure data lists are up-to-date
source(here("scripts","datacollect_ebola.R"))
source(here("scripts", "datacollect_cholera.R"))
source(here("scripts", "datacollect_covid.R"))

# Loading all data - collect Rt trajectories and simulated data in a list
disease <- "ebola"
rt_traj <- read_latest(here("data"), paste0("rt_traj_list_", disease))
sim_data <- read_latest(here("data"), paste0("sim_data_list_", disease))

# Scenario labels
scen_labs <- data.frame(scen=c(1:16),
                        rt_traj=c(rep("const_low", 4), rep("const_high", 4), rep("inc", 4), rep("dec", 4)),
                        rt_opts=rep(c("latest", "latest", "project", "project"), 4),
                        ur=rep(c("No under-reporting", "Under-reporting", "No under-reporting", "Under-reporting"), 4))

# Adding labels to data
rt_traj_scen <- lapply(c(1:16), function(i){
  rt_traj[[i]] |> 
    as.data.frame() |> 
    mutate(scen=i)
})

sim_data_scen <- lapply(c(1:16), function(i){
  sim_data[[i]] |> 
    as.data.frame()|> 
    mutate(scen=i)
})

## Create plot for each Rt trajectory

# Create Rt trajectory list
rankings_cases_list <- list()
rankings_rt_list <- list()

for(i in seq(1, 16, 4)){
  
  # Get all scenarios with that Rt trajectory
  scen_traj <- c(i, i+1, i+2, i+3)
  scen_lab <- scen_labs$rt_traj[scen_labs$scen==i]
  
  # Create scores lists
  scores_cases_list <- list()
  scores_rt_list <- list()
  
  for(j in scen_traj){
    
    # Get results
    res_samples <- read_latest(here(paste0("results/", disease)), paste0("res_", disease, "scen", j, "_all_samples"))
    res_R <- read_latest(here(paste0("results/", disease)), paste0("res_", disease, "scen", j, "_all_R"))
    res_id <- read_latest(here(paste0("results/", disease)), paste0("res_", disease, "scen", j, "_all_id")) 
    
    # Add scenario info
    res_samples <- res_samples |>
      filter(type=="forecast") |>
      # add info
      left_join(res_id, by=c("result_list", "gt"))
    
    res_R <- res_R |> 
      filter(type=="forecast") |>
      rename(prediction=value) |>
      # add info
      left_join(res_id, by=c("result_list", "gt"))
    
    # Add simulated data
    res_samples <- sim_data_scen[[j]] |>
      filter(variable=="reported_cases") |>
      rename(true_value=value) |>
      select(-variable) |>
      right_join(res_samples, by="date")
    
    res_R <- rt_traj_scen[[j]] |> 
      rename(true_value=R) |>
      right_join(res_R, by="date") 
    
    # Get rid of all columns that aren't date, true_value, prediction, sample
    res_samples <- res_samples |>
      select(date, true_value, prediction, sample, model, result_list, gt, type)
    
    res_R <- res_R |>
      select(date, true_value, prediction, sample, model, result_list, gt, type)
    
    # Log transform observations and predicted values
    res_samples <- as_forecast_sample(
      data=res_samples,
      forecast_unit=c("date", "type", "result_list" , "gt", "model"),
      observed="true_value",
      predicted="prediction",
      sample_id='sample'
    )
    
    res_samples <- res_samples |> 
      transform_forecasts(fun = log_shift, offset=1, label="log")
    
    res_R <- as_forecast_sample(
      data=res_R,
      forecast_unit=c("date", "type", "result_list" , "gt", "model"),
      observed="true_value",
      predicted="prediction",
      sample_id='sample'
    )
    
    res_R <- res_R |>
      transform_forecasts(fun = log_shift, offset=1, label="log")
    
    scores_cases <- res_samples |>
      # filtering out what I don't need to save memory
      filter(type=="forecast", scale=="log") |>
      score()
    
    scores_rt <- res_R |>
      # filtering out what I don't need to save memory
      filter(type=="forecast", scale=="log") |>
      score()
    
    scores_cases <- scores_cases |>
      left_join(res_id, by = c("result_list", "gt")) |>
      group_by(timepoint) |>
      filter(date == max(date)) |>
      ungroup() 
    
    scores_rt <- scores_rt |>
      left_join(res_id, by = c("result_list", "gt")) |>
      group_by(timepoint) |>
      filter(date == max(date)) |>
      ungroup()

    # Add scores to a list
    scores_cases_list[[j]] <- scores_cases
    scores_rt_list[[j]] <- scores_rt
  }
  
  scores_cases <- bind_rows(scores_cases)
  scores_rt <- bind_rows(scores_rt)
  
  # Get rankings
  rankings_cases <- scores_cases |>
    group_by(gen_time, inc_period) |>
    summarise(crps=mean(crps)) |>
    ungroup() |>
    mutate(rank = rank(crps))
  
  rankings_rt <- scores_rt |>
    group_by(gen_time, inc_period) |>
    summarise(crps=mean(crps)) |>
    ungroup() |>
    mutate(rank = rank(crps))
  
  # Making sure ordering is correct on plot
  rankings_cases <- rankings_cases |>
    mutate(
      inc_period = factor(
        inc_period, levels=c("no delay", "very low", "low", "correct", "high", "very high")
      ),
      gen_time = factor(
        gen_time, levels=c("no delay", "very low", "low", "correct", "high", "very high")
      ),
      rt_traj=scen_lab)
  
  rankings_rt <- rankings_rt |>
    mutate(
      inc_period = factor(
        inc_period, levels=c("no delay", "very low", "low", "correct", "high", "very high")
      ),
      gen_time = factor(
        gen_time, levels=c("no delay", "very low", "low", "correct", "high", "very high")
      ),
      rt_traj=scen_lab)
  
  rankings_cases_list <- c(rankings_cases_list, list(rankings_cases))
  rankings_rt_list <- c(rankings_rt_list, list(rankings_rt))
}

## Plotting
rankings_cases_list <- bind_rows(rankings_cases_list)
rankings_rt_list <- bind_rows(rankings_rt_list)

rank_cases_plot <- ggplot(rankings_cases_list, aes(x=gen_time, y=inc_period)) +
  geom_tile(aes(fill=rank)) +
  xlab("Generation time") +
  ylab("Incubation period") +
  scale_fill_gradientn(colours = terrain.colors(50), name="Ranking of two-week forecast") +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust=1), legend.position="none") +
  lshtm_theme() +
  facet_wrap(~rt_traj, scales="free")

rank_rt_plot <- ggplot(rankings_rt_list, aes(x=gen_time, y=inc_period)) +
  geom_tile(aes(fill=rank)) +
  xlab("Generation time") +
  ylab("Incubation period") +
  scale_fill_gradientn(colours = terrain.colors(50), name="Ranking of two-weeks forecast") +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  lshtm_theme() +
  facet_wrap(~rt_traj, scales="free")

plot_grid(rank_cases_plot, rank_rt_plot, ncol=2, rel_widths=c(1, 1.5), labels=c("A", "B"))

####### Would also be good to show as a barchart -> but want to show CRPS values rather than rank
mean_rt_gen_time <- rankings_rt_list |> 
  group_by(gen_time, rt_traj) |>
  summarise(mean_crps=mean(crps))

mean_rank_rt_inc_period <- rankings_rt_list |>
  group_by(inc_period, rt_traj) |>
  summarise(mean_crps=mean(crps))

mean_rank_rt_gen_plot <- ggplot(mean_rank_rt_gen_time, aes(x=gen_time, y=mean_crps)) +
  geom_col() +
  xlab("Generation time") +
  ylab("Mean CRPS") +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  lshtm_theme() +
  facet_wrap(~rt_traj, scales="free")

mean_rank_rt_inc_plot <- ggplot(mean_rank_rt_inc_period, aes(x=inc_period, y=mean_crps)) +
  geom_col() +
  xlab("Incubation period") +
  ylab("Mean CRPS") +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  lshtm_theme() +
  facet_wrap(~rt_traj, scales="free")

plot_grid(mean_rank_rt_gen_plot, mean_rank_rt_inc_plot, ncol=2, rel_widths=c(1, 1), labels=c("A", "B"))

## For cases ##

mean_rank_cases_gen_time <- rankings_cases_list |> 
  group_by(gen_time, rt_traj) |>
  summarise(mean_rank=mean(rank),
            mean_crps=mean(crps))

mean_rank_cases_inc_period <- rankings_cases_list |>
  group_by(inc_period, rt_traj) |>
  summarise(mean_rank=mean(rank),
            mean_crps=mean(crps))

mean_rank_cases_gen_plot <- ggplot(mean_rank_cases_gen_time, aes(x=gen_time, y=mean_crps)) +
  geom_col() +
  xlab("Generation time") +
  ylab("Mean CRPS") +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  lshtm_theme() +
  facet_wrap(~rt_traj, scales="free")

mean_rank_cases_inc_plot <- ggplot(mean_rank_cases_inc_period, aes(x=inc_period, y=mean_crps)) +
  geom_col() +
  xlab("Incubation period") +
  ylab("Mean CRPS") +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  lshtm_theme() +
  facet_wrap(~rt_traj, scales="free")

plot_grid(mean_rank_cases_gen_plot, mean_rank_cases_inc_plot, ncol=2, rel_widths=c(1, 1), labels=c("A", "B"))



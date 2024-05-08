
plotforecasts <- function(res_samples,
                          res_id,
                          sim_data){

res_samples_plot <- res_samples |> 
  filter(type=="forecast") |>
  # add info
  left_join(res_id, by="result_list")

sim_data_cases <- sim_data |> 
  filter(variable=="reported_cases") 

# Add rest of data
res_samples_plot <- sim_data_cases |>
  rename(true_value=value) |>
  select(date, true_value) |>
  left_join(res_samples_plot, by="date")

res_samples_plot$gen_time <- factor(res_samples_plot$gen_time, levels=c("very low", "low", "correct", "high", "very high"))
res_samples_plot$inc_period <- factor(res_samples_plot$inc_period, levels=c("very low", "low", "correct", "high", "very high"))

#### Generation time plot ####

res_samples_plot_gentime <- res_samples_plot |>
  filter(is.na(inc_period) | inc_period=="correct")

res_samples_plot_gentime <- sample_to_quantile(res_samples_plot_gentime, quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95), type = 7)

res_samples_plot_gentime <- res_samples_plot_gentime |>
  pivot_wider(names_from="quantile",
              values_from="prediction")

res_samples_plot_gentime <- res_samples_plot_gentime |>
  rename(lower90="0.05",
         upper90="0.95",
         lower50="0.25",
         upper50="0.75",
         median="0.5") # |>
# filter(!is.na(gen_time))

# Get rid of NA
res_samples_plot_gentime <- res_samples_plot_gentime |>
  filter(!is.na(gen_time))

## Generation time plot ##

# Simulated data
forecasts_gentime <- ggplot() + geom_line(sim_data_cases, mapping=aes(x=date, y=value))
# Forecasts
for(i in 1:max(res_samples_plot_gentime$timepoint)){
  forecasts_gentime <- forecasts_gentime + 
    geom_line(res_samples_plot_gentime |> filter(timepoint==i | is.na(timepoint)), mapping=aes(x=date, y=median, color=gen_time)) +
    geom_ribbon(res_samples_plot_gentime |> filter(timepoint==i | is.na(timepoint)), mapping=aes(x=date, ymin=lower50, ymax=upper50, fill=gen_time), alpha=0.5) +
    geom_ribbon(res_samples_plot_gentime |> filter(timepoint==i | is.na(timepoint)), mapping=aes(x=date, ymin=lower90, ymax=upper90, fill=gen_time), alpha=0.2)
}
# Aesthetics
forecasts_gentime <- forecasts_gentime + 
  scale_fill_viridis(discrete=TRUE, name="Generation time") +
  scale_colour_viridis(discrete=TRUE, name="Generation time") +
  xlab("Date") +
  ylab("Reported casts") +
  facet_wrap(~gen_time, ncol=1) +
  lshtm_theme() +
  ylim(0, max(res_samples_plot_gentime$upper90))

#### Incubation period plot ####

res_samples_plot_incperiod <- res_samples_plot |>
  filter(is.na(gen_time) | gen_time=="correct")

res_samples_plot_incperiod <- sample_to_quantile(res_samples_plot_incperiod, quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95), type = 7)

res_samples_plot_incperiod <- res_samples_plot_incperiod |>
  pivot_wider(names_from="quantile",
              values_from="prediction")

res_samples_plot_incperiod <- res_samples_plot_incperiod |>
  rename(lower90="0.05",
         upper90="0.95",
         lower50="0.25",
         upper50="0.75",
         median="0.5") 

# Get rid of NA
res_samples_plot_incperiod <- res_samples_plot_incperiod |>
  filter(!is.na(inc_period))

## Incubation period plot ##

# Simulated data
forecasts_incperiod <- ggplot() + geom_line(sim_data_cases, mapping=aes(x=date, y=value))
# Forecasts
for(i in 1:max(res_samples_plot_incperiod$timepoint)){
  forecasts_incperiod <- forecasts_incperiod + 
    geom_line(res_samples_plot_incperiod |> filter(timepoint==i | is.na(timepoint)), mapping=aes(x=date, y=median, color=inc_period)) +
    geom_ribbon(res_samples_plot_incperiod |> filter(timepoint==i | is.na(timepoint)), mapping=aes(x=date, ymin=lower50, ymax=upper50, fill=inc_period), alpha=0.5) +
    geom_ribbon(res_samples_plot_incperiod |> filter(timepoint==i | is.na(timepoint)), mapping=aes(x=date, ymin=lower90, ymax=upper90, fill=inc_period), alpha=0.2)
}
# Aesthetics
forecasts_incperiod <- forecasts_incperiod + 
  scale_fill_viridis(discrete=TRUE, name="Incubation period") +
  scale_colour_viridis(discrete=TRUE, name="Incubation period") +
  xlab("Date") +
  ylab("Reported casts") +
  facet_wrap(~inc_period, ncol=1) +
  lshtm_theme() +
  ylim(0, max(res_samples_plot_incperiod$upper90))


plot_grid(forecasts_gentime, forecasts_incperiod, ncol=2)
          }



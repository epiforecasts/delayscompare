
ebola_samples_plot <- ebola_samples |> 
  filter(type=="forecast", scale=="natural") |>
  # add info
  left_join(res_ebola_id, by="result_list")

# Add rest of data
ebola_samples_plot <- ebola_sim_data_infections |>
  rename(true_value=value) |>
  select(date, true_value) |>
  left_join(ebola_samples_plot |> select(-true_value), by="date")

ebola_samples_plot$gen_time <- factor(ebola_samples_plot$gen_time, levels=c("very low", "low", "correct", "high", "very high"))
ebola_samples_plot$inc_period <- factor(ebola_samples_plot$inc_period, levels=c("very low", "low", "correct", "high", "very high"))

#### Generation time plot ####

ebola_samples_plot_gentime <- ebola_samples_plot |>
  filter(is.na(inc_period) | inc_period=="correct")

ebola_samples_plot_gentime <- sample_to_quantile(ebola_samples_plot_gentime, quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95), type = 7)

ebola_samples_plot_gentime <- ebola_samples_plot_gentime |>
  pivot_wider(names_from="quantile",
              values_from="prediction")

ebola_samples_plot_gentime <- ebola_samples_plot_gentime |>
  rename(lower90="0.05",
         upper90="0.95",
         lower50="0.25",
         upper50="0.75",
         median="0.5") # |>
# filter(!is.na(gen_time))

# Get rid of NA
ebola_samples_plot_gentime <- ebola_samples_plot_gentime |>
  filter(!is.na(gen_time))

## Need to find a better way to do this ##
plot_gentime <- ggplot() +
  geom_line(ebola_sim_data_infections |> filter(date<="2015-04-07"), mapping=aes(x=date, y=value)) +
  geom_line(ebola_samples_plot_gentime |> filter(timepoint==1 | is.na(timepoint)), mapping=aes(x=date, y=median, color=gen_time)) +
  geom_ribbon(ebola_samples_plot_gentime |> filter(timepoint==1 | is.na(timepoint)), mapping=aes(x=date, ymin=lower50, ymax=upper50, fill=gen_time), alpha=0.5) +
  geom_ribbon(ebola_samples_plot_gentime |> filter(timepoint==1 | is.na(timepoint)), mapping=aes(x=date, ymin=lower90, ymax=upper90, fill=gen_time), alpha=0.2) +
  
  geom_line(ebola_samples_plot_gentime |> filter(timepoint==2 | is.na(timepoint)), mapping=aes(x=date, y=median, color=gen_time)) +
  geom_ribbon(ebola_samples_plot_gentime |> filter(timepoint==2 | is.na(timepoint)), mapping=aes(x=date, ymin=lower50, ymax=upper50, fill=gen_time), alpha=0.5) +
  geom_ribbon(ebola_samples_plot_gentime |> filter(timepoint==2 | is.na(timepoint)), mapping=aes(x=date, ymin=lower90, ymax=upper90, fill=gen_time), alpha=0.2) +
  
  geom_line(ebola_samples_plot_gentime |> filter(timepoint==3 | is.na(timepoint)), mapping=aes(x=date, y=median, color=gen_time)) +
  geom_ribbon(ebola_samples_plot_gentime |> filter(timepoint==3 | is.na(timepoint)), mapping=aes(x=date, ymin=lower50, ymax=upper50, fill=gen_time), alpha=0.5) +
  geom_ribbon(ebola_samples_plot_gentime |> filter(timepoint==3 | is.na(timepoint)), mapping=aes(x=date, ymin=lower90, ymax=upper90, fill=gen_time), alpha=0.2) +
  
  geom_line(ebola_samples_plot_gentime |> filter(timepoint==4 | is.na(timepoint)), mapping=aes(x=date, y=median, color=gen_time)) +
  geom_ribbon(ebola_samples_plot_gentime |> filter(timepoint==4 | is.na(timepoint)), mapping=aes(x=date, ymin=lower50, ymax=upper50, fill=gen_time), alpha=0.5) +
  geom_ribbon(ebola_samples_plot_gentime |> filter(timepoint==4 | is.na(timepoint)), mapping=aes(x=date, ymin=lower90, ymax=upper90, fill=gen_time), alpha=0.2) +
  
  geom_line(ebola_samples_plot_gentime |> filter(timepoint==5 | is.na(timepoint)), mapping=aes(x=date, y=median, color=gen_time)) +
  geom_ribbon(ebola_samples_plot_gentime |> filter(timepoint==5 | is.na(timepoint)), mapping=aes(x=date, ymin=lower50, ymax=upper50, fill=gen_time), alpha=0.5) +
  geom_ribbon(ebola_samples_plot_gentime |> filter(timepoint==5 | is.na(timepoint)), mapping=aes(x=date, ymin=lower90, ymax=upper90, fill=gen_time), alpha=0.2) +

scale_fill_viridis(discrete=TRUE, name="Generation time") +
  scale_colour_viridis(discrete=TRUE, name="Generation time") +
  xlab("Date") +
  ylab("Infections") +
  facet_wrap(~gen_time, ncol=1) +
  lshtm_theme() +
  ylim(0, max(ebola_samples_plot_gentime$upper90))

#### Incubation period plot ####

ebola_samples_plot_incperiod <- ebola_samples_plot |>
  filter(is.na(gen_time) | gen_time=="correct")

ebola_samples_plot_incperiod <- sample_to_quantile(ebola_samples_plot_incperiod, quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95), type = 7)

ebola_samples_plot_incperiod <- ebola_samples_plot_incperiod |>
  pivot_wider(names_from="quantile",
              values_from="prediction")

ebola_samples_plot_incperiod <- ebola_samples_plot_incperiod |>
  rename(lower90="0.05",
         upper90="0.95",
         lower50="0.25",
         upper50="0.75",
         median="0.5") # |>
# filter(!is.na(gen_time))

# Get rid of NA
ebola_samples_plot_incperiod <- ebola_samples_plot_incperiod |>
  filter(!is.na(inc_period))

plot_incperiod <- ggplot() +
  geom_line(ebola_sim_data_infections, mapping=aes(x=date, y=value), width=0.8) +
  geom_line(ebola_samples_plot_incperiod |> filter(timepoint==1 | is.na(timepoint)), mapping=aes(x=date, y=median, color=inc_period)) +
  geom_ribbon(ebola_samples_plot_incperiod |> filter(timepoint==1 | is.na(timepoint)), mapping=aes(x=date, ymin=lower50, ymax=upper50, fill=inc_period), alpha=0.5) +
  geom_ribbon(ebola_samples_plot_incperiod |> filter(timepoint==1 | is.na(timepoint)), mapping=aes(x=date, ymin=lower90, ymax=upper90, fill=inc_period), alpha=0.2) +
  
  geom_line(ebola_samples_plot_incperiod |> filter(timepoint==2 | is.na(timepoint)), mapping=aes(x=date, y=median, color=inc_period)) +
  geom_ribbon(ebola_samples_plot_incperiod |> filter(timepoint==2 | is.na(timepoint)), mapping=aes(x=date, ymin=lower50, ymax=upper50, fill=inc_period), alpha=0.5) +
  geom_ribbon(ebola_samples_plot_incperiod |> filter(timepoint==2 | is.na(timepoint)), mapping=aes(x=date, ymin=lower90, ymax=upper90, fill=inc_period), alpha=0.2) +
  
  geom_line(ebola_samples_plot_incperiod |> filter(timepoint==3 | is.na(timepoint)), mapping=aes(x=date, y=median, color=inc_period)) +
  geom_ribbon(ebola_samples_plot_incperiod |> filter(timepoint==3 | is.na(timepoint)), mapping=aes(x=date, ymin=lower50, ymax=upper50, fill=inc_period), alpha=0.5) +
  geom_ribbon(ebola_samples_plot_incperiod |> filter(timepoint==3 | is.na(timepoint)), mapping=aes(x=date, ymin=lower90, ymax=upper90, fill=inc_period), alpha=0.2) +
  
  geom_line(ebola_samples_plot_incperiod |> filter(timepoint==4 | is.na(timepoint)), mapping=aes(x=date, y=median, color=inc_period)) +
  geom_ribbon(ebola_samples_plot_incperiod |> filter(timepoint==4 | is.na(timepoint)), mapping=aes(x=date, ymin=lower50, ymax=upper50, fill=inc_period), alpha=0.5) +
  geom_ribbon(ebola_samples_plot_incperiod |> filter(timepoint==4 | is.na(timepoint)), mapping=aes(x=date, ymin=lower90, ymax=upper90, fill=inc_period), alpha=0.2) +
  
  geom_line(ebola_samples_plot_incperiod |> filter(timepoint==5 | is.na(timepoint)), mapping=aes(x=date, y=median, color=inc_period)) +
  geom_ribbon(ebola_samples_plot_incperiod |> filter(timepoint==5 | is.na(timepoint)), mapping=aes(x=date, ymin=lower50, ymax=upper50, fill=inc_period), alpha=0.5) +
  geom_ribbon(ebola_samples_plot_incperiod |> filter(timepoint==5 | is.na(timepoint)), mapping=aes(x=date, ymin=lower90, ymax=upper90, fill=inc_period), alpha=0.2) +
  
  geom_line(ebola_samples_plot_incperiod |> filter(timepoint==6 | is.na(timepoint)), mapping=aes(x=date, y=median, color=inc_period)) +
  geom_ribbon(ebola_samples_plot_incperiod |> filter(timepoint==6 | is.na(timepoint)), mapping=aes(x=date, ymin=lower50, ymax=upper50, fill=inc_period), alpha=0.5) +
  geom_ribbon(ebola_samples_plot_incperiod |> filter(timepoint==6 | is.na(timepoint)), mapping=aes(x=date, ymin=lower90, ymax=upper90, fill=inc_period), alpha=0.2) +
  
  geom_line(ebola_samples_plot_incperiod |> filter(timepoint==7 | is.na(timepoint)), mapping=aes(x=date, y=median, color=inc_period)) +
  geom_ribbon(ebola_samples_plot_incperiod |> filter(timepoint==7 | is.na(timepoint)), mapping=aes(x=date, ymin=lower50, ymax=upper50, fill=inc_period), alpha=0.5) +
  geom_ribbon(ebola_samples_plot_incperiod |> filter(timepoint==7 | is.na(timepoint)), mapping=aes(x=date, ymin=lower90, ymax=upper90, fill=inc_period), alpha=0.2) +
  
  geom_line(ebola_samples_plot_incperiod |> filter(timepoint==8 | is.na(timepoint)), mapping=aes(x=date, y=median, color=inc_period)) +
  geom_ribbon(ebola_samples_plot_incperiod |> filter(timepoint==8 | is.na(timepoint)), mapping=aes(x=date, ymin=lower50, ymax=upper50, fill=inc_period), alpha=0.5) +
  geom_ribbon(ebola_samples_plot_incperiod |> filter(timepoint==8 | is.na(timepoint)), mapping=aes(x=date, ymin=lower90, ymax=upper90, fill=inc_period), alpha=0.2) + 
  
  scale_fill_viridis(discrete=TRUE, name="Incubation period") +
  scale_colour_viridis(discrete=TRUE, name="Incubation period") +
  xlab("Date") +
  ylab("Infections") +
  facet_wrap(~inc_period, ncol=1) +
  lshtm_theme() +
  ylim(0, max(ebola_samples_plot_incperiod$upper90))

plot_grid(plot_gentime, plot_incperiod, ncol=2)

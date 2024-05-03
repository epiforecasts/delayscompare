source("~/delayscompare/scripts/01_packages.R")
source(here("R", "scenario_loop.R"))

library(scoringutils)
library(viridis)
library(RColorBrewer)

## Loading results ##

ebola_samples <- readRDS(here("results", paste0("res_ebola_samples", "2024-04-24", ".rds")))
res_ebola_id <- readRDS(here("results", paste0("res_ebola2_id", "2024-04-24", ".rds")))
ebola_sim_data <- readRDS(here("data", paste0("ebola_sim_data", "2024-04-23", ".rds")))

## CRPS ##

# Add simulated data
ebola_samples <- ebola_sim_data |> 
  filter(variable=="reported_cases") |>
  rename(true_value=value) |>
  select(-variable) |>
  right_join(ebola_samples, by="date") 

check_forecasts(ebola_samples)

# Get rid of all columns that aren't date, true_value, prediction, sample

ebola_samples <- ebola_samples |>
  select(date, true_value, prediction, sample, model, result_list, type)

# Log transform observations and predicted values

ebola_samples <- transform_forecasts(ebola_samples, fun = log_shift, offset=1, label="log")

ebola_samples |>
  check_forecasts()

ebola_scores <- ebola_samples |>
  # filtering out what I don't need to save memory
  filter(type=="forecast", scale=="log") |>
  set_forecast_unit(c("date", "model", "result_list", "type")) |>
  score() |>
  summarise_scores(by=c("model", "type", "result_list"))

## Add the info for each scenario to the plot
ebola_scores_details <- ebola_scores |>
  left_join(res_ebola_id, by="result_list")

### Heatmap by timepoint ###

# Need to turn inc_period and gen_time into factors to make sure ordering is correct
ebola_scores_details$inc_period <- factor(ebola_scores_details$inc_period, levels=c("very low", "low", "correct", "high", "very high"))
ebola_scores_details$gen_time <- factor(ebola_scores_details$gen_time, levels=c("very low", "low", "correct", "high", "very high"))

ebola_sim_data_infections <- ebola_sim_data |> 
  filter(variable=="reported_cases") 

ebola_timepoints <- ebola_sim_data_infections$date[c(1:(nrow(ebola_sim_data_infections) %/% (8*7)))*8*7]

ebola_sim_data_timepoints <- ebola_sim_data_infections |> filter(date %in% ebola_timepoints)

timeseries_ebola <- ggplot() + 
  geom_line(ebola_sim_data_infections, mapping=aes(x=date, y=value)) + 
  geom_point(ebola_sim_data_timepoints, mapping=aes(x=date,y=value), color="red") +
  xlab("Date") +
  ylab("Infections") +
  scale_x_continuous(breaks=ebola_sim_data_infections$date[grepl("-01$",ebola_sim_data_infections$date)]) +
  lshtm_theme()

timepoints_lab <- data.frame(timepoint=c(1:length(ebola_timepoints)),
                             timepoint_lab=ebola_timepoints)

ebola_scores_details <- ebola_scores_details |>
  left_join(timepoints_lab, by="timepoint")

heatmap_ebola <- 
  ggplot(ebola_scores_details, aes(x=gen_time, y=inc_period)) +
  geom_tile(aes(fill=crps)) +
  facet_wrap(~timepoint_lab, nrow=1) +
  xlab("Generation time") +
  ylab("Incubation period") +
  scale_fill_gradientn(colours = terrain.colors(50), name="CRPS for one-week forecast") +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  lshtm_theme()

heatmap_ebola_space <- plot_grid(NULL, heatmap_ebola, NULL, rel_widths=c(0.08, 1, 0.05), nrow=1)

plot_grid(timeseries_ebola, heatmap_ebola_space, ncol=1)

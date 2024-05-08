source("~/delayscompare/scripts/01_packages.R")
source(here("R", "lshtm_theme.R"))
library(scoringutils)

### Plotting each timepoint in turn ###

# Load data

res_ebola <- readRDS(here("results", paste0("res_ebola2", "2024-04-24", ".rds")))
res_ebola_id <- readRDS(here("results", paste0("res_ebola2_id", "2024-04-24", ".rds")))
ebola_sim_data <- readRDS(here("data", paste0("ebola_sim_data", "2024-04-23", ".rds")))

# New set of samples inc. estimates

ebola_samples_alltypes <- data.frame()
for(i in 1:length(res_ebola)){
  samples_scen <- res_ebola[[i]][res_ebola[[i]]$variable=="reported_cases"] |>
    mutate(model="EpiNow2")
  
  # Add ID
  samples_scen$result_list <- i
  
  # Bind to dataframe
  ebola_samples_alltypes <- rbind(ebola_samples_alltypes, samples_scen)
  }

ebola_samples_plot <- ebola_samples_alltypes |> 
  # add info
  left_join(res_ebola_id, by="result_list")

rm(ebola_samples_alltypes)

## Getting ready to plot ##

# Make factors so that ordering of facets is right
ebola_samples_plot$gen_time <- factor(ebola_samples_plot$gen_time, levels=c("very low", "low", "correct", "high", "very high"))
ebola_samples_plot$inc_period <- factor(ebola_samples_plot$inc_period, levels=c("very high", "high", "correct", "low", "very low"))

# Needed for scoring_utils functions
ebola_samples_plot <- ebola_samples_plot |>
  rename(prediction=value)

# Get quantiles
ebola_samples_plot <- sample_to_quantile(ebola_samples_plot, quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95), type = 7)

ebola_samples_plot <- ebola_samples_plot |>
  pivot_wider(names_from="quantile",
              values_from="prediction")

ebola_samples_plot <- ebola_samples_plot |>
  rename(lower90="0.05",
         upper90="0.95",
         lower50="0.25",
         upper50="0.75",
         median="0.5")

# Add observed cases
ebola_samples_plot <- ebola_sim_data |> 
  filter(variable=="reported_cases") |>
  rename(true_value=value) |>
  select(-variable) |>
  right_join(ebola_samples_plot, by="date") 

## Plot by timepoint, faceted by generation time and incubation period ##

for(i in 1:max(ebola_samples_plot$timepoint)){
  bytimepoint <- ebola_samples_plot |>
  filter(timepoint==i)
  plot_timepoint <- ggplot(bytimepoint) +
    geom_col(aes(x=date, y=true_value), alpha=0.5) +
    geom_line(aes(x=date, y=median, colour=type)) +
    geom_ribbon(aes(x=date, ymin=lower50, ymax=upper50, fill=type), alpha=0.5) +
    # geom_ribbon(aes(x=date, ymin=lower90, ymax=upper90, fill=type), alpha=0.2) +
    facet_grid(inc_period~gen_time) +
    ylab("Observed cases") +
    xlab("Date") +
    lshtm_theme() +
    scale_x_continuous(breaks= bytimepoint$date[grepl("-01$", bytimepoint$date)],
                       sec.axis = sec_axis(~ . , name = "Generation time", breaks = NULL, labels = NULL)) +
    scale_y_continuous(sec.axis = sec_axis(~ . , name = "Incubation period", breaks = NULL, labels = NULL)) 
  
  assign(paste0("plot_ebola_timepoint", i), plot_timepoint)
  ggsave(here("results", paste0("plot_ebola_timepoint", i, ".png")), plot_timepoint, width=12, height=7.65, units="in")
}

#### Plot all "correct" forecasts ####

ebola_samples_correct <- ebola_samples_plot |>
  filter(inc_period=="correct", gen_time=="correct")

plot_correct <- ggplot(ebola_samples_correct) +
  geom_col(aes(x=date, y=true_value), alpha=0.5) +
  geom_line(aes(x=date, y=median, colour=type)) +
  geom_ribbon(aes(x=date, ymin=lower50, ymax=upper50, fill=type), alpha=0.5) +
  # geom_ribbon(aes(x=date, ymin=lower90, ymax=upper90, fill=type), alpha=0.2) +
  facet_wrap(~timepoint, nrow=1, scale="free") +
  ylab("Observed cases") +
  xlab("Date") +
  lshtm_theme() +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Adding the Rt trajectory plot 

rt_ebola <- readRDS(here("data", "rt_ebola.rds"))

# Adding bars showing forecast horizon

ebola_forecasthorizons <- ebola_samples_correct |> filter(type=="forecast")

highlight_periods <- ebola_samples_correct |>
  group_by(timepoint, type) |>
  summarise(start_date=min(date),
            end_date=max(date)) |>
  filter(type=="forecast")
# Add rt_ebola values 

ebola_forecasthorizons <- rt_ebola |>
  select(date, upper_90) |>
  left_join(ebola_forecasthorizons, by="date")

ebola_forecasthorizons <- ebola_forecasthorizons |>
  select(date, upper_90, timepoint) |>
  mutate(upper_90= upper_90+0.1)

ebola_forecasthorizons$upper_90 <- ifelse(is.na(ebola_forecasthorizons$timepoint), NA, ebola_forecasthorizons$upper_90)

plot_rt_ebola <- ggplot(rt_ebola) + 
  geom_rect(data = highlight_periods, aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf), 
            fill="#619CFF",alpha = 0.5) +
  #geom_line(ebola_forecasthorizons, mapping=aes(x=date, y=upper_90), size=1.5) +
  geom_line(aes(x=date, y=R), colour="firebrick4", size=1.2) +
  geom_ribbon(aes(x=date, ymin=lower_50, ymax=upper_50), alpha=0.5, fill="firebrick4") +
  geom_ribbon(aes(x=date, ymin=lower_90, ymax=upper_90), alpha=0.3, fill="firebrick4") +
  xlab("Date") +
  ylab("Rt") +
  lshtm_theme() +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y", limits = c(min(rt_ebola$date), max(rt_ebola$date)), expand=c(0,0))

# Adding the simulated data plot 

ebola_sim_data_cases <- ebola_sim_data |> 
  filter(variable=="reported_cases") 

ebola_timepoints <- ebola_sim_data_cases$date[c(1:(nrow(ebola_sim_data_cases) %/% (8*7)))*8*7]

ebola_sim_data_timepoints <- ebola_sim_data_cases |> filter(date %in% ebola_timepoints)

timeseries_ebola <- ggplot() + 
  geom_line(ebola_sim_data_cases, mapping=aes(x=date, y=value)) + 
  geom_rect(data = highlight_periods, aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf), 
            fill = "#619CFF", alpha = 0.5) +
  #geom_point(ebola_sim_data_timepoints, mapping=aes(x=date,y=value), color="red") +
  xlab("Date") +
  ylab("Reported cases") +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y", limits = c(min(rt_ebola$date), max(rt_ebola$date)), expand=c(0,0)) +
  lshtm_theme()

## Adding to one plot ##

plots_upper <- plot_grid(plot_rt_ebola, timeseries_ebola, align="v", ncol=1)

plot_correct_space <- plot_grid(NULL, plot_correct, NULL, rel_widths=c(0.035, 1, 0.05), nrow=1)

plot_grid(plots_upper, plot_correct_space, ncol=1)



ggsave(here("results", paste0("plot_ebola_correct.png")), plot_correct, width=12, height=7.65, units="in")

#### AIC plot ####









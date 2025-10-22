library(here)
source(here("scripts", "01_packages.R"))
source(here("scripts", "02b_definedelays.R"))
source(here("R", "generate_scores_func.R"))
source("plots_baseline.R")
source(here("R", "lshtm_theme.R"))

###############################################################################################
###### Figure 1: CRPS across gen time and inc period assumptions - Rt and case forecasts ######
###############################################################################################

#### COVID-LIKE ####

## Data
covid_resim_data <- readRDS(here("data","covid_sim_data2025-10-15.rds"))

## Forecasts
covid_resim_samples <- readRDS("results/resim_covid/resim_covid_latest_all_samples2025-10-21.rds")
covid_resim_id <- readRDS("results/resim_covid/resim_covid_latest_all_id2025-10-21.rds")
covid_resim_R <- readRDS("results/resim_covid/resim_covid_latest_all_R2025-10-21.rds")

rt_covid <- rt_covid |>
  select(date, median) |>
  rename(R=median) |>
  mutate(scen=1)

## Score
covid_scores_cases <- generate_scores_cases(covid_resim_samples, covid_resim_id, covid_resim_data) |> 
  mutate(scen=1,
         rt_opts="latest",
         rt_traj="resim",
         ur="y")
covid_scores_rt <- generate_scores_rt(covid_resim_R, covid_resim_id, rt_covid) |>
  mutate(scen=1, 
         rt_opts="latest",
         rt_traj="resim",
         ur="y")

covid_caseplots <- plot_baseline_cases(covid_resim_samples,
                    covid_resim_id,
                    covid_resim_data,
                    covid_scores_cases,
                    forecast_freq = 4
                    )

#### EBOLA-LIKE ####

## Data
ebola_resim_data <- readRDS(here("data","ebola_sim_data2025-10-15.rds"))

## Forecasts
ebola_resim_samples <- readRDS("results/resim_ebola/resim_ebola_latest_all_samples2025-10-21.rds")
ebola_resim_id <- readRDS("results/resim_ebola/resim_ebola_latest_all_id2025-10-21.rds")
ebola_resim_R <- readRDS("results/resim_ebola/resim_ebola_latest_all_R2025-10-21.rds")

rt_ebola <- rt_ebola |>
  select(date, R) |>
  mutate(scen=1)

## Score
ebola_scores_cases <- generate_scores_cases(ebola_resim_samples, ebola_resim_id, ebola_resim_data) |> 
  mutate(scen=1,
         rt_opts="latest",
         rt_traj="resim",
         ur="y")
ebola_scores_rt <- generate_scores_rt(ebola_resim_R, ebola_resim_id, rt_ebola) |>
  mutate(scen=1, 
         rt_opts="latest",
         rt_traj="resim",
         ur="y")

ebola_caseplots <- plot_baseline_cases(ebola_resim_samples,
                               ebola_resim_id,
                               ebola_resim_data,
                               ebola_scores_cases,
                               forecast_freq = 4
)




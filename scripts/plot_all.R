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

# Generating plots

generate_plots("ebola", "gen_time", "2014-05-18")
generate_plots("ebola", "inc_period", "2014-05-18")

generate_plots("cholera", "gen_time", "2017-04-23")
generate_plots("cholera", "inc_period", "2017-04-23")

generate_plots("covid", "gen_time", "2021-01-01")
generate_plots("covid", "inc_period", "2021-01-01")



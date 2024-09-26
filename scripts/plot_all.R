library(here)

source(here("scripts", "01_packages.R"))
source(here("R", "funcs_plots.R"))
source(here("R", "funcs_data.R"))
source(here("R", "generate_scores_func.R"))

source(here("R", "lshtm_theme.R"))

#generate_plots("ebola")
generate_plots("cholera")
generate_plots("covid", "inc_period")

generate_plots("ebola", "gen_time")
generate_plots("ebola", "inc_period")

## Plotting the results ##

plots_list <- list()
for(i in 1:length(res_covid[[1]])){
  plot_scen <- plot_estimates(res_covid[[1]][[i]][variable=="reported_cases"],
               reported=covid_sim_data_cases,
               ylab="Reported Cases")
  plots_list[[length(plots_list)+1]] <- plot_scen
}

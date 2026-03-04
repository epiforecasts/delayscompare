for(disease in c("covid", "ebola", "cholera")){

## Collect Rt trajectories and simulated data in a list ##

disease <- disease

## Get correct start and end date for disease

startdate <- startenddates[[disease]][["startdate"]]
enddate <- startenddates[[disease]][["enddate"]]

rt_traj <- list()

# Scenarios - const Rt - low 
  rt_traj[[1]] <- data.frame(date=seq.Date(from=startdate, to=enddate, by=1),
                             R=0.8,
                             scen = "const_low")

# Scenarios - const Rt - high
  rt_traj[[2]] <- data.frame(date=seq.Date(from=startdate, to=enddate, by=1),
                             R=1.2,
                             scen = "const_high")

# Scenarios 9-12 - increasing Rt 
  rt_traj[[3]] <- data.frame(date=seq.Date(from=startdate, to=enddate, by=1),
                             R=seq(from=0.8, to=1.2, length.out=enddate-(startdate-1)),
                             scen = "inc")

# Scenarios 13-16 - decreasing Rt
  rt_traj[[4]] <- data.frame(date=seq.Date(from=startdate, to=enddate, by=1),
                             R=rev(seq(from=0.8, to=1.2, length.out=enddate-(startdate-1))),
                             scen = "dec")

## Save ##

save_latest(rt_traj, here("data"), paste0("rt_traj_list_", disease))
}

#### Rt trajectories ####

## SARS-Cov-2 ##

time_horizon <- 364
period <- 120*2

# Sinusoidal 
t <- seq(0, time_horizon, length=time_horizon)
rt_sine_covid <- 1 + 0.5*sin(2*pi*(1/period)*seq(0, time_horizon, length=time_horizon))

# Step change
change_gap <- sample(5:50, round(time_horizon/2), replace=TRUE) # Policy change times -> b/w 1 week and 3 months gap https://www.instituteforgovernment.org.uk/sites/default/files/2022-12/timeline-coronavirus-lockdown-december-2021.pdf
change_time <- vector()
for(i in 1:length(change_gap)){
  
  if(length(change_time)==0){
    change_time[i] <- change_gap[i]
  } else {
    # stop if already reached 200 days
    if((change_time[i-1] + change_gap[i])>time_horizon){
      break }
    
    change_time[i] <- change_time[i-1] + change_gap[i]
  }

}

change_dir <- sample(c("up", "down"), length(change_time), replace="TRUE")

rt_step_covid <- c(1)
for(i in 1:length(change_time)){
  rt_step_covid[i+1]=ifelse(change_dir[i]=="up",
                      runif(1, 1, 4)*rt_step_covid[i],
                      runif(1, 0.25, 1)*rt_step_covid[i])
  # cap
  rt_step_covid[i+1]=ifelse(rt_step_covid[i+1]>1.5,
                            1.5,
                            rt_step_covid[i+1])
}

## As dataframe input for simulate_infections

rt_sine_covid <- data.frame(R=rt_sine_covid,
                            date=seq(as.Date("2024-01-01"), by=1, len=364))

rt_step_covid <- data.frame(R=rt_step_covid,
                            date=as.Date("2024-01-01") + c(0, change_time))

## Ebola-like ##

# From data

def <- estimate_infections(ebola_confirmed,
                           generation_time = generation_time_opts(ebola_gen_time),
                           delays = delay_opts(ebola_inc_period + ebola_reporting_delay),
                           obs = obs_opts("poisson", 0.83))


                            
                            

source(here("R", "funcs_rtraj.R"))

## Ebola Rt trajectory

## create temporary directory
tmpdir <- tempdir()
## download data file from `ebola.forecast.wa.sl` repository and save in
## temporary directory
download.file(
  "https://github.com/sbfnk/ebola.forecast.wa.sl/raw/master/data/ebola_wa.rdata",
  file.path(tmpdir, "ebola_wa.rdata")
)
## load data (this creates the `ebola_wa` object)
load(file.path(tmpdir, "ebola_wa.rdata"))

## rename column for EpiNow2
ebola_wa <- ebola_wa |>
  rename(confirm = incidence)

## Use EpiNow2 to estimate Rt
## uses the delay distributions defined in `05_simulate_delay.R`
ebola_epinow <- epinow(
  ## use Ebola data set
  ebola_wa,
  ## Ebola generation time
  generation_time = generation_time_opts(ebola_gen_time),
  ## Ebola delay
  delay = delay_opts(combined_delay_ebola),
  ## assume 83% of infections are observed
  ## accumulate incidence over missing days (as data is weekly)
  obs = obs_opts(scale = 0.83, na = "accumulate"),
  ## generate 1000 samples
  stan = stan_opts(chains = 2, cores = 2, samples = 1000),
  ## when there is no data, revert to mean Rt
  ## (fine as we're not doing real-time inference and faster than the default)
  rt = rt_opts(gp_on = "R0")
)

rt_ebola <- ebola_epinow$estimates$samples |>
  filter(variable == "R") |>
  group_by(date) |>
  summarise(R=median(value),
            lower_50=quantile(value, 0.25),
            upper_50=quantile(value, 0.75),
            lower_90=quantile(value, 0.1),
            upper_90=quantile(value, 0.9))
            
saveRDS(rt_ebola, here("data", "rt_ebola.rds"))

#### SARS-CoV-2 ####

## SARS-CoV-2 Rt trajectory

rt_covid <- get_covid19_nowcasts()

# Keep UK and up to end of 2021 only
rt_covid <- rt_covid |>
  filter(country=="United Kingdom") |>
  filter(date<="2021-12-31")

saveRDS(rt_covid, here("data", "rt_covid.rds"))
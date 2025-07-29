
library(here)
source(here("scripts", "01_packages.R"))

# Load run times
# Read the sacct output (skip headers if repeated)
lines <- readLines("results/slurm_jobs_output.txt")
# Keep only the first header
header_line <- lines[1]
data_lines <- lines[-2]
data_lines <- data_lines[!grepl("^\\s*JobID", data_lines)]  # remove repeated headers
writeLines(c(header_line, data_lines), "cleaned_slurm_output.txt")

# Read cleaned data
slurm_data <- read_table("cleaned_slurm_output.txt")

# Function to parse Elapsed time to seconds
parse_elapsed <- function(x) {
  # Handle NA or empty strings
  if (is.na(x) || x == "") return(NA)
  
  # Check for days (format: DD-HH:MM:SS)
  if (grepl("-", x)) {
    parts <- unlist(strsplit(x, "-"))
    days <- as.numeric(parts[1])
    time_parts <- unlist(strsplit(parts[2], ":"))
  } else {
    days <- 0
    time_parts <- unlist(strsplit(x, ":"))
  }
  
  # Pad time parts if needed (e.g., MM:SS only)
  time_parts <- as.numeric(time_parts)
  len <- length(time_parts)
  
  if (len == 3) {
    h <- time_parts[1]
    m <- time_parts[2]
    s <- time_parts[3]
  } else if (len == 2) {
    h <- 0
    m <- time_parts[1]
    s <- time_parts[2]
  } else {
    return(NA)  # Unexpected format
  }
  
  total_seconds <- days * 86400 + h * 3600 + m * 60 + s
  return(total_seconds)
}


# Add ElapsedSeconds and parse datetime columns
slurm_data <- slurm_data |>
  mutate(
    ElapsedSeconds = sapply(Elapsed, parse_elapsed)
  )

# Keep covidscen+ jobs only
slurm_data <- slurm_data |>
  filter(JobName=="covidscen+") |>
  select(JobID, JobName, Elapsed, ElapsedSeconds)

# Rename columns - extract gen_time from JobID - last digit of JobID
slurm_data <- slurm_data |>
  mutate(
    gen_time = as.numeric(substr(JobID, nchar(JobID), nchar(JobID))),
    ElapsedHours = as.numeric(ElapsedSeconds/(60*60)),
    JobID = as.numeric(substr(JobID,1,7))
  ) 

# Add scenario names

scens <- data.frame(JobID=c(4227959, 
                            4228046,
                            4228073,
                            4228104),
                    scen=c("Rt=increasing","Rt=decreasing"))

slurm_data <- slurm_data |>
  left_join(scens, by = "JobID")

runtimeplot <- ggplot() + geom_col(data = slurm_data |> filter(!is.na(scen)), aes(x = factor(gen_time), y = ElapsedHours)) +
  labs(x = "Generation interval", y = "Elapsed Time (Hours)") +
  theme_minimal() +
  facet_wrap(~ scen, scales = "free_y", ncol = 2) +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "bottom")


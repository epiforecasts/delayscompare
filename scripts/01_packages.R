pkgs <- c("EpiNow2", 
          "incidence2",
          "readxl", 
          "dplyr",
          "tidyr",
          "purrr",
          "here",
          "ggplot2",
          "gh",
          "readr",
          "cowplot",
          "scoringutils",
          "viridis",
          "RColorBrewer")

# install.packages(setdiff(pkgs, rownames(installed.packages())), repos = c("https://epiforecasts.r-universe.dev", getOption("repos")))

lapply(pkgs, library, character.only=TRUE)

here::here("C:/Users/CiaraMcCarthy/delayscompare")

options(mc.cores = 4)

pkgs <- c("EpiNow2", 
          "incidence2",
          "readxl", 
          "dplyr",
          "tidyr",
          "here",
          "ggplot2",
          "gh",
          "readr",
          "cowplot"
              )
install.packages(setdiff(pkgs, rownames(installed.packages()))) 

#library("rstan")
#library("posterior")
#library("tidyr")
#library("fitdistrplus")

lapply(pkgs, library, character.only=TRUE)

here::here("C:/Users/CiaraMcCarthy/delayscompare")

options(mc.cores = 4)

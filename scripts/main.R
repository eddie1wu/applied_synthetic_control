# Run all scripts
# Ctrl + L to clear console
rm(list=ls())

### Set up ---------------------------------------------------------------------
clean_data <- FALSE
dep_var <- "RHHGDI_INDEX"  # Either RGDP_INDEX or RHHGDI_INDEX


### Define paths ---------------------------------------------------------------

home_path <- getwd()
data_path <- file.path(home_path, "../data/")
out_path <- file.path(home_path, "../output")


### Preprocess data and import functions ---------------------------------------
if (clean_data) {
  source("data.R")
}

source("utils.R")


### Run Abadie synthetic control -----------------------------------------------
source("run_sc.R")


### Run augmented synthetic control --------------------------------------------
source("run_augsc.R")





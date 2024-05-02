# Run synthetic control
# Ctrl + L to clear console

library(Synth)
library(tidyverse)
library(ggplot2)

# Load data
df <- read.csv(file.path(data_path, "clean_data.csv"))

################################################################################

# Set Dependent variable here:
# dep_var <- "RGDP_INDEX"

################################################################################

### Define plot labels and file names ------------------------------------------
if (dep_var == "RGDP_INDEX") {
  title_suffix <- "real GDP"
  file_prefix <- "gdp"
} else if (dep_var == "RHHGDI_INDEX") {
  title_suffix <- "real disposable income"
  file_prefix <- "gdi"
}


### Preprocessing --------------------------------------------------------------
# Drop countries with missing covariates, 
# and drop net transfer
df_sc <- df %>%
  filter(yearqtr != 2023.75,
         !( Country %in% c("Colombia", "Estonia", "Iceland", "Israel", "Korea",
                           "Latvia", "Lithuania", "Luxembourg", "Mexico",
                           "New Zealand", "Slovak Republic", "Switzerland") )) %>%
  filter(!( Country %in% c("Chile", "Canada", "Japan", "Norway") )) %>%
  select(-net_transfer) %>%
  mutate(Country = ifelse(Country == "United Kingdom", "UK", Country))

# Fill missing obs with pre-treatment period average
drop_name <- c(dep_var, "Country", "TIME", "yearqtr", "time_var")
df_sc <- fill_df(df_sc, drop_name, 2007.00, 2016.25)

# Save data
write.csv(df_sc, file.path(data_path, "sc_data.csv"), row.names = FALSE)

### Define variables -----------------------------------------------------------
# Numerical country ID
df_sc$country_id <- as.numeric(as.factor(df_sc$Country))

# Treated and control ID
treated_id <- 19
control_id <- setdiff( unique(df_sc$country_id), treated_id ) 

# Predictors
predictors <- c(dep_var, "UNEMPRATE", "final_consumption", "debt_ratio",
                "confidence", "networth", "HHSAV", "LAB_UR6", "cpi_ref2015",
                "stock_ref2015", "short_ir")

# Special predictors
special_preds <- list(
  list(dep_var, 4, "mean"),
  list(dep_var, 8, "mean"),
  list(dep_var, 12, "mean"),
  list(dep_var, 16, "mean"),
  list(dep_var, 20, "mean"),
  list(dep_var, 24, "mean"),
  list(dep_var, 28, "mean"),
  list(dep_var, 32, "mean"),
  list(dep_var, 36, "mean")
)

# Define dates 
dates <- df_sc[df_sc$Country == "UK", "TIME"]
dates <- as.yearqtr(format(dates), "%Y-Q%q")
event_time <- as.yearqtr(format("2016-Q3"), "%Y-Q%q")
  
# Convert the data type into dataframe
df_sc <- as.data.frame(df_sc)  

# List of all countries, excluding UK
country_ex_uk <- setdiff( unique(df_sc$Country), "UK" )


### Run synthetic control ------------------------------------------------------
if (TRUE) {
# Data prep for synth()
dataprep.out<-
  dataprep(
    foo = df_sc,
    predictors = predictors,
    predictors.op = "mean",
    time.predictors.prior = 1:38,
    special.predictors = special_preds,
    dependent = dep_var,
    unit.variable = "country_id",
    unit.names.variable = "Country",
    time.variable = "time_var",
    treatment.identifier = treated_id,
    controls.identifier = control_id,
    time.optimize.ssr = 1:38,
    time.plot = 1:67
  )

# Compute the optimal weights
synth.out <- synth(dataprep.out)
}

# plot paths (treated vs synthetic)
path.plot(dataprep.res = dataprep.out,synth.res = synth.out)

# plot gaps (treated - synthetic)
gaps.plot(dataprep.res = dataprep.out,synth.res = synth.out)

# Compute the gaps manually for plotting
UK_gaps <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)


### My beautiful plots ---------------------------------------------------------
plot_path(synth.out, dataprep.out,
          time = dates,
          event_time = event_time,
          breaks = seq(60, 160, by = 20),
          limits = c(60, 160),
          title = "UK (Solid) vs Synthetic (Dashed)",
          x_label = "Date",
          y_label = paste0("Per capita ", title_suffix),
          file_name = paste0("../output/", file_prefix, "_path.png"))

plot_gap(synth.out, dataprep.out,
         time = dates,
         event_time = event_time,
         breaks = seq(-30, 30, by = 10),
         limits = c(-30, 30),
         title = "Gaps: Treated - Synthetic",
         x_label = "Date",
         y_label = paste0("Gaps in per capita ", title_suffix),
         file_name = paste0("../output/", file_prefix, "_gap.png"))

plot_weights(synth.out, country_ex_uk,
             x_label = "Country",
             y_label = "Weight",
             file_name = paste0("../output/", file_prefix, "_weights.png"))


### Inference by placebo studies -----------------------------------------------
# Define variables
all_id <- unique(df_sc$country_id)
out <- c()

# Placebo studies
if (FALSE) {
for (i in all_id) {
  treated <- i
  control <- setdiff(all_id, i)
  
  dataprep.out<-
    dataprep(
      foo = df_sc,
      predictors = predictors,
      predictors.op = "mean",
      time.predictors.prior = 1:38,
      special.predictors = special_preds,
      dependent = dep_var,
      unit.variable = "country_id",
      unit.names.variable = "Country",
      time.variable = "time_var",
      treatment.identifier = treated,
      controls.identifier = control,
      time.optimize.ssr = 1:38,
      time.plot = 1:67
    )
  
  synth.out <- synth(dataprep.out)
  gaps <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
  out <- cbind(out, gaps)
}
saveRDS(out, file = paste0("../output/", file_prefix, "_placebo.rds"))
}

# Reload placebo data
out <- readRDS(paste0("../output/", file_prefix, "_placebo.rds"))
out <- as.data.frame(out)

# Rename columns
names(out) <- unique(df_sc$Country)

# Define date
out$dates <- dates

# Plot placebo gaps
plot_placebo(out, dep_var, paste0("../output/", file_prefix, "_inference.png"))





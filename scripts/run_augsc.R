# Run augmented synthetic control
# Ctrl + L to clear console

library(augsynth)
library(tidyverse)
library(zoo)

# Load data
df <- read.csv(file.path(data_path, "clean_data.csv"))

################################################################################

# Set Dependent variable here:
# dep_var <- "RHHGDI_INDEX"

################################################################################

### Define plot labels and file names ------------------------------------------
if (dep_var == "RGDP_INDEX") {
  file_prefix <- "gdp"
} else if (dep_var == "RHHGDI_INDEX") {
  file_prefix <- "gdi"
}


### Preprocessing --------------------------------------------------------------
# Drop countries with missing covariates, 
# and drop net transfer
df_asc <- df %>%
  filter(yearqtr != 2023.75,
         !( Country %in% c("Colombia", "Estonia", "Iceland", "Israel", "Korea",
                           "Latvia", "Lithuania", "Luxembourg", "Mexico",
                           "New Zealand", "Slovak Republic", "Switzerland") )) %>%
  filter(!( Country %in% c("Chile", "Canada", "Japan", "Norway") )) %>%
  select(-net_transfer) %>%
  mutate(Country = ifelse(Country == "United Kingdom", "UK", Country),
         treated = ifelse( (Country == "UK" & yearqtr >= 2016.50), 1, 0),
         country_id = as.numeric(as.factor(Country)))

# Fill missing obs with pre-treatment period average
drop_name <- c(dep_var, "Country", "TIME", "yearqtr", "time_var", "treated")
df_asc <- fill_df(df_asc, drop_name, 2007.00, 2016.25)

# Convert the data type into dataframe only
df_asc <- as.data.frame(df_asc)


### Augmented SC ---------------------------------------------------------------

# Without covariate
formula <- as.formula(paste0(dep_var, " ~ treated"))
asyn <- augsynth(formula, country_id, yearqtr, df_asc,
                progfunc = "Ridge", scm = T)
plot(asyn, cv = T)


# With covariates
formula <- as.formula(paste0(dep_var,
                             " ~ treated | ",
                             dep_var,
                             " + UNEMPRATE + final_consumption + debt_ratio + confidence",
                             " + networth + HHSAV + LAB_UR6 + cpi_ref2015 + stock_ref2015 + short_ir"))

get_augsc_results(formula, df_asc,
                  weights_file_name = "../output/gdp_augsc_weights.png",
                  path_breaks = seq(60, 160, by = 20),
                  path_limits = c(60, 160), 
                  path_y_label = "Real GDP Per Capita",
                  path_file_name = "../output/gdp_augsc_path.png",
                  txt_file_name = "../output/gdp_augsc.txt",
                  gaps_file_name = "../output/gdp_augsc_gaps.png")
  


#   
# covsyn <- augsynth(formula,
#                    country_id,
#                    yearqtr,
#                    df_asc,
#                    progfunc = "Ridge",
#                    scm = T)
# # summary(covsyn)
# png(paste0("../output/", file_prefix, "_augsc.png"),
#     width = 800, height = 600)
# temp <- plot(covsyn)
# print(temp)
# dev.off()
# 
# 
# # Residual
# covsyn <- augsynth(formula,
#                    country_id,
#                    yearqtr,
#                    df_asc,
#                    progfunc = "Ridge",
#                    scm = T,
#                    lambda = asyn$lambda,
#                    residualize = T)
# # summary(covsyn)
# png(paste0("../output/", file_prefix, "_augsc_resid.png"),
#     width = 800, height = 600)
# temp <- plot(covsyn)
# print(temp)
# dev.off()




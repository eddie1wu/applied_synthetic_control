# Clean up raw data
# Ctrl + L to clear console

library(tidyverse)
library(zoo)

### household variables --------------------------------------------------------
household <- read.csv(file.path(data_path, "HH_DASH_20032024031910148.csv"))

household <- household %>%
  subset(select = c(Country, INDICATOR, TIME, Value)) %>%
  pivot_wider(names_from = INDICATOR, values_from = Value) %>%
  rename(final_consumption = HHFINCON,
         net_transfer = HHNT,
         debt_ratio = ENDT,
         confidence = COCONF,
         networth = SBF90GDI,
         final_cons_index = HHFC_INDEX) %>%
  subset(!( Country %in% c("Euro area",
                           "European Union – 27 countries (from 01/02/2020)",
                           "G7", "OECD - Total") ))


### cpi ------------------------------------------------------------------------
cpi <- read.csv(file.path(data_path, "cpi_total.csv"))

cpi <- cpi %>%
  subset(select = c(Country, TIME, Value)) %>%
  rename(cpi_ref2015 = Value)


### Share price ----------------------------------------------------------------
stock <- read.csv(file.path(data_path, "share_price.csv"))

stock <- stock %>%
  subset(select = c(Country, TIME, Value)) %>%
  rename(stock_ref2015 = Value)


### interest rate --------------------------------------------------------------
ir <- read.csv(file.path(data_path, "short_term_rate.csv"))

ir <- ir %>%
  subset(select = c(Country, TIME, Value)) %>%
  rename(short_ir = Value)


### Merge all data -------------------------------------------------------------
df_list <- list(household, cpi, stock, ir)
df_wide <- df_list %>%
  reduce(full_join, by=c("Country", "TIME")) %>%
  mutate(yearqtr = as.numeric( as.yearqtr(format(TIME), "%Y-Q%q") ),
         time_var = (yearqtr - 2007)/0.25 + 1) %>%
  subset(!( Country %in% c("Euro area (19 countries)", "Türkiye",
                     "China (People's Republic of)") ))


### Drop countries -------------------------------------------------------------
# Count missing obs by country
out <- count_na(df_wide, Country)

# Drop countries with too many missing variables
df_wide <- df_wide %>%
  subset(!( Country %in% c("Brazil", "Bulgaria", "Costa Rica", "Croatia", "India",
                           "Indonesia", "Romania", "Russia", "Saudi Arabia",
                           "South Africa") )) %>%
  arrange(Country, yearqtr)

# Save data
write.csv(df_wide, file.path(data_path, "clean_data.csv"), row.names = FALSE)



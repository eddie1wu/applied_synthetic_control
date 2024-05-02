
### Define functions to be used in other scripts

check_balance_panel <- function(df, ID) {
  out <- df %>%
    group_by({{ID}}) %>%
    summarise(observations = n())
  
  return(out)
}


count_na <- function(df, ID) {
  # Counts the number of missing obs of each variable, for each ID
  
  out <- df %>%
    group_by({{ID}}) %>%
    summarise(across( everything(), ~sum(is.na(.)) ))
  
  return(out)
}


get_na_date <- function(df, country_name, var) {
  # Get the date of missing obs of a variable, for a country
  
  out <- df %>%
    filter(Country == country_name, is.na( {{var}} )) %>%
    select(Country, yearqtr, time_var, {{var}})
  print(out)
  
  return(out)
}


fill_na <- function(df, var_string, start, end) {
  # Fill pre-treatment period missing covariates by pre-treatment period average
  
  var <- as.symbol(var_string)
  temp <- df %>%
    group_by(Country) %>%
    filter(yearqtr >= start & yearqtr <= end) %>%
    mutate(temp_var = ifelse( is.na( {{var}} ), mean( {{var}}, na.rm = TRUE ), {{var}} ) ) %>%
    select(Country, temp_var)
  
  df[ (df$yearqtr >= start & df$yearqtr <= end), var_string ] <- temp$temp_var
  
  return(df)
}


fill_df <- function(df, drop_list, start, end) {
  # Fill every column of df
  
  column_name <- names(df)
  column_name <- column_name[!(column_name %in% drop_list)]
  for (col in column_name) {
    df <- fill_na(df, col, start, end)
  }
  
  return(df)
}


plot_path <- function(synth.out, dataprep.out, time, event_time,
                      breaks, limits, title, x_label, y_label, file_name) {
  # Plot paths
  
  synthetic_series <- dataprep.out$Y0plot %*% synth.out$solution.w
  
  ggplot() + 
    geom_line(aes(x = time, y = dataprep.out$Y1plot)) + 
    geom_line(aes(x = time, y = synthetic_series), linetype = "dashed") +
    geom_vline(xintercept = event_time, linetype = "dashed", color = "red") +
    scale_y_continuous(breaks = breaks, limits = limits) +
    labs(title = title,
         x = x_label,
         y = y_label) +
    theme_minimal()
  ggsave(file_name)
}


plot_gap <- function(synth.out, dataprep.out, time, event_time,
                     breaks, limits, title, x_label, y_label, file_name) {
  # Plot gaps
  
  gaps <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
  
  ggplot() +
    geom_line(aes(x = time, y = gaps)) + 
    geom_vline(xintercept = event_time, linetype = "dashed", color = "red") + 
    geom_hline(yintercept = 0, linetype = "solid", color = "grey") + 
    scale_y_continuous(breaks = breaks, limits = limits) +
    labs(title = title,
         x = x_label,
         y = y_label) +
    theme_minimal()
  ggsave(file_name)
}


plot_weights <- function(synth.out, country_list,
                         x_label, y_label, file_name) {
  # Plot weights
  
  temp <- data.frame(
    category = country_list,
    w.weight = synth.out$solution.w,
    abs_weights <- abs(synth.out$solution.w)
  )
  
  temp <- temp %>%
    arrange(-abs_weights) %>%
    # slice(1:15) %>%
    arrange(w.weight) %>%
    mutate(category = factor(category, levels = unique(category)))
  
  row.names(temp) <- NULL
  
  ggplot(temp, aes(x = w.weight, y = category)) +
    geom_bar(stat = "identity") +
    labs(x = x_label, y = y_label) +
    theme_minimal()
  ggsave(file_name)
}


plot_placebo <- function(df, dep_var, file_name) {
  # Placebo plot
  
  if (dep_var == "RGDP_INDEX") {
    df <- df %>%
      subset(select = -c(Greece, Poland, Ireland))
  } else if (dep_var == "RHHGDI_INDEX") {
    df <- df %>%
      subset(select = -c(Greece, Hungary, Poland))
  }
  
  df_long <- df %>%
    pivot_longer(!dates, names_to = "variable", values_to = "value")

  ggplot() +
    geom_line(data = df_long, aes(x = dates, y = value, group = variable), color = "grey") + 
    geom_line(data = df, aes(x = dates, y = UK), color = "red") +
    labs(title = "Inference by placebo study",
         x = "Date",
         y = "Treatment effect") + 
    theme_minimal()
  ggsave(file_name)
}


get_augsc_results <- function(formula, df, weights_file_name, 
                              path_breaks, path_limits, path_y_label, path_file_name,
                              txt_file_name, gaps_file_name) {
  # Obtain the table in text file, the gap plot and the weights plot
  
  # Run augmented SC
  covsyn <- augsynth(formula,
                     country_id,
                     yearqtr,
                     df,
                     progfunc = "Ridge",
                     scm = T)
  
  # Plot weights using ggplot2
  country_list <- setdiff(unique(df$Country), "UK")
  temp <- data.frame(
    category = country_list,
    weights = covsyn$weights,
    abs_weights <- abs(covsyn$weights)
  )
  
  temp <- temp %>%
    arrange(-abs_weights) %>%
    # slice(1:15) %>%
    arrange(weights) %>%
    mutate(category = factor(category, levels = unique(category)))
  
  row.names(temp) <- NULL
  
  ggplot(temp, aes(x = weights, y = category)) +
    geom_bar(stat = "identity") +
    labs(x = "Weights", y = "Countries in the control group") +
    theme_minimal()
  ggsave(weights_file_name)
  
  
  # Plot path using ggplot2
  Y1 <- covsyn$data$synth_data$Y1plot
  Y0 <- covsyn$data$synth_data$Y0plot
  synthetic_series <- Y0 %*% covsyn$weights
  
  # dates <- df[df$Country == "UK", "Year"]
  # event_time <- 2017
  # Define dates 
  dates <- df[df$Country == "UK", "TIME"]
  dates <- as.yearqtr(format(dates), "%Y-Q%q")
  event_time <- as.yearqtr(format("2016-Q3"), "%Y-Q%q")
  
  ggplot() + 
    geom_line(aes(x = dates, y = Y1)) + 
    geom_line(aes(x = dates, y = synthetic_series), linetype = "dashed") +
    geom_vline(xintercept = event_time, linetype = "dashed", color = "red") +
    scale_y_continuous(breaks = path_breaks, limits = path_limits) +
    labs(title = "UK (Solid) vs Synthetic (Dashed)",
         x = "Year",
         y = path_y_label) +
    theme_minimal()
  ggsave(path_file_name)
  
  
  # Save output to txt file
  sink(txt_file_name)
  print(summary( covsyn ))
  sink()  # returns output to the console
  
  
  # Plot gaps
  png(gaps_file_name,
      width = 800, height = 600)
  temp <- plot(covsyn)
  print(temp)
  dev.off()
}







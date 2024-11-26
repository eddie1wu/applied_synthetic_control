# Synthetic Control with Empirical Applications

This repository hosts my codes for the project on evaluating the economic impact of the Brexit referendum using synthetic control.

Here is the abstract: This research uses various synthetic control methods, including the augmented synthetic control and the synthetic difference-in-differences, to evaluate the impacts of the Brexit referendum which took place in 2016Q2 on the real GDP per capita and real gross disposable income per capita in the UK. I examine the short and medium term impacts till 2023Q3, and estimate that the Brexit referendum has caused a persistent drop in real GDP since 2016Q3, which accumulates to a 10% gap by 2023Q3. The same goes for real per capita gross disposable income, which amounts to a 16-22% gap by 2023Q3. By comparing the different methods, I find that the original synthetic control estimates are greater than those of the augmented synthetic control in magnitudes, although the assumptions for the original synthetic control are largely satisfied and there is no need to extrapolate beyond the convex hull of the control countries.

I collected my data from the OECD database. I use a panel dataset of 20 countries, from 2007Q1 to 2023Q3.

## File structure

The `output` folder contains various synthetic control figures and analysis results.

The `scripts` folder contains all the R scripts used for this analysis.

[`clean_data.R`](scripts/clean_data.R) cleans up raw data and saves a csv containing the clean data which is ready for analysis.

[`main.R`](scripts/main.R) the master file for running the entire analysis.

[`run_augsc.R`](scripts/run_augsc.R) runs the augmented synthetic control and outputs the relevant figures.

[`run_sc.R`](scripts/run_sc.R) runs the ordinary synthetic control and outputs the relevant figures.

[`utils.R`](scripts/utils.R) contains the utility functions used throughout the analysis.

[`sdid.do`](scripts/sdid.do) this is the Stata .do file for running the synthetic difference-in-differences.

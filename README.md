# Delayed introduction, transmission variation, and susceptible dynamics explain spatial asynchrony during Korea's large pertussis outbreak

[![DOI](https://zenodo.org/badge/1092955605.svg)](https://doi.org/10.5281/zenodo.21640529)

------

This repository contains all code and data used for the analysis.

- `R`: contains functions for simulating deterministic and stochastic SEIR models
  - `seir.R`: functions for deterministic SEIR simulations
  - `seir_stoch.R`: functions for stochastic SEIR simulations
- `analysis_R_t`: contains R scripts for estimating the effective reproduction number, R(t)
  - `analysis_R_t.R`: estimates R(t) using Poisson smoothing
  - `analysis_R_t_nb.R`: estimates R(t) using negative binomial smoothing
  - `analysis_R_t_nb_db.R`: sensitivity analysis using negative binomial smoothing and a longer generation interval
- `analysis_S0`: contains R scripts for summarizing regional estimates of the initial susceptible fraction, basic reproduction number, initial infected fraction, and reporting rate
- `analysis_stoch`: contains R scripts for evaluating the effects of demographic stochasticity and spatial variation in the basic reproduction number and introduction timing
- `analysis_synchrony`: contains R scripts for calculating spatial synchrony in reported cases and R(t)
- `compare_model`: contains an R script for comparing models with spatial variation in the basic reproduction number or initial susceptible fraction
- `data`: contains raw pertussis case time series, population sizes, age structure, and municipality coordinates
- `data_pdf`: contains raw vaccination data and associated spreadsheet files
- `data_processed`: contains R scripts for preprocessing the case, population, age-structure, and vaccination data
- `doc`: contains LaTeX files for the manuscript and supplementary information
- `figure`: contains R scripts for plotting the main and supplementary figures
  - `figure_data_spatial.R`: generates Figure 1
  - `figure_R_t_nb.R`: generates Figure 2
  - `figure_stanfit_region_R0.R`: generates Figure 3 and Figure S2
  - `figure_summary.R`: generates Figure 4
  - `figure_stoch.R`: generates Figure 5
  - `figure_data_region.R`: generates Figure S1
  - `figure_stanfit_region_R0_compare.R`: generates Figure S3
  - `figure_stanfit_all_R0_delta.R`: generates Figure S4
  - `figure_stanfit_region.R`: generates Figure S5
  - `figure_compare.R`: generates Figures S6 and S9
  - `figure_stanfit_all_delta.R`: generates Figure S7
  - `figure_stanfit_all_S0.R`: generates Figure S8
  - `figure_R_t_nb_db.R`: generates Figure S10
  - `figure_stanfit_region_R0_nb.R`: generates Figure S11
- `script`: contains a shared R script for loading and organizing processed data
- `simulate`: contains R scripts for simulating the fitted SEIR models across different initial conditions
  - `simulate_seir_stanfit.R`: explores variation in the initial susceptible and infected fractions
  - `simulate_seir_stanfit_R0.R`: explores variation in the basic reproduction number and initial infected fraction
- `stanfit`: contains R scripts for fitting SEIR models using Stan
  - `stanfit_region_R0.R`: fits the primary model with municipality-specific basic reproduction numbers
  - `stanfit_region.R`: fits the alternative model with municipality-specific initial susceptible fractions
  - `stanfit_all_R0_delta.R`: validates the primary model using municipalities with fewer reported cases
  - `stanfit_all_delta.R`: validates the alternative model using municipalities with fewer reported cases
  - `stanfit_region_R0_nb.R`: fits the primary model using negative binomial observation error
- `stanmodel`: contains Stan scripts for the deterministic SEIR models

------

- R scripts in `data_processed` can be used to regenerate the processed data files from the raw data.
- The main scripts in `stanfit` and the scripts in `analysis_R_t` should be run first to generate model fits and R(t) estimates.
- The validation scripts in `stanfit` should be run after their corresponding primary models have been fitted.
- R scripts in `simulate` can be run after the corresponding Stan models have been fitted.
- R scripts in `analysis_S0`, `analysis_stoch`, `analysis_synchrony`, and `compare_model` can then be used to generate summaries of the fitted models and subsequent analyses.
- R scripts in `figure` should be run after the required models and analyses have been completed.

------

All code was run on an M2 MacBook Pro (2023).

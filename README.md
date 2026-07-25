# Delayed introduction, transmission variation, and susceptible dynamics explain spatial asynchrony during Korea's large pertussis outbreak

------

This repository contains all code and data used for the analysis.

* `R`: contains an R script for simulating deterministic and stochastic SEIR models
* `analysis_R_t`: contains an R script for estimating Rt
* `analysis_S0`: contains an R script for summarizing parameter estimates
* `analysis_stoch`: contains an R script for simulating stochastic models
* `analysis_synchrony`: contains an R script for calculating spatial synchrony
* `compare_model`: contains an R script for model comparison
* `data`: contains time series data for pertussis cases, population size data, and coordinate data
* `data_pdf`: contains a pdf file for the raw vaccination data
* `data_processed`: contains R script for preprocessing data sets
* `doc`: contains latex files for the manuscript
* `script`: contains an R script for processing data
* `simulate`: contains R scripts for simulating the fitted SEIR model
* `stanfit`: contains R scripts for fitting SEIR model using Stan
* `stanmodel`: constrains Stan scripts for deterministic models 

------

* R scripts in `stanfit` and `analysis_R_t` folders can be run independently as standalone files; these files need to be run first to generate rda files for model fits.
* R scripts in `analysis_S0`, `analysis_stoch`, `analysis_synchrony`, and `compare_model` folders can be run after all stan models have been fitted and Rt analyses have been performed. These scripts will generate rda files that contain a summary of analyses of fitted models.
* R scripts in figure folder need to be run after all models have been fitted and analyzed.

------

All code was run on M2 MacBook Pro, 2023.

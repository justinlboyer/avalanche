# avalanche
Project in R on avalanches in Wasatch

If you are interested in contributing please do!
## Contents 
 + Cookbook.tex - documentation of the data cleaning process
 + ModelRegBiNumAv.R - Builds all models for ensemble forecast
 + EnsembleAllModels.R - Creates ensemble forecast
    -Requires:
      +ensembleBestLam.R
      +ensembleRRFit.rda
      +ensembleLasBestLam.R
      +ensembleLasso.rda
      +ensembleLinear.rda
      +ensembleLogistic.rda
 + EnsembleRR_app.R - Script for a shiny app that loads models, then based on user input reports the probability of an avalanche occuring
     -Requires:
      +ensembleBestLam.R
      +ensembleRRFit.rda
      +ensembleLasBestLam.R
      +ensembleLasso.rda
      +ensembleLinear.rda
      +ensembleLogistic.rda
 + EnsembleForecastAccuracy.R - The accuracy of the ensemble forecast models
 
 ###Required Files:
 + loadData.R - script loads data set, first cleaning
 + subsetCCB.R - subsets data by caught, cleaned and buried only used in data analysis
 + subsetDates.R - subsets and oraganizes data by dates (required by pretty much everything)
 + subsetIK.R - subsets data by injured and killed, only used in data analysis
 + subsetNumAv_Dates.R - subsets and organizes the data by number of avalanches that occured (required by all models and apps)
 + windLouisMeadow080716.csv - dataset containing wind information, procured August 7, 2016
 + altaGuardWeather080716.csv - dataset of weather from alta guard procured August 7, 2016
 + avalanches_raw.csv - dataset containing all avalanche data procured August 7, 2016
 + dataAnalysis.R - somewhat deprecated, but rough premliminary analysis of data
 + downloadCurrentWU.R - a script to pull down the weather conditions at alta, not started
 + avalanche.Rproj - workspace, might be usefull?

 + ModelReg.R - script containing different regession models
 + NOAA_DATA_documentation.pdf - self explantory
 
 + checkAccRidReg.R - script for checking accuracy of ridge regession model (needs rrfun.R)
 + rrfun.R - script needed by checkACCRidReg.R, builds model from training set
 
 ###Deprecated Files:
 + ~~ZeAvEqAv.R - script that compares different regression models when 3/4 of the observations contain 0 avalanches (this was a decision on my part, conservatively arguing that 3 out of 4 days we should observe at least one avalanche.  Need to come back to and use poisson process to model more accurately)'''~~
 + ~~app.R - shiny app that uses ridge regession model with all observations~~
 + ~~mostZrr.bestlam.R - best lambda from ZeAvEqAv.R (some0Av_app.R needs this file)~~
 + ~~mostZrr.fit_PSSTWA.rda - model from ZeAvEqAv.R (some0Av_app.R needs this file)~~
 + ~~some0Av_app.R - shiny app that predicts number of avalanches: currently running at justinlboyer.shinyapps.io/avalanche/~~

 
 ##Building model
 - Required files: 
 + avalanches_raw.csv
 + altaGuardWeather6516.csv
 + windLouisMeadow080716.csv
 + loadData.R
 + subsetDates.R
 + subsetNumAv_Dates.R
 + ModelRegBiNumAv.R ~~ModelReg.R ~~
 + EnsembleAllModels.R

 - ModelRegBiNumAv.R and execute the script,  then execute EnsembleAllModels.R there are some lines which are necessary to execute in order to build training sets, etc.. Contact me if you are confused :)
 
 ##Checking model
 + Done in ModelRegBiNumAv.R and EnsembleAllModels.R which generate files of the accuracy
 + ~~rrfun.R~~
 + ~~checkAccRidReg.R~~
 
 ##Looking to help?
 - (In no particular order)
 - [ ] Increasing the size of the data sets by including more data from the mesowest network!
 - [ ] Create a multiclass classification, so that the forecaster would reccommend an aspect and maybe? and elevation
 - [ ] Reconsider feature choices (ex: water weight instead of precip and snowfall)
 - [ ] Include feature scaling and renormalization
 - [ ] Build more models, using neural networks could be fun, and polynomial/nonlinear functions
 - [ ] Clean up code, so that functions are called instead of everything being hardcoded
 - [ ] Improve documentation
 - [x] Rewrangle data so that number of avalanches is not included, just whether or not an avalanche occured and rerun analysis/regression
 - [x] Develop an ensemble forecast, i.e., create multiple different models, then use these to develop a probability of whether or not an avalanche may occur
 - [ ] Build script to pull down current weather data, so that we don't have to enter it by hand
 - [ ] Check my work
 - [ ] Any of your own ideas you bring to the table
 
 >Thanks for stopping by!

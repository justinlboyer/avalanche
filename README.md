# avalanche
Project in R on avalanches in Wasatch

If you are interested in contributing please do!
## Contents 
 + Cookbook.tex - documentation of the data cleaning process
 + ModelReg.R - script containing different regession models
 + NOAA_DATA_documentation.pdf - self explantory
 + NoZAv_app.R - Shiny app that produces a model with all observations containing 0 avalanches removed, depends on:
    -NoZee.bestlam.R - the best lambda
    -NoZrr.fit_PSSTTW.rda - the saved model 
    -NoZrr.fit_PSSTTWA.rda - the saved model with the addition of aspect
 + Original_app.r - the template all other shiny apps were built off
 + ZeAvEqAv.R - script that compares different regression models when 3/4 of the observations contain 0 avalanches (this was a decision on my part, conservatively arguing that 3 out of 4 days we should observe at least one avalanche.  Need to come back to and use poisson process to model more accurately)'''
 + altaGuardWeather080716.csv - dataset of weather from alta guard procured August 7, 2016
 + app.R - shint app that uses ridge regession model with all observations
 + avalanche.Rproj - workspace, might be usefull?
 + avalanches_raw.csv - dataset containing all avalanche data procured August 7, 2016
 + checkAccRidReg.R - script for checking accuracy of ridge regession model (needs rrfun.R)
 + dataAnalysis.R - somewhat deprecated, but rough premliminary analysis of data
 + downloadCurrentWU.R - a script to pull down the weather conditions at alta, not started
 + loadData.R - script loads data set, first cleaning
 + mostZrr.bestlam.R - best lambda from ZeAvEqAv.R (some0Av_app.R needs this file)
 + mostZrr.fit_PSSTWA.rda - model from ZeAvEqAv.R (some0Av_app.R needs this file)
 + rrfun.R - script needed by checkACCRidReg.R, builds model from training set
 + some0Av_app.R - shiny app that predicts number of avalanches: currently running at justinlboyer.shinyapps.io/avalanche/
 + subsetCCB.R - subsets data by caught, cleaned and buried only used in data analysis
 + subsetDates.R - subsets and oraganizes data by dates (required by pretty much everything)
 + subsetIK.R - subsets data by injured and killed, only used in data analysis
 + subsetNumAv_Dates.R - subsets and organizes the data by number of avalanches that occured (required by all models and apps)
 + windLouisMeadow080716.csv - dataset containing wind information, procured August 7, 2016
 
 
 ##Building model
 - Required files: 
 + avalanches_raw.csv
 + altaGuardWeather6516.csv
 + windLouisMeadow080716.csv
 + loadData.R
 + subsetDates.R
 + subsetNumAv_Dates.R
 + ModelReg.R 

 - Open ModelReg.R and execute the lines you are interested in, there are some lines which are necessary to execute in order to build training sets, etc.. Contact me if you are confused :)
 
 ##Checking model
 + rrfun.R
 + checkAccRidReg.R
 
 ##Looking to help?
 - (In no particular order)
 - [ ] Improve documentation
 - [ ] Rewrangle data so that number of avalanches is not included, just whether or not an avalanche occured and rerun analysis/regression
 - [ ] Build script to pull down current weather data, so that we don't have to enter it by hand
 - [ ] Check my work
 - [ ] Any of your own ideas you bring to the table
 
 >Thanks for stopping by!

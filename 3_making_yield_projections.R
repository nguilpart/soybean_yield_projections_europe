#=================================================================#
#                                                                 #
#   RSCRIPT USED TO MAKE SOYBEAN YIELD PROJECTIONS                #
#   IN EUROPE UNDER CURRENT AND FUTURE CLIMATE SCENARIOS          #
#                                                                 #
#=================================================================#
#
# AUTHOR      : Nicolas Guipart (nicolas.guilpart@agroparistech.fr)
# LAST UPDATE : 15 February 2022
# DESCRITPION : This scripts uses the Random Forest model fitted
#               in a previous R script (the best model) to make 
#               projections of soybean yield in Europe under current
#               and future climate scenarios. Projections outputs are
#               saved as netCDF files (one file by year and climate model).

# set working directory, seed, and load packages
setwd("..")
set.seed(32)
library(ranger); library(raster); library(plyr)

# Load data and functions -------------------------------------------------

# load RF model fitted over the whole dataset (from previous script)
load("outputs/mod.rf.all.RData")             

# load functions to get climate data
# note : the two functions below are used to read montly climate data
# (from GRASP or CMIP5) in netCDF format and produce a rasterstack with 
# 36 layers corresponding to the 36 predictors in the RF model, i.e. 5 climate
# variables over 7 months of growing season (tmax,tmin,rain,solar,and vp) 
# plus the irrigated.fraction (which is set to zero for projections, assuming
# rainfed soybean everywhere). One rasterstack is produced per year and climate
# model, and then used as an input of the RF model to make projections in Europe.
# These functions are not provided here.
source("Rscripts/function_get_GRASP_historical_climate.R")
source("Rscripts/function_get_climate_scenario.R")


# Projections with GRASP historical climate data ---------------------------

inputs <- data.frame(year = 1981:2010)

mlply(.data= inputs,.progress="text",
      .fun= function(year){
        newExpl <- get_GRASP_climate(year)
        proj <- predict(newExpl,mod.rf.all, type='response', fun = function(model, ...){predict(model, ...)$predictions})
        file.name <- paste("outputs/RF_projections/GRASP_historical_",year,sep="")
        writeRaster(proj,file.name,format="CDF",overwrite=TRUE)
      })

# projections for future climate scenarios conditions ---------------------------------------

inputs <- expand.grid(model= c("GFDL-ESM2M","HadGEM2-ES","IPSL-CM5A-LR",
            "MIROC5","MIROC-ESM","MIROC-ESM-CHEM","MRI-CGCM3","NorESM1-M"),
            RCP  = c("rcp45","rcp85"), year = c(2050:2059,2090:2099))
inputs <- subset(inputs,subset=!(model=="HadGEM2-ES"&year%in%c(2099,2100)))

mlply(.data= inputs,.progress="text",
      .fun= function(model,RCP,year){
        newExpl <- get_climate(model,RCP,year)
        proj <- predict(newExpl,mod.rf.all, type='response', fun = function(model, ...){predict(model, ...)$predictions})
        file.name <- paste("outputs/RF_projections/",model,"_",RCP,"_",year,sep="")
        writeRaster(proj,file.name,format="CDF",overwrite=TRUE)
      })

# end of script
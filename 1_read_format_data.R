#=================================================================#
#                                                                 #
#     R SCRIPT USED TO READ SOYBEAN YIELD AND CLIMATE DATA        #
#       AND FORMAT THE RESULTING DATAFRAME FOR MODELING           #
#                                                                 #
#=================================================================#
#
# AUTHOR      : Nicolas Guilpart (nicolas.guilpart@agroparistech.fr)
# DATE        : 15 February 2022
# DESCRIPTION : This script reads yield data from the Global Dataset of
#               Historical Yields for major crops updated version 
#               (Iizumi et al. 2014, 2018) and historical climate data 
#               from the GRASP dataset (Ruane et al 2015) and format 
#               them to investigate climate - crop yield relationships for soybean. 
# NOTE        : This script cannot be run because inputs data are not provided
# CITED REFERENCES
# Iizumi et al. (2014) Historical changes in global yields: major cereal and legume crops from 1982 to 2006. Global ecology and biogeography, 23(3), 346-357.
# Iizumi et al. (2018) Uncertainties of potentials and recent changes in global yields of major crops resulting from census- and satellite-based yield datasets at multiple resolutions. PLoS One 13, e0203809 (2018).
# Ruane et al (2015). Climate forcing datasets for agricultural modeling: Merged products for gap-filling and historical climate series estimation. Agricultural and Forest Meteorology, 200, 233-248.

# packages
library(plyr); library(data.table); library(raster); library(RANN)

# set working directory
# note: the script was initially stored in a folder named "Rscripts" 
# and data were initially stored in a folder named "data" that is 
# located is the same folder than the "Rscripts" folder. The output
# of this script will be saved under a folder named "outputs" located
# ii the same folder than the "Rscripts" and "data" folder.
setwd("..")

# set seed
set.seed(32)

# PART I ------------------------------------------------------------------
# Read the file "geo_info_1.125degree.dat" 
# This file contains latitude and longitude of each 
# grid cell as well as grid cell area harvested soybean, 
# maize, wheat and rice areas, country name and country 
# code, and gridcode. The gridcode is the unique ID of a grid cell,
# it allows linking yield, climate and geo data sets
# -------------------------------------------------------------------------

# read file
data.geo <- read.table("data/geo_info_1.125degree.dat",header=TRUE,flush="TRUE")

# select columns
data.geo <- subset(data.geo,select=c("GridCode","Long.deg.","Lati.deg.","Elev.m.","GridCellArea.ha.","HvstSoybean...","HvstMaize...","HvstRice...","HvstWheat...","CountryName","CountryCode"))

# rename columns
names(data.geo)  <- c("gridcode","lon","lat","elevation","cellarea","soy_area","maize_area","rice_area","wheat_area","country","country_code")

# set NAs to NA
data.geo$country <- gsub("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX","NA",data.geo$country)

# rename some countries
data.geo$country[data.geo$country_code==229] <- "United_Kingdom"
data.geo$country[data.geo$country_code==162] <- "Norway"
data.geo$country[data.geo$country_code==154] <- "Macedonia"
data.geo$country[data.geo$country_code==27] <- "Bulgaria"

# change longitude from [0;360] (original GRASP system) to [-180;180]
data.geo$lon[data.geo$lon>180] <- data.geo$lon[data.geo$lon>180] - 360

# PART II -----------------------------------------------------------------
# Read yield data
# Yield data are stored in a folder named "GDHY_v1.1" (which stands for Global Dataset of Historical
# Yields version 1.1) that contains "*.dat" files in which yield data are stored. Each "*.dat" file 
# correspond to a specific grid cell with yields (reported and modeled) for all years from 1981 to 2011 in t/ha. For example,
# the file named "yield_004-089.dat" correspond to the grid cell having the grid code 004-089.
# -------------------------------------------------------------------------

# read data
yield_file  <- list.files("data/GDHY_v1.1",full.names = TRUE)
data.yield  <- llply(yield_file,read.table,sep = "", dec = ".", header = TRUE,.progress="text")

# from list to dataframe
data.yield  <- rbindlist(data.yield)

# extract gridcode from file name
gridcode    <- substring(yield_file,22,28)

# each gridcode should be replicated 31 times as there are 31 years (1981-2011)
data.yield$gridcode <- rep(gridcode,each=31)

# keep only modeled yields and remove reported yields
data.yield <- data.yield[,c(1,3,4)]

# rename columns
names(data.yield) <- c("year","Ya","gridcode")

# remove -999 and replace by NA
data.yield$Ya[data.yield$Ya==-999] <- NA

# remove useless objects
rm(gridcode,yield_file)

# PART III ----------------------------------------------------------------
# Remove yield trend
# To avoid any confusion with technological progress, soybean yield data
# are detrended in order to remove the increasing trends of soybean yield 
# time series due to improved cultivars and technological progress 
# -------------------------------------------------------------------------

# remove NAs
data.yield.notrend <- na.omit(data.yield)

# loop on grid cell (grid code)
gridcode <- unique(data.yield.notrend$gridcode)
for (i in gridcode){
  # loop on gridcode starts with subset on gridcode
  tmp <- subset(data.yield.notrend,gridcode==i)
  # fit smoothing spline
  mod <- smooth.spline(tmp$year,tmp$Ya)
  # add yearly residuals from smoothing splines to 
  # expected value from smoothing spline
  data.yield.notrend$Ya[data.yield.notrend$gridcode==i] <-  predict(mod)$y[length(tmp$Ya)] + residuals(mod)
}

# remove useless objects
rm(tmp,mod,i,data.yield)

# PART IV -----------------------------------------------------------------
# Add locations with yield equals zero in desert and arctic areas
# In this section we use the Koppen-Geiger climate classification
# to identify desert and arctic areas in which we will set
# soybean yields equal to zero
# -------------------------------------------------------------------------

# read Koppen-Geiger classification in raster format
# data are from http://koeppen-geiger.vu-wien.ac.at/present.htm
# the march 2017 version
KGmap <- raster("data/Koppen_Geiger/KG_1986-2010.grd")

# select climate zone of interest
# 7  - BWh : arid, winter dry , hot arid
# 8  - BWk : arid, winter dry , cold arid
# 20 - Dfc : snow, fully humid, cool summer 
# 21 - Dfd : snow, fully humid, extremely continental
# 30 - EF  : polar frost
# 31 - ET  : polar tundra
KGmap.nosoy <- KGmap
KGmap.nosoy[!(KGmap.nosoy%in%c(7,8,20,21,30,31))] <- NA

# remove antarctic
KGmap.nosoy <- crop(KGmap.nosoy, extent(-180, 180,-60, 90))

# Sample random points. Sampling is stratified by climate zones
# here we select what percentage of the final dataset
# sampled values with yield=0 will represent
perc <- 0.2
n <- round(((1/(30*6))*(perc * dim(data.yield.notrend)[1]) / (1-perc)),digits=0)
KGmap.nosoy.sample <- sampleStratified(KGmap.nosoy, size=n,xy=TRUE,sp=TRUE)

# retrieve gridcode of sample points
  # define two datasets 
  set1 <- subset(data.geo,select=c(lon,lat))
  set2 <- subset(KGmap.nosoy.sample@data,select=c(x,y))
  names(set2) <- c("lon,lat")
  # use the nn2 function from the RANN package to 
  # find nearest neighbor of each line of set2 in set1
  output <- nn2(set1,set2,k=1)
  output <- set1[output[[1]][,1],]
  # merge with data.geo to retrieve gridcode
  output <- merge(output,data.geo)
  
# remove sample points that fall into the same gridcell (gridcode)
# to avoid duplicated gridcode  
  output <- output[!duplicated(output$gridcode),]
  
# add sample points to yield data
out <- data.frame(year = rep(1981:2011,times=dim(output)[1]),
                    Ya=rep(0,times=31*dim(output)[1]),   
                    gridcode = rep(output$gridcode,each=31))
data.yield.notrend <- rbind(data.yield.notrend,out)

# remove useless objects
rm(out,output,set1,set2,n,perc,KGmap,KGmap.nosoy,KGmap.nosoy.sample,gridcode)

# PART V ------------------------------------------------------------------
# Read climate data
# These data are at the monthly time step and contains data only where
# maize or soybean or wheat or rice is grown. Data are stored in a "*.dat"
# format that is similar to the yield data. For example, the file names 
# "GRASP_climate_001-089.dat" contains climate data at a monthly time step 
# for the grid cell havng the grid code "001-089". Data cover the period from
# 1961 to 2010 (50 years).
# -------------------------------------------------------------------------

# read data
climat_file  <- list.files("data/GRASP_climate_monthly_whole_world",full.names = TRUE)
data.clim    <- llply(climat_file,read.table,sep = "", dec = ".", header = TRUE,.progress="text")

# from list to dataframe
data.clim <- rbindlist(data.clim)

# extract gridcode from file name
gridcode  <- substring(climat_file,54,60)

# replicate each gridcode 50 years * 12 months
data.clim$gridcode <- rep(gridcode,each=50*12)

# set column names
names(data.clim) <- c("year","month","tmax","tmin","rain","solar","vp","wind","gridcode")

# remove useless objects
rm(gridcode,climat_file)

# PART VI -----------------------------------------------------------------
# Define soybean crop cycle (planting and harvest dates) 
# The soybean growing season is defined country-by-country 
# according to the crop calendars provided by the Agricultural 
# Market Information System (available at: http://www.amis-outlook.org/amis-about/calendars/soybeancal/en/). 
# Based on this source of information, the soybean growing season 
# is considered to range from April to October in China, USA, and Italy, 
# from November to May in Argentina and Brazil, from May to November 
# in Canada, and from June to December in India.
# -------------------------------------------------------------------------

crop.cycle <- data.frame(country = rep(c("China","Italy","Argentina","Brazil","Canada","USA","India","Desert"),each=12),
                         month = rep(1:12,times=8),
                         month.from.sowing = c(NA,NA,NA,1:7,NA,NA,
                                               NA,NA,NA,1:7,NA,NA,
                                               3:7,NA,NA,NA,NA,NA,1:2,
                                               3:7,NA,NA,NA,NA,NA,1:2,
                                               NA,NA,NA,NA,1:7,NA,
                                               NA,NA,NA,1:7,NA,NA,
                                               NA,NA,NA,NA,NA,1:7,
                                               NA,NA,NA,1:7,NA,NA)) # here we define soybean growing season from april to october
                                                                    # in desert areas with yield = 0. 

# PART VII ----------------------------------------------------------------
# Merge detrended yield and climate data for modeling
# -------------------------------------------------------------------------

# merge climate and detrended yield data
tab.notrend <- merge(data.yield.notrend,data.clim,by=c("gridcode","year"),all.x=TRUE)

# remove pixels with Ya = NA
tab.notrend <- tab.notrend[!is.na(tab.notrend$Ya),]

# merge with geo data to retrieve countries
tab.notrend <- merge(tab.notrend,data.geo,all.x=TRUE,by="gridcode")

# add a fictive country code and a high fictive percentage 
# of soy area in sample points in desert locations with Ya=0
tab.notrend$country_code[tab.notrend$Ya==0] <- 100000
tab.notrend$soy_area[tab.notrend$Ya==0] <- 100

# select a few countries : USA (231), Canada (33), Italy (351), China (106), 
# Argentina (9), Brazil (21), India (100), and Desert areas (100000)
tab.notrend <- subset(tab.notrend,subset=country_code%in%c(231,33,351,106,9,21,100,100000))
tab.notrend$country <- gsub("United","USA",tab.notrend$country)
tab.notrend$country[tab.notrend$Ya==0] <- "Desert"

# select pixels with soybean area >= 1%
tab.notrend <- subset(tab.notrend,subset=!(soy_area<1))

# select months of the soybean growing season
tab.notrend <- merge(tab.notrend,crop.cycle,by=c("country","month"))
tab.notrend <- subset(tab.notrend,subset=!is.na(month.from.sowing))
tab.notrend <- subset(tab.notrend,subset=!(Ya==0&lat<0))

# make sure factors are factors
tab.notrend$month              <- factor(tab.notrend$month)
tab.notrend$month.from.sowing  <- factor(tab.notrend$month.from.sowing)
tab.notrend$year               <- factor(tab.notrend$year)
tab.notrend$country            <- factor(tab.notrend$country)
tab.long.notrend               <- tab.notrend

# put in wide format with average monthly climate variables
tab.notrend.tmax         <- dcast(tab.notrend,gridcode+country+year+Ya~month.from.sowing,value.var = "tmax")
names(tab.notrend.tmax)  <- paste(c(rep("",4),rep("tmax.",7)),names(tab.notrend.tmax),sep="")
tab.notrend.tmin         <- dcast(tab.notrend,gridcode+country+year+Ya~month.from.sowing,value.var = "tmin")
names(tab.notrend.tmin)  <- paste(c(rep("",4),rep("tmin.",7)),names(tab.notrend.tmin),sep="")
tab.notrend.rain         <- dcast(tab.notrend,gridcode+country+year+Ya~month.from.sowing,value.var = "rain")
names(tab.notrend.rain)  <- paste(c(rep("",4),rep("rain.",7)),names(tab.notrend.rain),sep="")
tab.notrend.solar        <- dcast(tab.notrend,gridcode+country+year+Ya~month.from.sowing,value.var = "solar")
names(tab.notrend.solar) <- paste(c(rep("",4),rep("solar.",7)),names(tab.notrend.solar),sep="")
tab.notrend.vp           <- dcast(tab.notrend,gridcode+country+year+Ya~month.from.sowing,value.var = "vp")
names(tab.notrend.vp)    <- paste(c(rep("",4),rep("vp.",7)),names(tab.notrend.vp),sep="")
tab.notrend <- merge(tab.notrend.tmax,tab.notrend.tmin)
tab.notrend <- merge(tab.notrend,tab.notrend.rain)
tab.notrend <- merge(tab.notrend,tab.notrend.solar)
tab.notrend <- merge(tab.notrend,tab.notrend.vp)
tab.wide.notrend <- tab.notrend

# remove useless objects
rm (tab.notrend.rain,tab.notrend.solar,tab.notrend.tmax,tab.notrend.vp,tab.notrend.tmin,tab.notrend)

# PART VIII ---------------------------------------------------------------
# Read SPAM data for irrigated vs. rainfed discrimination
# The objective of this section is to retrieve the gridcode
# that are irrigated and those that are rainfed, according 
# to a threshold (e.g. a pixel is classified as irrigated if
# more than 1% of its harvested soybean area is irrigated).
# SPAM2005 data are available at: 
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/DHXBJX
# -------------------------------------------------------------------------

# read SPAM2005 data
spam.soy.irrigated <- raster("data/SPAM2005/spam2005v2r0_harvested-area_soybean_irrigated.tiff")
spam.soy.total     <- raster("data/SPAM2005/spam2005v2r0_harvested-area_soybean_total.tiff")

# calculate the percentage of soybean area that is rainfed in each pixel
spam.irrigated.fraction <- spam.soy.irrigated / spam.soy.total

# transform yield data in a raster in which pixel values are the gridcodes
data.y                 <- unique(subset(tab.long.notrend,select=c(lon,lat,gridcode)))
data.y                 <- SpatialPointsDataFrame(coords=subset(data.y,select=c(lon,lat)),data=subset(data.y,select="gridcode"))
data.y.raster          <- data.y
data.y.raster$gridcode <- gsub("-","",as.character(data.y.raster$gridcode))
data.y.raster$gridcode <- as.numeric(data.y.raster$gridcode)
data.y.raster          <- rasterize(data.y.raster,raster(nrows=160, ncols=320))
data.y.raster          <- data.y.raster[[2]]

# change the resolution of spam2005 data so that it matches
# the resolution of yield data
spam.irrigated.fraction <- projectRaster(spam.irrigated.fraction,data.y.raster,method="bilinear")
irrigated.fraction <- extract(spam.irrigated.fraction,data.y,sp=TRUE)@data
names(irrigated.fraction) <- c("gridcode","irrigated.fraction")
irrigated.fraction$irrigated.fraction[is.na(irrigated.fraction$irrigated.fraction)] <- 0

# remove useless objects
rm(spam.soy.irrigated,spam.soy.total,data.y,data.y.raster,spam.irrigated.fraction)

# merge irrigated fraction with detrended yield and climate data
tab.long.notrend <- merge(tab.long.notrend,irrigated.fraction,by="gridcode")
tab.wide.notrend <- merge(tab.wide.notrend,irrigated.fraction,by="gridcode")

# remove useless objects
rm(crop.cycle,data.clim,data.geo,data.yield,data.yield.notrend,irrigated.fraction,tab.long.notrend)

# FINAL PART --------------------------------------------------------------
# the output of this scripts is the dataframe named
# "tab.wide.notrend" that contains detrended yield and climate data
# and is ready to be used for modeling.
# -------------------------------------------------------------------------

# merge with data.geo to retrieve latitude and longitude
tab.wide.notrend <- merge(tab.wide.notrend,subset(data.geo,select=c(gridcode,lon,lat)),by="gridcode")

# save output
save(tab.wide.notrend,file="outputs/soybean_yield_climate_data.Rdata")

# end of script
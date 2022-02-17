#=================================================================#
#                                                                 #
#   R SCRIPT USED TO TRAIN MODELS TO PREDICT                      #
#   SOYBEAN YIELD FROM CLIMATE INPUTS AND                         #
#   ASSESS THEIR PREDICTIVE ABILITY THROUGH RESIDUAL ANALYSIS     #
#   AND TRANFERABILITY ASSESSMENT IN SPACE AND TIME               #
#                                                                 #
#=================================================================#
#
# AUTHOR      : Nicolas Guilpart (nicolas.guilpart@agroparistech.fr)
# LAST UPDATE : 15 February 2022 
# DESCRITPION : In this script, several algorithms/models (Random Forest, 
#               Artificial Neural Network, Generalized Additive Model, and 
#               Multiple Linear Regression) are used to model soybean yield
#               from climate inputs. The best model (random forest) is then
#               analyzed in details, including :(i) an in-depth analysis of
#               model residuals, (ii) transferability in time, and 
#               (iii) transferability in space. 
#
# set working directory
# note: the script was initially stored in a folder named "Rscripts" 
# and the dataframe containing soybean yeidl and limate data was 
# initially stored in a folder named "outputs" that is 
# located is the same folder than the "Rscripts" folder
setwd("..")

# set seed
set.seed(32)

# packages
library(ranger); library(gam) ; library(neuralnet)  ; library(caret) 
library(plyr)  ; library(data.table) ; library(sp) ; library(raster)
library(dismo)

# read data
load("outputs/soybean_yield_climate_data.RData")

# PART I ------------------------------------------------------------------
# Fit and save the Random Forest (RF) model over the whole dataset
# -------------------------------------------------------------------------

mod.rf.all  <- ranger(Ya~.,data=tab.wide.notrend[,4:40],num.tree=500,importance="impurity")
save(mod.rf.all,file="outputs/mod.rf.all.RData")


# PART II -----------------------------------------------------------------
# Assess RF model performance with unstratified bootstrap and compare the
# performance with other models : Generalized Additive Model (GAM), 
# Artificial Neural Networks (ANN), and Multiple Linear Regression (MLR). 
# Bootstrap is performed using the train() function of the caret R package.
# -------------------------------------------------------------------------

# define summary function
my.summaryFunction <- function(data, lev = NULL, model = NULL){
  EFF <- 1 - (sum((data$obs-data$pred)^2)/sum((data$obs-mean(data$obs))^2))
  res <- c(defaultSummary(data),EFF)
  names(res) <- c("RMSE","R2","MAE","EFF")
  return(res)
}

# apply to random forest (takes ~ 10 min)
caret.rf.all <- train(Ya ~ ., data = tab.wide.notrend[,4:40],
                   method = "ranger",
                   tuneGrid=expand.grid(mtry = 6, splitrule = "variance",min.node.size =5),
                   trControl = trainControl(method="boot",verboseIter  = TRUE,
                                    number=25,savePredictions = "final",
                                    summaryFunction = my.summaryFunction))
  
# apply to GAM (takes ~ 30 min)
caret.gam.all <- train(form=Ya ~ tmin.1 + tmin.2 + tmin.3 + tmin.4 + tmin.5 + 
                           tmin.6 + tmin.7 + tmax.1 + tmax.2 + tmax.3 + 
                           tmax.4 + tmax.5 + tmax.6 + tmax.7 + rain.1 + 
                           rain.2 + rain.3 + rain.4 + rain.5 + rain.6 + 
                           rain.7 + solar.1 + solar.2 + solar.3 + solar.4 + 
                           solar.5 + solar.6 + solar.7 + vp.1 + vp.2 + 
                           vp.3 + vp.4 + vp.5 + vp.6 + vp.7 + irrigated.fraction
                         , data = tab.wide.notrend[,4:40],
                         method = "gam",
                         trControl = trainControl(method="boot",verboseIter  = TRUE,
                                                  number=25,savePredictions = "final",
                                                  summaryFunction = my.summaryFunction),
                         tuneGrid=expand.grid(method = "GCV.Cp",select = FALSE))
  
# apply to MLR
caret.glm.all <- train(Ya ~ ., data = tab.wide.notrend[,4:40],
                        method = "glm",
                        trControl = trainControl(method="boot",verboseIter  = TRUE,
                                                 number=25,savePredictions = "final",
                                                 summaryFunction = my.summaryFunction))
  
# apply to artificial neural network
caret.ann.all <- train(Ya ~ ., data = tab.wide.notrend[,4:40],
                        method = "neuralnet",
                        tuneGrid=expand.grid(layer1 = 10, layer2 = 0,layer3 =0),
                        trControl = trainControl(method="boot",verboseIter  = TRUE,
                                                 number=25,savePredictions = "final",
                                                 summaryFunction = my.summaryFunction))
  
# get evaluation results for the 4 models
caret.rf.all$results[4:7]
caret.glm.all$results[2:5]
caret.gam.all$results[3:6]
caret.ann.all$results[4:7]

# make plot of observed vs predicted for he RF model
par(mfrow=c(1,1),mar=c(5,5,5,7))
plot(caret.rf.all$pred$pred,caret.rf.all$pred$obs,cex=0.4,
                pch=16,xlim=c(0,7),ylim=c(0,7),col=rgb(0.01,0.01,0.01,alpha=0.2),
                xlab=expression(paste("predicted soybean yield (t ",ha^-1,")",sep="")),
                ylab=expression(paste("observed soybean yield (t ",ha^-1,")",sep="")),las=1)
abline(0,1,lty=3)
legend("topright",legend="1:1",bty="n")
x <- c(density.default(caret.rf.all$pred$pred,from=0,to=7)$x,0)
y <- c(density.default(caret.rf.all$pred$pred,from=0,to=7)$y+7.5,7.5)
polygon(x,y,col="grey",xpd=TRUE,border="grey")
x <- c(density.default(caret.rf.all$pred$obs,from=0,to=7)$x,0)
y <- c(density.default(caret.rf.all$pred$obs,from=0,to=7)$y+7.5,7.5)
polygon(y,x,col="grey",xpd=TRUE,border="grey")


# PART III ----------------------------------------------------------------
# Analysis of RF model residuals
# -------------------------------------------------------------------------

# get out-of-bag predictions from caret train() results
df <- data.frame(obs  = caret.rf.all$pred$obs,
                 pred = caret.rf.all$pred$pred,
                 id   = caret.rf.all$pred$rowIndex)

# calculate residuals
df$res <- df$obs-df$pred

# merge with 
tab.wide.notrend$id <- 1:dim(tab.wide.notrend)[1]
df <- merge(df,tab.wide.notrend)

# plot histogram of residuals
hist(df$res,main="",breaks=40,col="lightgreen",
     xlab=expression(paste("residuals (t ",ha^-1,")")),las=1,freq=FALSE,
     xlim=c(-3,3))
box()

# plot residuals as a function of yield
plot(df$obs,df$res,
     las=1,xlab=expression(paste("observed soybean yield (t ",ha^-1,")")),
     xlim=c(0,7),ylim=c(-3,3),ylab=expression(paste("residuals (t ",ha^-1,")")),
     col=rgb(0.01,0.01,0.01,alpha=0.2),pch=16,cex=0.4)
abline(h=0)

# plot model residuals as a function of latitude
plot(res~lat,data=df,pch=16,col=rgb(0.1,0.1,0.1,alpha=0.2),las=1,
     cex=0.5,xlim=c(-40,80),xlab="latitude",ylab="residuals")

# model residuals in hot and dry years along a rainfall gradient

  # calculate total in-season rainfall and average
  # in-season average tmax
  df$tmax.tot <- (df$tmax.1 + df$tmax.2 + df$tmax.3 + df$tmax.4 + df$tmax.5 + df$tmax.6 + df$tmax.7)/7
  df$rain.tot <- df$rain.1 + df$rain.2 + df$rain.3 + df$rain.4 + df$rain.5 + df$rain.6 + df$rain.7

  # Identify hot and cold years
  # Hot years are defined as years for which average in-season tmax 
  # is higher than the 90th percentile of average in-season tmax (~30°C),
  # and cold years are defined as years for which average in-season tmax 
  # is lower than the 40th percentile of average in-season tmax (~22°C).
  q <- quantile(df$tmax.tot, probs = c(0.4,0.9))
  df$col <- "black"
  df$col[df$tmax.tot<q[1]] <- "blue"
  df$col[df$tmax.tot>q[2]] <- "red"

  # make plots
  plot(res~tmax.tot,data=df,pch=16,col=rgb(0.1,0.1,0.1,alpha=0.2),las=1,
     cex=0.4,xlab="average in-season tmax",ylab="residuals")
  plot(res~rain.tot,data=df,pch=16,col="grey",las=1,
     cex=0.4,xlab="total in-season rainfall (mm)",ylab="residuals",xlim=c(0,6000),ylim=c(-3,3))
  lines(res~rain.tot,data=subset(df,col=="red"),pch=16,col=col,cex=0.4,type="p")
  legend("topright",legend=c("all points","hot years"),pch=15,pt.cex = 1.5,col=c("grey","red"),bty="n")
  plot(res~rain.tot,data=df,pch=16,col="grey",las=1,
     cex=0.4,xlab="total in-season rainfall (mm)",ylab="residuals",xlim=c(0,6000),ylim=c(-3,3))
  lines(res~rain.tot,data=subset(df,col=="blue"),pch=16,col=col,cex=0.4,type="p")
  legend("topright",legend=c("all points","cold years"),pch=15,pt.cex = 1.5,col=c("grey","blue"),bty="n")


# PART III ----------------------------------------------------------------
# Assessing RF model transferability in time
# This analysis is presented only for the RF model to save space
# but an exact similar procedure can be applied to other models (GAM, ANN, MLR).
# Transferability in time was assessed by splitting the dataset into two 
# periods in order to assess the ability of the model to predict a period 
# of time different from the one used for the training: 1981-1995, and 1996-2010.
# For simplicity we only present below the case of training on 1981-1995 
# to predict 1996-2010.
# -------------------------------------------------------------------------

# define train and test datasets
data.train <- subset(tab.wide.notrend,subset=year%in%c(1981:1995))
data.test <- subset(tab.wide.notrend,subset=year%in%c(1996:2010))

# fit RF model
mod.rf  <-  ranger(Ya~.,data=data.train[,4:40],num.tree=500)

# make predictions on test dataset
data.test$Ya.pred.rf  <- predict(mod.rf,data=data.test)$predictions

# compute R2 between observed and predicted values on test dataset
mod <- lm(Ya~Ya.pred.rf,data=data.test)
summary(mod)

# compute  RMSEP
sqrt(mean((data.test$Ya-data.test$Ya.pred.rf)^2))
  
# compute model efficiency
1 - (sum((data.test$Ya-data.test$Ya.pred.rf)^2)/sum((data.test$Ya-mean(data.test$Ya))^2))

# make plot of observed vs predicted yields on test dataset 
par(mfrow=c(1,1),mar=c(5,5,5,7))
plot(data.test$Ya.pred.rf,data.test$Ya,cex=0.4,
     pch=16,xlim=c(0,7),ylim=c(0,7),col=rgb(0.01,0.01,0.01,alpha=0.2),
     xlab=expression(paste("predicted soybean yield (t ",ha^-1,")",sep="")),
     ylab=expression(paste("observed soybean yield (t ",ha^-1,")",sep="")),las=1)
abline(0,1,lty=3)
legend("topright",legend="1:1",bty="n")
x <- c(density.default(caret.rf.all$pred$pred,from=0,to=7)$x,0)
y <- c(density.default(caret.rf.all$pred$pred,from=0,to=7)$y+7.5,7.5)
polygon(x,y,col="grey",xpd=TRUE,border="grey")
x <- c(density.default(caret.rf.all$pred$obs,from=0,to=7)$x,0)
y <- c(density.default(caret.rf.all$pred$obs,from=0,to=7)$y+7.5,7.5)
polygon(y,x,col="grey",xpd=TRUE,border="grey")


# PART IV -----------------------------------------------------------------
# ASSESSING TRANSFERABILITY IN SPACE
# In this section, transferability in space is assessed using a five-step 
# procedure: (i) selection of a pixel at random in the yield database 
# (excluding added true absences), (ii) creation of 7 buffer zones of different 
# size (radius) around that pixel (radius values are 100 km, 500 km, 1000 km, 
# 1500 km, 2000 km, 2500 km, 3000 km), (iii) for each buffer zone, remove pixels 
# within the buffer zone and fit one model on the rest of the dataset (including 
# added true absences), (iv) predict the initially selected pixel (30 years by pixel), 
# (v) compute average error of prediction over years for the pixel. This procedure 
# is applied to 10 pixels by country selected at random.
# For simplicity the procedure is only presented below for RF model, but it can
# be easily applied to other models (GAM, ANN, MLR)
# -------------------------------------------------------------------------

# transform data into spatial data
sdata <- SpatialPointsDataFrame(coords = subset(tab.wide.notrend,select=c(lon,lat)),
                                data=tab.wide.notrend,
                                proj4string=CRS("+proj=longlat +datum=WGS84"))

# loop on countries (takes ~3h)
countries <- c("Argentina","Brazil","Canada","China","India","Italy","USA")

for (i in 1:7){
  
  print(countries[i])
  
  grids <- as.character(sample(sdata$gridcode[sdata$country==countries[i]],10))
  
  for (j in 1:10){
    
    print(j)
    
    # select starting point
    # we don't want a desert/(ant)artic area as a starting point
    print("create buffer zones")
    start <- subset(sdata,gridcode==grids[j])
    bzone.100  <- circles(start,d=100000)
    bzone.500  <- circles(start,d=500000)
    bzone.1000 <- circles(start,d=1000000)
    bzone.1500 <- circles(start,d=1500000)
    bzone.2000 <- circles(start,d=2000000)
    bzone.2500 <- circles(start,d=2500000)
    bzone.3000 <- circles(start,d=3000000)
    
    # create training datasets :
    # keep only gridcodes outside the buffer zones
    df.100  <- sdata@data[is.na(over(sdata,bzone.100@polygons)),]
    df.500  <- sdata@data[is.na(over(sdata,bzone.500@polygons)),]
    df.1000 <- sdata@data[is.na(over(sdata,bzone.1000@polygons)),]
    df.1500 <- sdata@data[is.na(over(sdata,bzone.1500@polygons)),]
    df.2000 <- sdata@data[is.na(over(sdata,bzone.2000@polygons)),]
    df.2500 <- sdata@data[is.na(over(sdata,bzone.2500@polygons)),]
    df.3000 <- sdata@data[is.na(over(sdata,bzone.3000@polygons)),]
    
    # select 700 gridcodes at random to get the same size for
    # all training datasets
    df.100  <- subset(df.100,subset=gridcode%in%sample(unique(df.100$gridcode),700))
    df.500  <- subset(df.500,subset=gridcode%in%sample(unique(df.500$gridcode),700))
    df.1000 <- subset(df.1000,subset=gridcode%in%sample(unique(df.1000$gridcode),700))
    df.1500 <- subset(df.1500,subset=gridcode%in%sample(unique(df.1500$gridcode),700))
    df.2000 <- subset(df.2000,subset=gridcode%in%sample(unique(df.2000$gridcode),700))
    df.2500 <- subset(df.2500,subset=gridcode%in%sample(unique(df.2500$gridcode),700))
    df.3000 <- subset(df.3000,subset=gridcode%in%sample(unique(df.3000$gridcode),700))
    
    # fit models
    print("model fitting")
    mod.rf.100  <- ranger(Ya~.,data=df.100[,4:40],num.trees=500)
    mod.rf.500  <- ranger(Ya~.,data=df.500[,4:40],num.trees=500)
    mod.rf.1000 <- ranger(Ya~.,data=df.1000[,4:40],num.trees=500)
    mod.rf.1500 <- ranger(Ya~.,data=df.1500[,4:40],num.trees=500)
    mod.rf.2000 <- ranger(Ya~.,data=df.2000[,4:40],num.trees=500)
    mod.rf.2500 <- ranger(Ya~.,data=df.2500[,4:40],num.trees=500)
    mod.rf.3000 <- ranger(Ya~.,data=df.3000[,4:40],num.trees=500)
    
    # make predictions
    print("make predictions")
    start$Ya.rf.pred.100   <- predict(mod.rf.100,data=start@data)$predictions
    start$Ya.rf.pred.500   <- predict(mod.rf.500,data=start@data)$predictions
    start$Ya.rf.pred.1000  <- predict(mod.rf.1000,data=start@data)$predictions
    start$Ya.rf.pred.1500  <- predict(mod.rf.1500,data=start@data)$predictions
    start$Ya.rf.pred.2000  <- predict(mod.rf.2000,data=start@data)$predictions
    start$Ya.rf.pred.2500  <- predict(mod.rf.2500,data=start@data)$predictions
    start$Ya.rf.pred.3000  <- predict(mod.rf.3000,data=start@data)$predictions
    
    # create or update outputs
    if (i==1 & j==1) {
      output.pred <- start@data
    } else {
      output.pred <- rbind(output.pred,start@data)
    }
  }
}

# use these lines to start again from here is the script takes
# too long to run
# save(output.pred,file="spatial_transferability.Rdata")
# load("spatial_transferability.Rdata")

# format data for subsequent boxplot
df <- data.table(subset(output.pred,select=c(gridcode,country,year,Ya,
                  Ya.rf.pred.100,Ya.rf.pred.500,
                  Ya.rf.pred.1000,Ya.rf.pred.1500,
                  Ya.rf.pred.2000 ,Ya.rf.pred.2500 ,Ya.rf.pred.3000)))
df <- melt.data.table(df,id.vars = c("gridcode","country","year","Ya"),variable.factor=FALSE)
names(df) <- c("gridcode","country","year","Ya","dist","Ya.pred")
df$dist <- gsub("Ya.rf.pred.","",df$dist)
df$dist <- factor(df$dist,levels=c("100","500","1000","1500","2000","2500","3000"))

# make boxplot of RMSEP = f(distance) (for 1 distance there are 70 grid cells)  
df$res <- df$Ya.pred-df$Ya
my.fun.rmsep <- function(res){sqrt(mean(res^2))}
df.rmsep.by.gridcode.and.dist <- aggregate(res~dist+gridcode,data=df,FUN=my.fun.rmsep)
par(mar=c(5,5,2,2),mfrow=c(1,1))
boxplot(res~dist,data=df.rmsep.by.gridcode.and.dist,las=1,
        ylab=expression(paste("RMSEP (t ",ha^-1,")")),
        xlab="distance from the predicted grid-cell (km)
within which grid-cells were removed from the training dataset")

# calculate overall RMSEP by distance
df.rmsep <- aggregate(res~dist,data=df,FUN=my.fun.rmsep)
  
# calculate overall model efficiency by distance
df$EF.numerator <- (df$Ya-df$Ya.pred)^2
df$EF.denominator <- (df$Ya-mean(df$Ya))^2
df.EF.num <- aggregate(EF.numerator~dist,data=df,FUN=sum)
df.EF.den <- aggregate(EF.denominator~dist,data=df,FUN=sum)
df.EF <- merge(df.EF.num,df.EF.den)
df.EF$EF <- 1-(df.EF$EF.numerator/df.EF$EF.denominator)
df.EF <- subset(df.EF,select=c(dist,EF))

# remove useless objects
rm(df.EF.den,df.EF.num,output.pred)

# end of script
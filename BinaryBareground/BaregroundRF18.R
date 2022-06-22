# Random Forest Example with Grand Canyon Bareground Presence/Absence data
# adapted from Brian Woodward
# Griffin Shelor 07/16/2018

# update 7/16/18: this is for comparing binary bareground presence/absence in Grand Canyon NP

# install package for group installing and loading of following libraries (you
# only need to do this once ever on your computer)
# install.packages("pacman")

# installs packages that are not installed yet and loads all packages needed
pacman::p_load(raster, shapefiles, sp, rgdal, randomForest, caret, e1071, 
               MASS, ROCR, corrplot, rfUtilities, VSURF)

# set seed = RF is an inherently random process (i.e. you will get a slightly
# different result each time you run the algorithm). setting a seed allows you
# to get the exact same results each time. the seed is just a random number that
# you choose-- it doesn't have a meaning.

set.seed(123)

# read in CSV file and save as an object
data <- read.csv("/Volumes/GISDRIVE/BigBison/RandomForest/Bareground_sentinel_PA.csv")
str(data) # 1204 obs of 17 variables


#values and make this a new object
rastercolumns <- data[, 1:10]

## Let's look at variable importance and parsimony (reduce number of predictors
## as much as possible without losing too much explanatory power) in our model

varimportance_cov <- rf.modelSel(rastercolumns, data$PA, imp.scale="se", r=(0.01),final.model = FALSE, seed = 123)
plot(varimportance_cov)
#?rf.modelSel
varimportance_cov <- cbind(rownames(varimportance_cov$sel.importance), varimportance_cov$sel.importance)
rownames(varimportance_cov) <- NULL
colnames(varimportance_cov) <- c("name", "imp")

varimportance_cov_ord <- varimportance_cov[order(-varimportance_cov$imp),]
varimportance_cov_ord

# drop columns that are not as important based on rfmodelsel

raster_cov_names <- varimportance_cov_ord$name[1:10]

rastercolumns_cov <- rastercolumns[,as.character(raster_cov_names)[-c(3,4,6,8,9,10)]]

# calculate correlation coefficient matrix
correlation <-cor(rastercolumns_cov, method="pearson")

# plot the correlation. the darker the number, the more correlated the two
# variables
corrplot(correlation,method="number")

## You can now manually remove any variables that are above your correlation 
## threshold. In this case we will just leave them all in, but we could remove
## them by using the following code

# make data set with just percent cover and raster columns of choice
data_cov_model <- cbind(rastercolumns_cov, PA = data$PA)


# Now let's restrict our final predictors to those that are most important (Change manually) 
# and run the continuous model

rf_model1 = randomForest(PA ~ ., data=data_cov_model, importance = TRUE, ntree = 5000, mtry = 3)
rf_model1

# LONG STORY SHORT: higher values of %IncMSE mean that a predictor is more
# important relative to other predictors

importance(rf_model1)

varImpPlot(rf_model1)

# Plot predicted vs. Observed: now we want to see how well our algorithm
# predicts bareground presence/absence and compare that to the actual bareground presence.

# sets graphic parameter so it is a 1 x 1 output
par(mfrow=c(1,1))

# creates a vector of predicted values of bareground presence based on model (one for each
# actual observation)
predicted <- rf_model1$predicted
predicted

# creates a vector of actual values of bareground based on collected data
observed<-data$PA
accuracy(observed,predicted)
plot(predicted,observed)
presenceobserved <- nrow(subset(data,data$PA=="1"))
presenceobserved
absenceobserved <- nrow(subset(data,data$PA=="0"))
absenceobserved
presencepredicted <- nrow(subset(rf_model1,rf_model1$predicted=="1"))
presencepredicted



# code taken from https://stackoverflow.com/questions/37897252/plot-confusion-matrix-in-r-using-ggplot
TClass <- factor(c(0, 0, 1, 1))
PClass <- factor(c(0, 1, 0, 1))
Y      <- c(2816, 248, 34, 235)
df <- data.frame(TClass, PClass, Y)
library(ggplot2)
ggplot(data =  df, mapping = aes(x = TClass, y = PClass)) +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
  scale_fill_gradient(low = "beige", high = "red4") +
  theme_bw() + theme(legend.position = "none")

# plot observed values on the x-axis and predicted values on the y-axis. we are
# looking for these to be correlated (so close to a 1:1 line)
plot(observed,predicted, ylab= "Predicted", xlab = "Observed", main = "Bareground Presence", pch = 20)

# we can fit a line to denote the central tendency and plot it on top of the 
# points
abline(fit <- lm(predicted ~ observed), col='Black')

# we can add a legend that calculates an R2 value (ranges from 0 to 1, the
# closer to 1 the better)
legend("top", bty="n", legend=paste("R2 is", format(summary(fit)$adj.r.squared, digits=4)))


# Mean Absolute Error (MAE): average magnitude of errors in a set of predictions
# without considering their direction

# MAE = (1/n)*sum[i:n]abs(yobs_i - ypredict_i)

# Root Mean Squared Error (RMSE): ALSO average magnutide of error without
# considering their direction

# RMSE = sqrt((1/n)*sum[i:n](yobs_i - ypredict_i)^2

# Interpretation: for both MAE and RMSE, lower values indicate better prediction
# ability of model (i.e. lower model prediction error). RMSE gives a relatively
# high weight to large errors. It seems like MAE was more traditionally used,
# but RMSE is more standard currently.
error <- predicted - observed

rmse <- function(error)
{
  sqrt(mean(error^2))
}


mae  <- function(error)
{
  mean(abs(error))
}

error <- observed - predicted

rmse(error)
mae(error)


######Map results#####
# we can do this once we have our rasters ready
# X0716evi=raster("C:/Users/new/Documents/CSU/NASA_Develop/Data/Landsat_Scenes/02_Final/p36r33/LS8_p036r033_20160705_evi.tif")
# X1015red=raster("C:/Users/new/Documents/CSU/NASA_Develop/Data/Landsat_Scenes/02_Final/p36r33/LS8_p036r033_20151007_red.tif")
# X0815greenn=raster("C:/Users/new/Documents/CSU/NASA_Develop/Data/Landsat_Scenes/02_Final/p36r33/LS8_p036r033_20150804_TCapGreenness.tif")
# X0915blue=raster("C:/Users/new/Documents/CSU/NASA_Develop/Data/Landsat_Scenes/02_Final/p36r33/LS8_p036r033_20150921_blue.tif")
# X0915slavi=raster("C:/Users/new/Documents/CSU/NASA_Develop/Data/Landsat_Scenes/02_Final/p36r33/LS8_p036r033_20150921_slavi.tif")
# X0415_gndvi=raster("C:/Users/new/Documents/CSU/NASA_Develop/Data/Landsat_Scenes/02_Final/p36r33/LS8_p036r033_20150414_gndvi.tif")
# X0815gemi=raster("C:/Users/new/Documents/CSU/NASA_Develop/Data/Landsat_Scenes/02_Final/p36r33/LS8_p036r033_20150804_gemi.tif")
# X0716ndwi2=raster("C:/Users/new/Documents/CSU/NASA_Develop/Data/Landsat_Scenes/02_Final/p36r33/LS8_p036r033_20160705_ndwi2.tif")

# Stack the objects into one dataframe
# stack_6band=stack(X0716evi,X1015red,X0815greenn,X0915blue,X0915slavi,X0415_gndvi,X0815gemi,X0716ndwi2)

# Add header names - this might not be necessary since I add names later
# names(stack_6band)=c('X0716_evi','X1015_red', 'X0815_greenn', 'X0915_blue','X0915_slavi','X0415_gndvi','X0815_gemi','X0716_ndwi2')

####################################### map it, BINARY FIRST, then CONTINUOUS###################################
# predict(stack_6band, rf_model1, filename="TamCover_16.tif",fun=predict,format="GTiff",datatype="FLT4S",overwrite=TRUE )





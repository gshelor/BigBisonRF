## RANDOM FOREST CLASSIFICATION
## WITH CIRCLE CONFUSION PLOT

## Created by Megan Vahsen (mvahsen@nd.edu)
## DEVELOP Summer 2017 Colorado River Basin Water Resources

##
# Classification
##

# tam16 <- read.csv("/filepath/tamarisk_data.csv")

# subset data so it only includes percent cover >=20% (presence) and 0% (absence)
tam16_pa <- subset(tam16, Percent_Cov %in% c(0,seq(20,100,1)))

# create column for presence/absence based on above criteria
tam16_pa$PA <- as.factor(ifelse(tam16_pa$Percent_Cov >=20, 1, 0))

# Best model from VSURF = X0716_slavi, X0415_gndvi, X1015_Tbrig, X0915_blue
data_model16pa <- tam16_pa[,c("PA", "X0716_slavi", "X0415_gndvi", "X0815_gemi", "X0915_blue","X0815_ndwi2")]

rf_16_pa <- randomForest(PA ~ . , data = data_model16pa, ntree = 2000, importance = TRUE)
# randomForest(formula = PA ~ ., data = data_model16pa, ntree = 2000,      importance = TRUE, mtry = 3) 
# Type of random forest: classification
# Number of trees: 2000
# No. of variables tried at each split: 3
# 
# OOB estimate of  error rate: 14.52%
# Confusion matrix:
#   0  1 class.error
# 0 298 30  0.09146341
# 1  32 67  0.32323232

# rate by level of importance
importance(rf_16_pa)
# 0        1 MeanDecreaseAccuracy MeanDecreaseGini
# X0716_slavi 32.76352 70.43156             62.00598         40.21618
# X0415_gndvi 50.60414 35.39011             60.78388         30.20003
# X0815_gemi  30.72686 11.92180             33.40495         17.51110
# X0915_blue  60.82455 20.10339             61.74626         27.59617
# X0815_ndwi2 27.59926 38.54444             47.09026         36.22786

pred16pa <- predict(rf_16_pa)
obs16pa <- data_model16pa$PA

# summary statistics
summary_16pa <- accuracy(x = pred16pa, y=obs16pa)
# Accuracy (PCC): 84.9411764705882% 
# 
# Cohen's Kappa: 0.5695 
# 
# Area under the ROC curve: 0.682084 
# 
# Users accuracy: 
# 0    1 
# 91.1 64.6 
# 
# 
# Producers accuracy: 
# 0    1 
# 89.5 68.8 
# 
# 
# True Skill statistic: 0.5575076 
# 
# Sensitivity: 0.9110429 
# 
# Sensitivity: 0.6464646 
# 
# Positive Likelihood Ratio: 2.57695 
# 
# Negative Likelihood Ratio: 0.1376054 
# 
# Type I error: 0.3535354 
# 
# Type II error: 0.08895706 
# 
# F-score: 0.9027356 
# 
# Matthews correlation coefficient: 0.5699893 
# 
# Confusion matrix 
# y
# x     0   1
# 0 297  35
# 1  29  64

# plot confusion matrix
confusion_16 <- summary_16pa$confusion
names(dimnames(confusion_16)) <- c("Predicted", "Observed")
colnames(confusion_16) <-c("Absence", "Presence")
rownames(confusion_16) <-c("Absence", "Presence")

pdf("Confusion16_LS_0801.pdf", height = 6.23, width = 6.64)

fourfoldplot(confusion_16, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main = "Tamarisk 2016")

dev.off()

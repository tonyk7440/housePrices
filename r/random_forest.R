#Random Forest har har

#Load/Install the following packages needed for the script
pacman::p_load(randomForest, caret, dplyr, car)

#Read data files
trainOrig <- read.csv("input/train.csv", stringsAsFactors = FALSE)
testOrig <- read.csv("input/test.csv", stringsAsFactors = FALSE)


n <-dim(trainOrig)[1]
missing.summary <- sapply(trainOrig, function(x) sum(is.na(x))) 
index.missing <- sapply(trainOrig, function(x) sum(is.na(x))) > 0 
num.variable.missing <- length(missing.summary[index.missing])
freq.table.miss <- data.frame( Variable = names(missing.summary[index.missing]), 
                       Number.of.Missing = as.integer(missing.summary[index.missing]), 
                       Percentage.of.Missing = as.numeric(prop.table(missing.summary[index.missing])) )

freq.table.miss <- freq.table.miss %>% 
  select(Variable:Percentage.of.Missing) %>%
  arrange(desc(Number.of.Missing))

# Remove id variable
train <- trainOrig[,-1]
test <- testOrig[,-1]

# Remove outliers according to pdf
train <- subset(train, train$GrLivArea < 4500)
full <- rbind(train[,-ncol(train)], test)

summary(full)

numeric_df <- train[sapply(train,is.numeric)]

#Replace NA's
full$Alley <- replace(full$Alley, is.na(full$Alley), "None")
full$BsmtQual <- replace(full$BsmtQual, is.na(full$BsmtQual), "noBasement")
full$BsmtCond <- replace(full$BsmtCond, is.na(full$BsmtCond), "noBasement")
full$PoolQC <- replace(full$PoolQC, is.na(full$PoolQC), "noPool")
full$Fence <- replace(full$Fence, is.na(full$Fence), "noFence")
full$MiscFeature <- replace(full$MiscFeature, is.na(full$MiscFeature), "None")

full$MSSubClass[full$MSSubClass == 150] <- 120
full$MSSubClass <- as.factor(full$MSSubClass)
full$MSZoning <- replace(full$MSZoning, is.na(full$MSZoning), "RL")
full$LotFrontageA <- replace(full$LotFrontage, is.na(full$LotFrontage), 0)
full$LotFrontage <- replace(full$LotFrontage, is.na(full$LotFrontage), full$LotArea - full$GrLivArea)
full$Utilities <- replace(full$Utilities, is.na(full$Utilities), "AllPub")
full$Exterior1st <- replace(full$Exterior1st, is.na(full$Exterior1st), "VinylSd")
full$Exterior2nd <- replace(full$Exterior2nd, is.na(full$Exterior2nd), "VinylSd")
full$MasVnrType <- replace(full$MasVnrType, is.na(full$MasVnrType), "None")
full$MasVnrArea <- replace(full$MasVnrArea, is.na(full$MasVnrArea), 0)
full$BsmtQual[full$BsmtQual == "Po"] <- "Fa"
levels(full$BsmtQual) = levels(full$BsmtQual)
full$BsmtCond[full$BsmtCond == "Po"] <- "Fa"
full$BsmtExposure <- replace(full$BsmtExposure, is.na(full$BsmtExposure), "None")
full$BsmtFinType1 <- replace(full$BsmtFinType1, is.na(full$BsmtFinType1), "None")
full$BsmtFinSF1 <- replace(full$BsmtFinSF1, is.na(full$BsmtFinSF1), 0)
full$BsmtFinType2 <- replace(full$BsmtFinType2, is.na(full$BsmtFinType2), "None")
full$BsmtFinSF2 <- replace(full$BsmtFinSF2, is.na(full$BsmtFinSF2), 0)
full$BsmtUnfSF <- replace(full$BsmtUnfSF, is.na(full$BsmtUnfSF), 0)
full$TotalBsmtSF <- replace(full$TotalBsmtSF, is.na(full$TotalBsmtSF), 0)
full$BsmtFullBath <- replace(full$BsmtFullBath, is.na(full$BsmtFullBath), 0)
full$BsmtFullBath <- as.factor(full$BsmtFullBath)
full$BsmtHalfBath <- replace(full$BsmtHalfBath, is.na(full$BsmtHalfBath), 0)
full$BsmtHalfBath <- as.factor(full$BsmtHalfBath)
full$KitchenQual <- replace(full$KitchenQual, is.na(full$KitchenQual), "Gd")
full$Functional <- replace(full$Functional, is.na(full$Functional), "Typ")
full$FireplaceQu <- replace(full$FireplaceQu, is.na(full$FireplaceQu), "None")
full$GarageType <- replace(full$GarageType, is.na(full$GarageType), "None")
full$GarageYrBlt <- replace(full$GarageYrBlt, is.na(full$GarageYrBlt), 0)
full$GarageFinish <- replace(full$GarageFinish, is.na(full$GarageFinish), "None")
full$GarageCars <- replace(full$GarageCars, is.na(full$GarageCars), 0)
full$GarageArea <- replace(full$GarageArea, is.na(full$GarageArea), 0)
full$GarageQual <- replace(full$GarageQual, is.na(full$GarageQual), "None")
full$GarageCond <- replace(full$GarageCond, is.na(full$GarageCond), "None")
full$SaleType <- replace(full$SaleType, is.na(full$SaleType), "WD")
full <- full %>% mutate_if(is.character, as.factor)

#full$OverallCondFac <- as.factor(full$OverallCond)
#full$BedroomAbvGrFac <- as.factor(full$BedroomAbvGr)
#full$KitchenAbvGrFac <- as.factor(full$KitchenAbvGr)
#full$RoomsExBed <- as.factor(full$TotRmsAbvGrd - full$BedroomAbvGr)
#full$MasVnrAreaFac <- as.factor(ifelse(full$MasVnrArea == 0 , 0,1))
#full$TotalBsmtSFFac <- as.factor(ifelse(full$TotalBsmtSF == 0 , 0,1))
#full$WoodDeckSFFac <- as.factor(ifelse(full$WoodDeckSF == 0 , 0,1))
#full$YrSold <- as.factor(full$YrSold)
#remod <- as.factor(ifelse(full$YearBuilt != full$YearRemodAdd & full$YearBuilt >1950 , 1,0))
#lotSurround <- full$LotArea - full$GrLivArea
#full <- cbind(full[1:20],remod,lotSurround, full[21:ncol(full)])

colSums(full == 0)




# Separate out into train and test
train <- full[1:nrow(trainOrig),]
train <- cbind(train, SalePrice = trainOrig[,ncol(trainOrig)])
test <- full[1461:nrow(full),]

#
sapply(full, function(y) sum(length(which(is.na(y)))))
summary(full$KitchenQual)
new_DF <- full[rowSums(is.na(full)) > 0,]

sum(complete.cases(full))

scatterplot(SalePrice ~ Neighborhood, data=train,  xlab="Square Footage Floor 1", 
            ylab="Sale Price", grid=FALSE) # should be factor variable
scatterplot(SalePrice ~ MoSold, data=train,  xlab="Square Footage Floor 1", 
            ylab="Sale Price", grid=FALSE)
scatterplot(YearRemodAdd ~ YearBuilt, data=train,  xlab="Year Built", 
            ylab="Year Remod Added", grid=FALSE)

ggplot(train, aes(x = FullBath, y = SalePrice, color = OverallCond )) + geom_point()
ggplot(train, aes(x = HalfBath, y = SalePrice, color = OverallCond )) + geom_point()
ggplot(train, aes(x = OverallQual, y = OverallCond )) + geom_point()
# How many na's in each column
getModelInfo("rf")$qrf$parameters

grid <- expand.grid(mtry = 100)

ctrl <- trainControl(method = "oob",
                     summaryFunction = defaultSummary,
                     classProbs = FALSE)

RandomForestTune <- train(SalePrice ~., data = train,
                          method = "rf",
                          verbose = TRUE,
                          trControl = ctrl,
                          tuneGrid = grid)

print(RandomForestTune)
print(RandomForestTune$finalModel)

RandForestPred <- predict(RandomForestTune2,test)

#See which variables are important in the model
varImpPlot(RandomForestTune$finalModel)
varImpPlot(RandomForestTune2$finalModel)

# Create a data frame with two columns
my_solution <- data.frame(Id = testOrig$Id, SalePrice = RandForestPred)

# Write your solution away to a csv file with the name my_solution.csv
write.csv(my_solution,file="output/my_solutioncv.csv", row.names=FALSE)


#
cv.ctrl <- trainControl(method = "repeatedcv", repeats = 10,number = 2, 
                        #summaryFunction = twoClassSummary,
                        allowParallel=T, verboseIter = TRUE)

xgb.grid <- expand.grid(nrounds = 500,
                        max_depth = c(2,3),
                        eta = c(0.05,0.07),
                        gamma = c(0.5,1),
                        colsample_bytree = c(0.3,0.5, 0.8),
                        min_child_weight =  c(15, 20, 25)
)

set.seed(45)
xgb_tune <-train(SalePrice ~.,
                 data=train,
                 method="xgbTree",
                 metric = "RMSE",
                 trControl=cv.ctrl,
                 tuneGrid=xgb.grid,
                 verbose=T,
                 nthread =3
)

min(xgb_tune$results$RMSE)
importance_matrix <- xgb.importance(model = xgb_tune$finalModel)
print(importance_matrix)
library(Ckmeans.1d.dp)
xgb.plot.importance(importance_matrix)
xgb.plot.deepness(model = xgb_tune$finalModel)

xgbPredictions <- predict(xgb_tune,test)

# Create a data frame with two columns
my_solution <- data.frame(Id = testOrig$Id, SalePrice = xgbPredictions)

# Write your solution away to a csv file with the name my_solution.csv
write.csv(my_solution,file="output/xgb_solution_many_feats.csv", row.names=FALSE)

current_best <- capture.output(cat("Best RMSE so far",min(xgb_tune$results$RMSE), "with",
                  "\n nrounds: ", unlist(xgb_tune$bestTune[1]),
                  "\n max_depth:", unlist(xgb_tune$bestTune[2]),
                  "\n eta:", unlist(xgb_tune$bestTune[3]),
                  "\n gamma:", unlist(xgb_tune$bestTune[4]),
                  "\n colsample_bytree:", unlist(xgb_tune$bestTune[5]),
                  "\n min_child_weight:", unlist(xgb_tune$bestTune[6]),
                  sep = " "))
#nrounds (# Boosting Iterations)
#max_depth (Max Tree Depth) [default=6]
maximum depth of a tree, increase this value will make model more complex / likely to be overfitting.
range: [1,∞]
#eta (Shrinkage) [default=0.3]
step size shrinkage used in update to prevents overfitting. After each boosting step, we can directly get the weights of new features. and eta actually shrinks the feature weights to make the boosting process more conservative.
range: [0,1]
#gamma (Minimum Loss Reduction)  [default=0]
minimum loss reduction required to make a further partition on a leaf node of the tree. the larger, the more conservative the algorithm will be.
range: [0,∞]
#colsample_bytree (Subsample Ratio of Columns) [default=1]
subsample ratio of columns when constructing each tree.
range: (0,1] 
#min_child_weight (Minimum Sum of Instance Weight)  [default=1]
minimum sum of instance weight(hessian) needed in a child. If the tree partition step results in a leaf node with the sum of instance weight less than min_child_weight, then the building process will give up further partitioning. In linear regression mode, this simply corresponds to minimum number of instances needed to be in each node. The larger, the more conservative the algorithm will be.
range: [0,∞]
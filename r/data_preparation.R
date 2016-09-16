# Data Preparation
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

# Glmnet
library(Metrics)

#Read data files
training80 <- read.csv("output/train80.csv", stringsAsFactors = TRUE)
validation20 <- read.csv("output/validation20.csv", stringsAsFactors = TRUE)

myControl <- trainControl(
  method = "cv", number = 10,
  #summaryFunction = twoClassSummary,
  #classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE
)

# Two tuning parameters, alpha and lambda
myGrid <- expand.grid(alpha = seq(0.0001, 1, length = 20),
                      lambda = seq(0.0001, 1, length = 20))

# Train glmnet with custom trainControl and tuning: model
model <- train(SalePrice ~., training80,
               tuneGrid = myGrid,
               method = "glmnet",
               trControl = myControl
)

# Print model to console
model

validation20$Condition2[validation20$Condition2 == "RRAe"] <- "Norm"
validation20$ExterCond[validation20$ExterCond == "Po"] <- "Fa"
test$ExterCond[test$ExterCond == "Po"] <- "Fa"
levels(train$BsmtQual) = levels(train$BsmtQual)
# Predict on validation set
glmnet_v_preds <- predict(model,validation20)

# Check equal
validation_log_SalePrice <- log(validation20$SalePrice)
glmnet_log_prediction <- log(glmnet_v_preds)

#Test with RMSE
rmse(validation_log_SalePrice,glmnet_log_prediction)

# Print maximum ROC statistic
max(model$results$ROC)

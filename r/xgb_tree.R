#
<<<<<<< HEAD
library(Metrics)
training <- read.csv("output/train_ohe.csv", stringsAsFactors = TRUE)
validation <- read.csv("output/validation_ohe.csv", stringsAsFactors = TRUE)
test <- read.csv("output/test_ohe.csv", stringsAsFactors = TRUE)

training <- read.csv("output/train_plus_ohe.csv", stringsAsFactors = TRUE)
validation <- read.csv("output/validation_plus_ohe.csv", stringsAsFactors = TRUE)
test <- read.csv("output/test_plus_ohe.csv", stringsAsFactors = TRUE)

cv.ctrl <- trainControl(method = "repeatedcv", number = 5, 
=======
cv.ctrl <- trainControl(method = "repeatedcv", repeats = 10,number = 2, 
>>>>>>> 00669d4ddb1337d05c4f4fb98c1d21b01c52ab7b
                        #summaryFunction = twoClassSummary,
                        allowParallel=T, verboseIter = TRUE)

xgb.grid <- expand.grid(nrounds = 500,
<<<<<<< HEAD
                        max_depth = c(3,4),
                        eta = c(0.07, 0.09),
                        gamma = c(0.7,1),
                        colsample_bytree = c(0.2, 0.3,0.5),
                        min_child_weight =  c(20, 25, 30)
=======
                        max_depth = c(2,3),
                        eta = c(0.05,0.07),
                        gamma = c(0.5,1),
                        colsample_bytree = c(0.3,0.5, 0.8),
                        min_child_weight =  c(15, 20, 25)
>>>>>>> 00669d4ddb1337d05c4f4fb98c1d21b01c52ab7b
)

set.seed(45)
xgb_tune <-train(SalePrice ~.,
<<<<<<< HEAD
                 data=training,
=======
                 data=train,
>>>>>>> 00669d4ddb1337d05c4f4fb98c1d21b01c52ab7b
                 method="xgbTree",
                 metric = "RMSE",
                 trControl=cv.ctrl,
                 tuneGrid=xgb.grid,
                 verbose=T,
                 nthread =3
)

<<<<<<< HEAD
# Predict on validation set
xgb_v_preds <- predict(xgb_tune,validation)

# Check equal
validation_log_SalePrice <- log(validation$SalePrice)
xgb_log_prediction <- log(xgb_v_preds)

#Test with RMSE
validation_log_err <- rmse(validation_log_SalePrice,xgb_log_prediction)

# Validation error
validation_error <- rmse(validation$SalePrice,xgb_v_preds)

model_output_new7 <- capture.output(cat("Model RMSE is: ",min(xgb_tune$results$RMSE), 
                                        "with \n validation set error of", validation_error,
                                        "and \n validation log error of", validation_log_err,
                                        "\n nrounds: ", unlist(xgb_tune$bestTune[1]),
                                        "\n max_depth:", unlist(xgb_tune$bestTune[2]),
                                        "\n eta:", unlist(xgb_tune$bestTune[3]),
                                        "\n gamma:", unlist(xgb_tune$bestTune[4]),
                                        "\n colsample_bytree:", unlist(xgb_tune$bestTune[5]),
                                        "\n min_child_weight:", unlist(xgb_tune$bestTune[6]),
                                        sep = " "))
model_output # with remod
model_output_new # with remod & RoomsExBeds
model_output_new2 # with remod & lotSurround
model_output_new3 # with remod & lotSurround & max_depth 4 added
model_output_new4 # with remod & lotSurround & depth 4 removed
model_output_new5 # with remod & lotSurround & depth 4 added
model_output_new6 # dropped 30 worst features
model_output_new7 # full rank with remod

####################################



min(xgb_tune$results$RMSE) # 33122.7

=======
min(xgb_tune$results$RMSE)
>>>>>>> 00669d4ddb1337d05c4f4fb98c1d21b01c52ab7b
importance_matrix <- xgb.importance(model = xgb_tune$finalModel)
print(importance_matrix)
library(Ckmeans.1d.dp)
xgb.plot.importance(importance_matrix)
<<<<<<< HEAD
xgb.plot.importance(xgb_tune$finalModel)
xgb.plot.deepness(model = xgb_tune$finalModel)

importance_matrix2 <- xgb.importance(names(new_training), model = xgb_tune$finalModel)

# Remove 30 worst features
importance_df <- as.data.frame(importance_matrix)
remove_features <- importance_df[101:131,1]

new_training <- subset(training, select=-remove_features)
new_training <- training[ , !(names(training) %in% remove_features)]
new_validation <- validation[ , !(names(validation) %in% remove_features)]
# Train on full dataset

full_train <- rbind(training, validation)

xgb_tune <-train(SalePrice ~.,
                 data=full_train,
                 method="xgbTree",
                 metric = "RMSE",
                 trControl=cv.ctrl,
                 tuneGrid=xgb.grid,
                 verbose=T,
                 nthread =3
)


=======
xgb.plot.deepness(model = xgb_tune$finalModel)

>>>>>>> 00669d4ddb1337d05c4f4fb98c1d21b01c52ab7b
xgbPredictions <- predict(xgb_tune,test)

# Create a data frame with two columns
my_solution <- data.frame(Id = testOrig$Id, SalePrice = xgbPredictions)

# Write your solution away to a csv file with the name my_solution.csv
<<<<<<< HEAD
write.csv(my_solution,file="output/xgb_solution_ohe_full_plus.csv", row.names=FALSE)
=======
write.csv(my_solution,file="output/xgb_solution_many_feats.csv", row.names=FALSE)

current_best <- capture.output(cat("Best RMSE so far",min(xgb_tune$results$RMSE), "with",
                                   "\n nrounds: ", unlist(xgb_tune$bestTune[1]),
                                   "\n max_depth:", unlist(xgb_tune$bestTune[2]),
                                   "\n eta:", unlist(xgb_tune$bestTune[3]),
                                   "\n gamma:", unlist(xgb_tune$bestTune[4]),
                                   "\n colsample_bytree:", unlist(xgb_tune$bestTune[5]),
                                   "\n min_child_weight:", unlist(xgb_tune$bestTune[6]),
                                   sep = " "))
>>>>>>> 00669d4ddb1337d05c4f4fb98c1d21b01c52ab7b

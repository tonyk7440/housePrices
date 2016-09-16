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
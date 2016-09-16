#Adapted from the caret vignette
library("caret")
library("mlbench")
library("pROC")
data(Sonar)
set.seed(107)
inTrain <- createDataPartition(y = Sonar$Class, p = .75, list = FALSE)
training <- Sonar[ inTrain,]
testing <- Sonar[-inTrain,]
my_control <- trainControl(
  method="boot",
  number=25,
  savePredictions="final",
  classProbs=TRUE,
  index=createResample(training$Class, 25),
  summaryFunction=twoClassSummary
)

library("rpart")
library("caretEnsemble")
model_list <- caretList(
  Class~., data=training,
  trControl=my_control,
  methodList=c("glm", "rpart")
)

p <- as.data.frame(predict(model_list, newdata=head(testing)))
print(p)

cv.ctrl <- trainControl(method = "repeatedcv", repeats = 3,number = 2, 
                        #summaryFunction = twoClassSummary,
                        allowParallel=T, verboseIter = TRUE)


library("nnet")
model_list_big <- caretList(
  SalePrice~., data=train,
  trControl=cv.ctrl,
  metric="RMSE",
  methodList=c("glm", "rpart"),
  tuneList=list(
    rf1=caretModelSpec(method="rf", tuneGrid=data.frame(.mtry=2)),
    nn=caretModelSpec(method="nnet", tuneLength=2, trace=FALSE)
  )
)


modelCor(resamples(model_list_big))

greedy_ensemble <- caretEnsemble(
  model_list_big, 
  metric="RMSE",
  trControl=trainControl(
    number=2
  ))
summary(greedy_ensemble)

glm_ensemble <- caretStack(
  model_list_big,
  method="glm",
  metric="RMSE",
  trControl=trainControl(
    method="boot",
    number=10,
    savePredictions="final",
  )
)

greedy_ensemble_preds <- predict(greedy_ensemble, newdata=test)
glm_ensemble_preds <- predict(glm_ensemble, newdata=test)
summary(glm_ensemble)
rf2=caretModelSpec(method="xgbtree", 
                   tuneGrid=data.frame(.nrounds = 500, max_depth = c(3),
                                       eta = c(0.07), gamma = c(1), colsample_bytree = c(0.3, 0.8),
                                       min_child_weight =  c(15, 20, 25))),
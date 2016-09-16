#Random Forest
#Load/Install the following packages needed for the script

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
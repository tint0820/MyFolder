## Churn prediction
library(caret)

df <- read.csv("~/Downloads/churn.csv")

# split data
train_test_split <-  function(data, size=0.8){
  set.seed(42)  
	n <- nrow(data)  
	train_id <- sample(1:n, size*n)  
	train_df <- data[train_id, ]   
	test_df <- data[-train_id, ]   
return(list(train_df,train_df))
}
prep_df <- train_test_split(df, size=0.8)

# train data
# K-Cross-Validated Fold
train_control <- trainControl(method = "CV",                              
															number = 10)
model <- train(churn ~ totaldayminutes + totaldaycalls + totalnightminutes
                + totalnightcalls + numbercustomerservicecalls + numbervmailmessages,
               data = prep_df[[1]],
               method = "glm",
               trControl = train_control)

# score model
pred_churn <- predict(model, newdata = prep_df[[2]])

# evaluate
actual_churn <- prep_df[[2]]$churn
confusionMatrix(pred_churn, factor(actual_churn))

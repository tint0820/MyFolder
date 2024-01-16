library(caret)

# preview data
head(mtcars)


# build standard interface for model training 
model_2 <- train(mpg ~ hp + wt,
                 data = mtcars,
                 method = "lm") #just change method

## 1.split data
train_test_split <-  function(data, size=0.8){
  set.seed(42)
  n <- nrow(data)
  train_id <- sample(1:n, size*n)
  train_df <- data[train_id, ] #make data frame form train_id
  test_df <- data[-train_id, ] #ไม่เอา train_id ที่ใช้ไปแล้ว
  return(list(train_df,train_df))
}

prep_df <- train_test_split(mtcars, size=0.8)

## 2.train data
# K-Cross-Validated Fold
train_control <- trainControl(method = "CV",
                              number = 5)

model <- train(mpg ~ hp + wt,
               data = prep_df[[1]],
               method = "lm",
               trControl = train_control)

## 3.score model
pred_mpg <- predict(model, newdata = prep_df[[2]])

## 4.evaluation model
actual_mpg <- prep_df[[2]]$mpg
MAE(pred_mpg, actual_mpg)
RMSE(pred_mpg, actual_mpg)

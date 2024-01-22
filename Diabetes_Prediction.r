## build ML to classify diabetes patients
## binary classification

library(tidyverse)
library(caret)
library(mlbench)
library(MLmetrics)

## load dataset
data("PimaIndiansDiabetes")
df <- PimaIndiansDiabetes

## explore dataset
glimpse(df)

# missing values checking
mean(complete.cases(df))
# if mean = 1 No missing values

# select variables for training model
df_starter <- df %>%
  select(2,5,6,8,9) # we can use name's column select(glucose,...)

## 1. split data
set.seed(42)
n <- nrow(df_starter)
id <- sample(1:n, size = 0.8*n)
train_df <- df_starter[id, ]
test_df <- df_starter[-id, ]

## 2. train # train(y,x) ., คือทุกตัวแปรต้น

set.seed(42)

ctrl <- trainControl(method = "cv",
                     number=5,
                     # ดู pr = precision + recall เพิ่ม 2 lines ด้านล่าง
                     summaryFunction = prSummary,
                     classProbs = TRUE)

logis_model <- train(diabetes ~ .,
                     data = train_df,
                     method = "glm",
                     trControl = ctrl)

# 3. score
p <- predict(logis_model, newdata = test_df)

# 4. evaluate
mean(p == test_df$diabetes)

# confusion matrix 
#table(test_df$diabetes, p, dnn = c("actual","preds"))

# function 
confusionMatrix(p,
                test_df$diabetes,
                positive = "pos",
                mode="prec_recall")

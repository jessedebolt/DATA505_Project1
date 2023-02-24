
# Setup
library(tidyverse)
library(caret)
library(formatR)
library(moderndive)
library(class)
library(fastDummies)

#wine <- read_rds("../pinot.rds")


## Feature Engineering
# pinot <- wine %>% 
#   rename_all(funs(tolower(.))) %>% 
#   rename_all(funs(str_replace_all(., "-", "_"))) %>% 
#   rename_all(funs(str_replace_all(., " ", "_"))) %>% 
#   mutate(old = str_detect(description, " old")) %>% 
#   mutate(village = str_detect(description, "[Vv]illage")) %>% 
#   mutate(northern = str_detect(description, "[Nn]orthern")) %>%
#   mutate(acidity = str_detect(description, "[Aa]cidity")) %>%
#   mutate(aromas = str_detect(description, "[Aa]romas")) %>%
#   mutate(black = str_detect(description, "[Bb]lack")) %>%
#   mutate(notes = str_detect(description, "[Nn]otes")) %>%
#   mutate(plum = str_detect(description, "[Pp]lum")) %>%
#   mutate(red = str_detect(description, "[Rr]ed")) %>% 
#   mutate(tannis = str_detect(description, "[Tt]annis")) %>% 
#   mutate(drink = str_detect(description, "[Dd]rink")) %>% 
#   mutate(black = str_detect(description, "[Bb]lack")) %>% 
#   mutate(red = str_detect(description, "[Rr]ed")) %>% 
#   mutate(fruits = str_detect(description, "[Ff]ruits")) %>% 
#   mutate(cherry = str_detect(description, "[Cc]erry")) %>% 
#   mutate(palate = str_detect(description, "[Pp]alate")) %>% 
#   mutate(flavors = str_detect(description, "[Ff]lavors")) %>% 
#   mutate(fruit = str_detect(description, "[Ff]uit")) %>% 
#   mutate(finish = str_detect(description, "[Ff]inish")) %>% 
#   mutate(noir = str_detect(description, "[Nn]oir")) %>% 
#   mutate(notes = str_detect(description, "[Nn]notes")) %>% 
#   select(-description) %>% 
#   drop_na(.)
# 
# head(pinot) %>% 
#   select(1:10)


# Set seed
set.seed(504)


## Basis KNN model
basicKNN_index <- createDataPartition(pinot$province, p = 0.8, list = FALSE)
basicKNN_train <- pinot[basicKNN_index, ]
basicKNN_test <- pinot[-basicKNN_index, ]

basicKNN_fit <- knn(
  train = select(basicKNN_train,-province), 
  test = select(basicKNN_test,-province), 
  k=5, 
  cl = basicKNN_train$province, 
  prob = T)

# Basic KNN model confusion matrix
confusionMatrix(basicKNN_fit,factor(basicKNN_test$province))

# Basic KNN model with parameter tuning
param_tune_control <- trainControl(method = "boot", number = 1)
param_tune_fit <- train(province ~ .,
                        data = basicKNN_train, 
                        method = "knn",
                        tuneLength = 27,
                        trControl = param_tune_control)
param_tune_fit


# Parameter tuning confusion Matrix
confusionMatrix(predict(param_tune_fit, basicKNN_test),factor(basicKNN_test$province))


# KNN model with parameter tuning and subsampling
subsample_fit <- train(province ~ .,
                       data = basicKNN_train, 
                       method = "knn",
                       tuneLength = 27,
                       metric = "Kappa",
                       trControl = param_tune_control)

subsample_fit

# Parameter tuning and subsampling confusion Matrix
confusionMatrix(predict(subsample_fit, basicKNN_test),factor(basicKNN_test$province))


# Parameter tuning and subsampling
ggplot(subsample_fit, metric="Kappa")


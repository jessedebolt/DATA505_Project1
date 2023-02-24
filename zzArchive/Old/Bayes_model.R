
# Setup
library(tidyverse)
library(caret)
library(formatR)
library(moderndive)
library(class)
library(fastDummies)

wine <- read_rds("../pinot.rds")

#------------ USE 'Feature_eng.R -----------------
# # Feature Engineering
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
# head(wino) %>% 
#   select(1:10)


# Set seed
set.seed(504)


# Naive Bayes
bayes_index <- createDataPartition(pinot$province, p = 0.80, list = FALSE)
bayes_train <- pinot[bayes_index, ]
bayes_test <- pinot[-bayes_index, ]

bayes_fit <- train(province ~ .,
                   data = bayes_train,
                   method = "naive_bayes",
                   tuneGrid = expand.grid(usekernel = c(T,F), laplace = T, adjust = T),
                   #can specify additional parameter/details for the algorithm to use
                   metric = "Kappa",
                   trControl = trainControl(method = "cv"))
bayes_fit

# Confusion Matrix
confusionMatrix(predict(bayes_fit, bayes_test),factor(bayes_test$province))


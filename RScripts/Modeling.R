library(tidyverse)
library(randomForest)
library(glmnet)
library(caret)
library(magrittr)
library(MASS)
library(GGally)
library(rpart)
library(rpart.plot)
df <-  read.csv("spotify_data - spotify_data.csv")
df_SR <- df %>% 
  dplyr::select(-c(target,artist,uri,track,popularity,key))
# Let's see if we can predict the Sum Rank column
# Start out with a train-test split
# transform the data
# log transforms
df_tr <- df  %>% 
  mutate(across(c(acousticness,liveness,duration_ms,speechiness), ~ log(.x)),
         instrumentalness = ifelse(instrumentalness == 0,1,0),
          across(c(chorus_hit,sections,SumRank), ~ log(.x + 1)))




#
splitVec <- createDataPartition(y = df_SR$SumRank,p = .75)$Resample1
df_train <- df_SR %>% 
  slice(splitVec)
df_test <- df_SR %>% 
  slice(-splitVec)
df_train_tr <- df_tr %>% 
  slice(splitVec)
df_test <- df_SR %>% 
  slice(-splitVec)
df_train_sr 
blind_lm <- lm(SumRank ~ .,df_train)
summary(blind_lm)
plot(blind_lm)
# Let's see if we can box-cox transform
bc <- boxcox(object = blind_lm)
df_log_train <- df_train %>% 
  dplyr::select(-loudness) %>% 
  dplyr::mutate(across(everything(), ~ log(.x + 1)))
df_log_test <- df_test %>% 
  dplyr::select(-loudness) %>% 
  dplyr::mutate(across(everything(), ~ log(.x + 1)))


log_lm <- lm(SumRank ~ .,df_log_train)
summary(log_lm)  
plot(log_lm)
# Let's see what variables are selected as a result of model selection
# With BIC this time
step_log_lm <- stepAIC(object = log_lm,k = log(nrow(df_log_train)))
summary(step_log_lm)
anova(log_lm,step_log_lm)
# Lets try lasso and ridge regression now
train_feat <- df_log_train %>% 
  dplyr::select(-SumRank) %>% 
  as.matrix()
train_respo <- df_log_train %>% 
  dplyr::select(SumRank) %>% 
  as.matrix()
test_feat <- df_log_test %>% 
  dplyr::select(-SumRank) %>% 
  as.matrix()
test_respo <- df_log_test %>% 
  dplyr::select(SumRank) %>% 
  as.matrix()
cv_log_lasso <- cv.glmnet(train_feat,train_respo,alpha = 1)
cv_log_ridge <- cv.glmnet(train_feat,train_respo,alpha = 0)

best_lam_lasso <- cv_log_lasso$lambda.min
best_lam_ridge <- cv_log_ridge$lambda.min
log_lasso <- glmnet(x = train_feat,train_respo,alpha = 1,lambda = best_lam_lasso)
log_ridge <- glmnet(x = train_feat,train_respo,alpha = 0,lambda = best_lam_ridge)
names(log_lasso)
coef(log_ridge)
log_lasso$jerr
# Let's see what has the lowest test error out of all the linear models

lm_pred <- predict(log_lm,newdata = df_log_test)
step_pred <- predict(step_log_lm,newdata = df_log_test)
lasso_pred <- predict(log_lasso,test_feat)
ridge_pred <- predict(log_ridge,test_feat)
lm_mse <- mean((lm_pred - test_respo)^2)
step_mse <- mean((step_pred - test_respo)^2)
lasso_mse <- mean((lasso_pred - test_respo)^2)
ridge_mse <- mean((ridge_pred - test_respo)^2)
c(lm_mse,step_mse,lasso_mse,ridge_mse)
# It looks as though the 
# Due to more interpretability with step_mse, we go with this
summary(step_log_lm)
plot(step_log_lm)
#Let's try more non-linear options
log_tree <- rpart(SumRank ~ . , df_log_train)
rpart.plot(log_tree)
tree_pred <- predict(log_tree,df_log_test)
log_rf_cv <- randomForest::rfcv(train_feat,train_respo)
summary(step_log_lm)


log_rf <- randomForest(train_feat,train_respo,mtry = 6)

rf_pred <- predict(log_rf, df_log_test)
varImpPlot(log_rf)
tree_mse <- mean((tree_pred - test_respo)^2)
rf_mse <- mean((rf_pred - test_respo)^2)
tree_mse
rf_mse


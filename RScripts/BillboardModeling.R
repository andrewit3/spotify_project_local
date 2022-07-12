library(tidyverse)
library(randomForest)
library(glmnet)
library(caret)
library(magrittr)
library(MASS)
library(GGally)
library(rpart)
library(rpart.plot)
library(plotmo)
library(plotly)
library(leaps)
library(ggthemes)

df <-  read_csv("spotify_data - spotify_data.csv")

df %>% 
  mutate(billBool = ifelse(SumRank != 0,1,0)) %>% 
  count(billBool) 

df_SR <- df %>% 
  dplyr::select(-c(target,artist,uri,track,popularity))

df_SR %>% 
  ggplot(aes(x = log(SumRank))) +
  geom_histogram()

df_tr <- df_SR  %>% 
  filter(SumRank != 0) %>% 
  mutate(across(c(liveness,duration_ms,speechiness,SumRank), ~ log(.x)),
         instrumentalness = ifelse(instrumentalness == 0,1,0),
         across(c(chorus_hit,sections,acousticness), ~ log(.x + 1)),
         across(c(key,mode,time_signature), ~ as.factor(.x)),
         loudness  = log(-loudness))


splitVec <- createDataPartition(y = df_tr$SumRank,p = .75)$Resample1
df_train <- df_tr%>% 
  slice(splitVec)
df_test <- df_tr %>% 
  slice(-splitVec)
blind_lm <- lm(formula = SumRank ~ .,df_train)
summary(blind_lm)
par(mfrow = (c(2,2)))
step_lm_BIC <- stepAIC(object = blind_lm,k = log(nrow(df_train)),direction = "backward")
step_lm_AIC <- stepAIC(object = blind_lm,direction = "backward")
coef_vec_AIC <- summary(step_lm_AIC)$coefficients[,1]
coef_vec_BIC <- summary(step_lm_BIC)$coefficients[,1]
std_e_AIC <- summary(step_lm_AIC)$coefficients[,2]
std_e_BIC <- summary(step_lm_BIC)$coefficients[,2]
summary(step_lm_AIC)
summary(step_lm_BIC)
coef_df_AIC <- data.frame("Term" = names(coef_vec_AIC),
                      "AICoefficient" = coef_vec_AIC,
                      "StdE" = std_e_AIC)
coef_df_BIC <- data.frame("Term" = names(coef_vec_BIC),
                          "BICoefficient" = coef_vec_BIC,
                          "StdE" = std_e_BIC)
coef_df_AIC %>% 
  pivot_longer(cols =  AICoefficient:StdE)

coef_df_AIC %>% 
  mutate(StdE = ifelse(AICoefficient > 0,StdE,-StdE)) %>% 
  rename("Standard Error" = "StdE","Coefficient" = AICoefficient) %>% 
  pivot_longer(cols = Coefficient:`Standard Error`) %>% 
  mutate(name = fct_relevel(name, "Standard Error",
                            "Coefficient")) %>% 
  filter(Term != "(Intercept)") %>% 
  ggplot(aes(x = Term,y = value,fill = name)) +
  geom_col() + 
  coord_flip() + 
  labs(title = "Coefficients of Model Selected Based on AIC ") +
  guides(fill=guide_legend(title="Name")) + 
  theme_tufte() +
  scale_fill_manual(values=c("#1DB954", "Black")) 

summary(step_lm_AIC)
coef_vec_BIC
coef_df_BIC  %>% 
  mutate(StdE = ifelse(BICoefficient > 0,StdE,-StdE)) %>% 
  rename("Standard Error" = "StdE","Coefficient" = BICoefficient) %>% 
  pivot_longer(cols = Coefficient:`Standard Error`) %>% 
  mutate(name = fct_relevel(name, "Standard Error",
                            "Coefficient"))  %>% 
  filter(Term != "(Intercept)")  %>% 
  ggplot(aes(x = Term,y = value,fill = name)) +
  geom_col() + 
  theme(axis.text.x =  element_text(size = 1000000)) +
  labs(title = "Coefficients of Model Selected Based on BIC ") +
  ylab("Coefficients") + 
  guides(fill=guide_legend(title="Term")) + 
        theme_tufte() +
  coord_flip() +
  scale_fill_manual(values=c("#1DB954", "Black")) 
  
coef_df_BIC




train_feat <- df_train %>% 
  dplyr::select(-SumRank) %>% 
  data.matrix()
train_respo <- df_train %>% 
  dplyr::select(SumRank) %>% 
  data.matrix()
test_feat <- df_test %>% 
  dplyr::select(-SumRank) %>% 
  data.matrix()
test_respo <- df_test %>% 
  dplyr::select(SumRank) %>% 
  data.matrix()
cv_lasso <- cv.glmnet(train_feat,train_respo,alpha = 1)
cv_ridge <- cv.glmnet(train_feat,train_respo,alpha = 0)
par(mfrow = c(1,1))



best_lam_lasso <- cv_lasso$lambda.min
best_lam_ridge <- cv_ridge$lambda.min
lasso <- glmnet(x = train_feat,train_respo,alpha = 1,)
ridge <- glmnet(x = train_feat,train_respo,alpha = 0,)

best_lasso <- glmnet(x = train_feat,train_respo,alpha = 1,lambda = best_lam_lasso)


summary(lasso$beta)
feat_names <- attr(blind_lm$terms , "term.labels")

lasso_plot <- as.data.frame(summary(lasso$beta)) %>% 
    mutate(lambda = lasso$lambda[j],Name = feat_names[i]) %>% 
  filter(Name != "(Intercept)") %>% 
  ggplot(aes(x = log(lambda), y = x,color = Name)) +
  geom_line(size = 1) +
  geom_vline(xintercept = log(best_lam_lasso))
  ylab("Coefficient") + 
  labs(title = "Lasso Coefficients", color = 'Feature') +
  xlab("Log of Lambda")
lasso_plot
colors <- c(brewer.pal(n = 8,name = "Greens"),brewer.pal(n = 5,name = "Greys"))
colors[1] = "Green"
lasso_coef <- predict(lasso,train_feat,s = best_lam_lasso,type = "coefficients")
 as.data.frame(summary(lasso_coef)) %>% 
  mutate(Coefficent = x,Feature = feat_names[i]) %>% 
  filter(Feature != "(Intercept)") %>% 
  ggplot(aes(x = Feature, y = Coefficent,fill = Feature)) +
  geom_col() +
  coord_flip() +
  labs(title = "Coefficients of the Best Lasso Model") +
  scale_fill_manual(values = colors) +
  theme_tufte()
 
ylab("Coefficient") + 
  labs(title = "Lasso Coefficients", color = 'Feature') +
  xlab("Log of Lambda")
as.data.frame(summary(ridge$beta)) %>% 
  mutate(lambda = ridge$lambda[j],Name = feat_names[i]) %>% 
  filter(Name != "(Intercept)") %>% 
  ggplot(aes(x = log(lambda), y = x,color = Name)) +
  geom_line(size = 1) +
  ylab("Coefficient") + 
  labs(title = "Ridge Coefficients",color = 'Feature') +
  theme_tufte()





ggplotly(ridge_lambda_plot)
summary(ridge$beta)
coef(lasso)
summary(ridge$lambda)
lm_pred <- predict(blind_lm,newdata = df_test)
step_AIC_pred <- predict(step_lm_AIC,newdata = df_test)
step_BIC_pred <- predict(step_lm_BIC,newdata = df_test)
lasso_pred <- predict(best_lasso,test_feat)
ridge_pred <- predict(ridge,test_feat)
lm_mse <- mean((lm_pred - test_respo)^2)
step_AIC_mse <- mean((step_AIC_pred - test_respo)^2)
step_BIC_mse <- mean((step_BIC_pred - test_respo)^2)

lasso_mse <- mean((lasso_pred - test_respo)^2)
ridge_mse <- mean((ridge_pred - test_respo)^2)
c(lm_mse,step_AIC_mse,step_BIC_mse,lasso_mse)


tree <- rpart(SumRank ~ . , df_train)
rpart.plot(tree)
tree_pred <- predict(tree,df_test)
rf_cv <- randomForest::rfcv(train_feat,train_respo)
rf_cv$n.var
rf_cv$error.cv
rf <- randomForest(train_feat,train_respo,mtry = 4)
par(mfrow = c(1,1))
varImpPlot(rf)
rf_pred <- predict(rf, test_feat)
tree_mse <- mean((tree_pred - test_respo)^2)
rf_mse <- mean((rf_pred - test_respo)^2)
tree_mse
rf_mse
rf

names(coef(step_lm_BIC))
sign_AIC <- summary(step_lm_AIC)$coefficients[,4]
sign_BIC <- summary(step_lm_BIC)$coefficients[,4]
sign_Blind <- summary(blind_lm)$coefficients[,4]
sign_rf <- rf$importance
feat_names <- attr(blind_lm$terms , "term.labels")
names(blind_vec) <- feat_names
feat_names
coef(lasso)
blind_vec
# predictor in model
feat_names <- names(blind_lm$coefficients)
AIC_Pres <- ifelse(feat_names %in% names(step_lm_AIC$coefficients),1,0)
BIC_Pres <- ifelse(feat_names %in% names(step_lm_BIC$coefficients),1,0)
lasso.fit <- coef(glmnet(train_feat, train_respo, alpha = 1, lambda = best_lam_lasso))
lasso_pres <- ifelse(lasso.fit = 0,1,0)
pres_mat <- rbind(AIC_Pres,BIC_Pres)
heatmap(pres_mat)
rf

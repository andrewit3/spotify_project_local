library(tidyverse)
library(glmnet)
library(magrittr)
library(caret)
library(MASS)
library(pROC)
library(randomForest)
library(e1071)
library(rpart)
library(ggthemes)
df <- read_csv("spotify_data - spotify_data.csv")
df %>% 
  arrange(desc(SumRank)) %>% 
df %<>% 
  dplyr::select(-c(target,artist,uri,track,popularity))
df %<>%
  mutate(madeBB = ifelse(SumRank != 0,1,0)) %>% 
  dplyr::select(-SumRank)
splitVec <- createDataPartition(df$madeBB,p = .5)  
dfTrain = df %>% 
  slice(splitVec$Resample1)
dfTest = df %>% 
  slice(-splitVec$Resample1)
# Fit a naive logistic regression model in R
df_tr <- df  %>% 
  mutate(across(c(liveness,duration_ms,speechiness), ~ log(.x)),
         instrumentalness = ifelse(instrumentalness == 0,1,0),
         across(c(chorus_hit,sections,acousticness), ~ log(.x + 1)),
         loudness  = log(-loudness)) %>% 
  dplyr::select(-c(key,mode,time_signature))

dfTrainTr = df_tr %>% 
  slice(splitVec$Resample1)
dfTestTr = df_tr %>% 
  slice(-splitVec$Resample1)
logit <- glm(madeBB ~ .,data = dfTrain,family = "binomial")
logitTr <- glm(madeBB ~ .,data = dfTrainTr,family = "binomial")
logitStep <- logit %>% 
  stepAIC(direction = "backward")

logitRoc <- roc(predictor = logit$fitted.values, response = dfTrain$madeBB)
logitTRRoc <- roc(predictor = logitTr$fitted.values, response = dfTrain$madeBB)
logitStepRoc <- roc(predictor = logitStep$fitted.values, response = dfTrain$madeBB)
logitRoc$auc ;  logitStepRoc$auc
dfTrainNKey = dfTrain %>% 
  dplyr::select(-key)
dfTestNKey = dfTest %>% 
  dplyr::select(-key)
X =  model.matrix(madeBB ~ ., dfTrainNKey)[, -1]
cv.lasso = cv.glmnet(X,dfTrainNKey$madeBB,family = "binomial")

feat_names <- attr(logit$terms , "term.labels")

logitLasso = glmnet(X,y = dfTrainNKey$madeBB,family = "binomial",alpha = 1)
logitLassoFkey = glmnet(XFkey,y = dfTrainFKey$madeBB,family = "binomial",alpha = 1)
predict(logitLassoFkey,type = "coefficients",s = cv.lassoFkey$lambda.1se)
  
lassoLambdaBest = cv.lasso$lambda.1se
as.data.frame(summary(logitLasso$beta)) %>% 
  mutate(lambda = logitLasso$lambda[j],Name = feat_names[i]) %>% 
  filter(Name != "(Intercept)") %>% 
  ggplot(aes(x = log(lambda), y = x,color = Name)) +
  geom_line(size = 1) +
  ylab("Coefficient") + 
  geom_vline(xintercept = log(cv.lasso$lambda.min)) + 
  labs(title = "Lasso Coefficients", color = 'Feature') +
  xlab("Log of Lambda")
predict(logitLasso,s = lassoLambdaBest,type = "coefficients")
XTestNkey = model.matrix(madeBB ~ ., dfTestNKey)[, -1]
XTestNkey
predictedLasso <- ifelse(predict(logitLasso,XTestNkey,type="response", s=lassoLambdaBest) > .5,1,0)
table(predictedLasso,dfTest$madeBB) / length(dfTest$madeBB)



lassoRoc <-roc(predictor = as.vector(predictedLasso), response = dfTrain$madeBB)
lassoRoc$auc
table(ifelse(predictedLasso > .5,1,0),dfTrain$madeBB)
mean(ifelse(predictedLasso > .5,1,0) != dfTrain$madeBB)
table(ifelse(logit$fitted.values > .5,1,0),dfTrain$madeBB)
table(ifelse(logitStep$fitted.values > .5,1,0),dfTrain$madeBB)

mean(ifelse(predictedLasso > .4,1,0) != dfTest$madeBB)
table(ifelse(logit$fitted.values > .4,1,0),dfTrain$madeBB)
table(ifelse(logitStep$fitted.values > .4,1,0),dfTrain$madeBB)
as.data.frame(summary(logitLasso$beta)) %>% 
  mutate(lambda = logitLasso$lambda[j],Name = feat_names[i]) %>% 
  filter(Name != "(Intercept)") %>% 
  ggplot(aes(x = log(lambda), y = x,color = Name)) +
  geom_line(size = 1) +
  geom_vline(xintercept = log(lassoLambdaBest), col = "maroon")
  ylab("Coefficient") + 
  labs(title = "Lasso Coefficients", color = 'Feature') +
  xlab("Log of Lambda")
# Evaluate performance on the test set
table(ifelse(predict(object = logit,newdata = dfTest) > .5,1,0),dfTest$madeBB) / length(dfTest$madeBB)
mean(ifelse(predict(object = logit,newdata = dfTest) > .5,1,0)!= dfTest$madeBB)

table(ifelse(predict(object = logitStep,newdata = dfTest) > .4,1,0),dfTest$madeBB) / length(dfTest$madeBB)

XTestNkey = model.matrix(madeBB ~ ., dfTestNKey)[, -1]
table(ifelse(predict(object = logitLasso,newx = XTestNkey,s = ) > .4,1,0),dfTest$madeBB) / length(dfTest$madeBB)
table(ifelse(predict(object = logitStep,newdata = dfTest) > .4,1,0),dfTest$madeBB) / length(dfTest$madeBB)
mean(ifelse(predict(object = logitStep,newdata = dfTest) > .4,1,0) != dfTest$madeBB)
anova(logit,glm(madeBB ~ 1,family = "binomial",data =dfTrain)
      ,test = "Chisq")  
anova(logitStep,logit
      ,test = "Chisq") 
rf <- randomForest(factor(madeBB) ~ .,data = dfTrain,mtry = 4,)
varImpPlot(rf)

rfPredict = predict(rf,newdata = dfTest)
table(rfPredict,dfTest$madeBB) / length(dfTest$madeBB)
mean(rfPredict != dfTest$madeBB)


classQDA <- qda(factor(madeBB) ~.,data = dfTrainTr)

qdaPredict <- predict(classQDA,newdata = dfTestTr)
qdaPredict
mean(qdaPredict$class != dfTest$madeBB)

RSVM <- svm(factor(madeBB) ~ .,data = dfTrain,kernel = "radial")
RSVMPredict <- predict(RSVM,dfTest)
table(RSVMPredict,dfTest$madeBB)
mean(RSVMPredict != dfTest$madeBB)[1]
logitMat <- summary(logit)$coefficients[,1:2]
logitStepMat <- summary(logitStep)$coefficients[,1:2]
logitMat["danceability",1]

coefMat <- tibble(coefNames = feat_names)
lassoFeatNames <- rownames(logitMat)
lassoMat <- as.data.frame(summary(predict(logitLasso,s = cv.lasso$lambda.1se,type = "coefficients"))) %>% 
  mutate(Name = lassoFeatNames[i],lassoCoef = x) %>% 
  dplyr::select(Name,lassoCoef)
summary(predict(logitLasso,s = lassoLambdaBest,type = "coefficients"))
lassoMat
coefMat <- tibble(coefNames = feat_names,logitCoef = vector(length = length(feat_names),mode = "numeric"),logitSe = vector(length = length(feat_names),mode = "numeric"),
                logitStepCoef = vector(length = length(feat_names),mode = "numeric"),logitStepSe  =  vector(length = length(feat_names),mode = "numeric"),
                lassoCoef = vector(length = length(feat_names),mode = "numeric"))
                
for(name in coefMat$coefNames){
  coefMat[coefMat$coefNames == name,"logitCoef"] = ifelse(name %in% rownames(logitMat),logitMat[name,1],0)
  coefMat[coefMat$coefNames == name,"logitSe"] = ifelse(name %in% rownames(logitMat),logitMat[name,2],0)
  coefMat[coefMat$coefNames == name,"logitStepCoef"] = ifelse(name %in% rownames(logitStepMat),logitStepMat[name,1],0)
  coefMat[coefMat$coefNames == name,"logitStepSe"] = ifelse(name %in% rownames(logitStepMat),logitStepMat[name,2],0)
  coefMat[coefMat$coefNames == name,"lassoCoef"] = ifelse(name %in% lassoMat$Name,lassoMat[lassoMat$Name == name,2],0)
}
coefMat
nameCorrecter <- function(name){
  if(name %in% c("lassoCoef"))
    return("Lasso")
  else if(name %in% c("logitCoef","logitSe"))
    return("Logistic")
  else if(name %in% c("logitStepCoef","logitStepSe"))
    return("Logistic Step")
}

coefFiltMat <- coefMat %>% 
  pivot_longer(cols = c("lassoCoef","logitCoef","logitStepCoef"),names_to = "Model"
               ,values_to = "Coefficient") %>% 
  dplyr::select(-logitSe,-logitStepSe) %>% 
  mutate(Model = sapply(X = Model,FUN = nameCorrecter) )
seMat <- coefMat %>% 
  pivot_longer(cols = c("logitSe","logitStepSe"),names_to = "Model"
               ,values_to = "Se") %>% 
  dplyr::select(coefNames,Model,Se) %>% 
  mutate(Model = sapply(X = Model,FUN = nameCorrecter))

coefFiltMat %>% 
  left_join(seMat) %>% 
  mutate(Se = ifelse(is.na(Se),0,Se),Se = ifelse(Coefficient > 0,Se,-Se)) %>% 
  pivot_longer(cols = c("Coefficient","Se")) %>% 
  ggplot(aes(coefNames,value,fill = name)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values=c("#1DB954", "Black")) +
  theme_classic() + 
  ylab("Value") +
  xlab("Feature") + 
  theme(axis.text.y = element_text(size = 15)) + 
  guides(fill=guide_legend(title="Term")) +
  facet_wrap(Model~.)
sampleTree <- rpart(factor(madeBB) ~ .,data = dfTrain,)
par(mfrow = c(1,2))
varImpPlot(rf)
rpart.plot::rpart.plot(sampleTree)
text(sampleTree)
mean(ifelse(predict(sampleTree,newdata = dfTest)[,2] > .5,1,0) != dfTest$madeBB)
predict(sampleTree,newdata = dfTest)


as.data.frame(logitStepMat) %>% 
  mutate(names = rownames(logitStepMat)) %>% 
  ggplot(aes(x = names,y = Estimate,fill = name)) +
  geom_col() +
  geom_text(aes(label=round(`Std. Error`,digits = 2)), vjust=0) +
  coord_flip()

as.data.frame(logitStepMat) %>% 
    mutate(names = rownames(logitStepMat),`Std. Error` = ifelse(Estimate >= 0,`Std. Error`,-`Std. Error`)) %>% 
    pivot_longer(Estimate:`Std. Error`) %>% 
    mutate(name2 = relevel(x = factor(name),ref = "Std. Error"),)  %>% 
    ggplot(aes(x = names,y = value,fill = name2)) +
    geom_col(position = "stack") +
   guides(fill = guide_legend(reverse=TRUE)) + 
  scale_fill_manual(values=c("#1DB954", "Black")) +
  ylab("Value") +
  xlab("Feature") + 
  guides(fill=guide_legend(title="Term")) +
  ggtitle("Logistic Model Selected Via Backwards Selection") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
as.data.frame(logitMat) %>% 
  mutate(names = rownames(logitMat),`Std. Error` = ifelse(Estimate >= 0,`Std. Error`,-`Std. Error`)) %>% 
  pivot_longer(Estimate:`Std. Error`) %>% 
  mutate(name2 = relevel(x = factor(name),ref = "Std. Error"),)  %>% 
  ggplot(aes(x = names,y = value,fill = name2)) +
  geom_col(position = "stack") +
  guides(fill = guide_legend(reverse=TRUE)) + 
  scale_fill_manual(values=c("#1DB954", "Black")) +
  ylab("Value") +
  xlab("Feature") + 
  guides(fill=guide_legend(title="Term")) +
  ggtitle("Logistic Model with All Features Used") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
lassoMat %>% 
  ggplot(aes(x = Name,y = lassoCoef)) +
  geom_col(color = "Black",fill = "#1DB954") +
  ylab("Value") +
  xlab("Feature") +
  ggtitle("Logistic Model with Lasso Penalty .014 Used")
  

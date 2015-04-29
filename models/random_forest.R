### This script trains four models
### First it trains an RF, SVM and Elastic net regression
### using cross-validation to find optimal parameters
###
### Then, is uses the predictions from those models to build 
### an ensemble model

library("caret")
library("randomForest")
library("kernlab")
library("glmnet")
library("doParallel")
library("dplyr")
library("glmnet")

set.seed(1)
registerDoParallel(15)

## PREDICTION FUNCTION WRAPPERS

get_model_predictions = function(newdata, rf, svm, net){
  data.frame(rf=predict(rf, newdata=newdata, type="prob"),
             svn=predict(svm, newdata=newdata, type="prob"),
             net=predict(net, newx=newdata, s="lambda.min"))
}

get_ensemble_predictions = function(newdata, ensemble, ...){
  z = get_model_predictions(...)
  predict(ensemble, newx=as.matrix(z), s="lambda.min")
}

## GENERATE TRAINING SETS

# load full data
train_full = get(load("./data/census_training.Rdata")) %>%
  mutate(target=factor(target)) %>%
  as.data.frame %>%
  as.tbl

# first training set is used to train the RF, SVM and Elastic Net
train1 = train_full %>%
  group_by(target) %>% 
  sample_n(5000) %>% # downsampling for quick training
  ungroup 

x1 = train1 %>%
  select(-id, -target) %>%
  model.matrix(~.-1 , data=.) # create dummy variables

y1 = train1$target

# second training set is used to train the ensemble model
train2 = train_full %>%
  anti_join(train1 %>% select(id)) %>%
  group_by(target) %>% 
  sample_n(500) %>% # downsampling for quick training
  ungroup 

x2 = train2 %>%
  select(-id, -target) %>%
  model.matrix(~., data=.) # create dummy variables

y2 = train2$target

## TRAINING MODELS

# create control object for caret models
ctrl = trainControl(method="repeatedcv",
                    number=10,
                    repeats=1,
                    classProbs=T,
                    savePredictions=T)

# grid search for random forest
rf = train(x1,
           y1,
           method="rf",
           trControl=ctrl,
           ntrees=1000)

rf_importance = rf$finalModel %>%
  importance %>%
  as.data.frame %>%
  mutate(variable=row.names(.)) %>%
  arrange(-MeanDecreaseGini)

# grid search for svm 
svm = train(x1,
            y1,
            method="svmRadialCost",
            trControl=ctrl,
            scale=F)

# glmnet has a nice wrapper for CV
# no need to use caret
net = cv.glmnet(x1,
             y1,
             nfolds=10,
             keep=T,
             alpha=0.5,
             family="binomial",
             parallel=T)

net_coef = coef(net) %>%
  as.matrix %>%
  as.data.frame %>%
  mutate(variable=row.names(.)) %>%
  arrange(-abs(coef))

# create new predictors for ensemble models

z2 = data.frame(rf=predict(rf, newdata=x2, type="prob"),
                svn=predict(svm, newdata=x2, type="prob"),
                net=predict(net, newx=x2, s="lambda.min"))

ensemble = cv.glmnet(x2,
                     y2,
                     keep=T,
                     nfolds=10,
                     alpha=0.1,
                     lower.limit=0)

ensemble_coef = coef(net) %>%
  as.matrix %>%
  as.data.frame %>%
  mutate(model=row.names(.))
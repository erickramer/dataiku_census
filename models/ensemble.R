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
library("ROCR")
library("pROC")

set.seed(1)
registerDoParallel(15)

## PREDICTION FUNCTION WRAPPERS

get_model_predictions = function(newdata, rf, svm, net){
  data.frame(rf=predict(rf$finalModel, newdata=newdata, type="prob")[,2],
             svm=predict(svm$finalModel, newdata=newdata, type="prob")[,2],
             net=predict(net, newx=newdata, s="lambda.min", type="response")[,1])
}

get_ensemble_predictions = function(newdata, ensemble, ...){
  z = get_model_predictions(newdata, ...)
  p = predict(ensemble, newx=as.matrix(z), s="lambda.min", type="response")[,1]
  
  z %>%
    mutate(ensemble=p)
}

## GENERATE TRAINING SETS

# load full data
train_full = get(load("./data/census_training.Rdata")) %>%
  mutate(Target=factor(Target)) %>%
  select(-Cohort) %>%
  as.data.frame %>%
  as.tbl %>%
  group_by(Target) %>%
  sample_n(12382) # downsampling for quick training

# first training set is used to train the RF, SVM and Elastic Net
train1 = train_full %>%
  group_by(Target) %>% 
  sample_frac(0.9) %>% # use 90% for models, 10% for ensemble
  ungroup 

x1 = train1 %>%
  select(-id, -Target) %>%
  model.matrix(~.-1 , data=.) # create dummy variables

y1 = train1$Target

# second training set is used to train the ensemble model
train2 = train_full %>%
  anti_join(train1 %>% select(id)) 

x2 = train2 %>%
  select(-id, -Target) %>%
  model.matrix(~.-1, data=.)

y2 = train2$Target

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
  mutate(ElasticNetCoefficient=`1`) %>%
  select(-`1`) %>%
  arrange(-abs(ElasticNetCoefficient))

save(rf, svm, net, file="./data/models.Rdata")
save(net_coef, rf_importance, file="./data/importance_measures.Rdata")

# create new predictors for ensemble models

z2 = get_model_predictions(x2, rf=rf, svm=svm, net=net)

ensemble = cv.glmnet(as.matrix(z2),
                     y2,
                     keep=T,
                     nfolds=10,
                     alpha=0.1,
                     lower.limit=0,
                     family="binomial",
                     parallel=T)

ensemble_coef = coef(ensemble) %>%
  as.matrix %>%
  as.data.frame %>%
  mutate(model=row.names(.))

save(ensemble, file="./data/ensemble.Rdata")

## PREDICTING ON TESTING SET

testing_full = get(load("./data/census_testing.Rdata"))
  
x_testing = testing_full %>%
  select(-id, -Target, -Cohort) %>%
  model.matrix(~.-1 , data=.) # create dummy variables

y_testing = factor(testing_full$Target)

pred_testing = get_ensemble_predictions(x_testing, 
                                        ensemble=ensemble, 
                                        rf=rf, 
                                        svm=svm, 
                                        net=net)
# calculating AUCs
aucs = plyr::llply(pred_testing, 
                  function(x) roc(y_testing, x),
                  .parallel=T)

save(aucs, file="./data/aucs.Rdata")
save(aucs, file="./Rmd/data/aucs.Rdata")

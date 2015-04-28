library("caret")
library("randomForest")
library("kernlab")
library("glmnet")
library("doParallel")
library("dplyr")
library("glmnet")

set.seed(1)

registerDoParallel(10)

# load training data

train_full = get(load("./data/census_training.Rdata")) %>%
  mutate(target=factor(target))

train1 = train_full %>%
  sample_frac(0.02)

x1 = train1 %>%
  select(-id, -target) %>%
  model.matrix(~., data=.)

y1 = train1$target

train2 = train_full %>%
  anti_join(train1 %>% select(id)) %>%
  sample_frac(0.02)

x2 = train2 %>%
  select(-id, -target) %>%
  model.matrix(~., data=.)

y2 = train2$target

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

# create new predictors for ensemble models

z2 = data.frame(rf=predict(rf, newdata=x2, type="prob"),
                svn=predict(svm, newdata=x2, type="prob"),
                net=predict(net, newx=x2, s="lambda.min"))

ensemble = cv.glmnet(x2,
                     y2,
                     keep=T,
                     nfolds=10,
                     alpha=0.1)
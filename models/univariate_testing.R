library("dplyr")
library("magrittr")
library("pROC")
library("rms")
library("randomForest")

get_univariate_performance = function(x, target, m_full){
  
  m = lrm(target ~ ., data=data.frame(x=x))
  rf = randomForest(target ~ ., data=data.frame(x=x))

  data.frame(R2=m$stats["R2"],
             AUC=auc(roc(target, rf$votes[,1])))
}

load("./data/census_training.Rdata")

census_training %<>% 
  group_by(target) %>%
  sample_n(5000) %>%
  ungroup

## loop over all features and calculate error rate

performance = plyr::ldply(census_training %>% 
                              select(starts_with("V")),
                          get_univariate_performance, 
                          target=census_training$target,
                          .progress="text")

x = census_training %>% 
  select(starts_with("V")) %>%
  model.matrix(~.,data=.)

target = census_training$target

rf = randomForest(x, target)




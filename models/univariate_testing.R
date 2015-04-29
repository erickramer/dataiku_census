library("dplyr")
library("magrittr")
library("pROC")
library("rms")
library("randomForest")
library("doParallel")

get_univariate_performance = function(x, target){
  
  #m = lrm(target ~ ., data=data.frame(x=x))
  rf = randomForest(target ~ ., data=data.frame(x=x))

#   data.frame(R2=m$stats["R2"],
#              AUC=auc(roc(target, rf$votes[,1])))
  data.frame(AUC=auc(roc(target, rf$votes[,1])))
}

registerDoParallel(10)

load("./data/census_training.Rdata")

census_training = census_training %>% 
  group_by(Target) %>%
  sample_n(2000) %>% # downsample to make this faster
  ungroup

## loop over all features and calculate error rate



performance = plyr::ldply(census_training %>% 
                              select(-id) %>%
                              select(-Cohort) %>%
                              select(-Target),
                          get_univariate_performance, 
                          target=factor(census_training$Target),
                          .parallel=T) %>%
  mutate(Variable=`.id`) %>%
  select(-`.id`)

save(performance, file="./Rmd/data/univariate_performance.Rdata")

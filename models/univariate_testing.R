library("dplyr")
library("magrittr")
library("pROC")
library("rms")

get_univariate_performance_ = function(x, target, m_full){
  m = lrm(target ~ ., data=data.frame(x=x))

  data.frame(R2=m$stats["R2"])
}

get_univariate_performance = failwith(data.frame(R2=NA), 
                                      get_univariate_performance_)

load("./data/census_training.Rdata")

census_training %<>% sample_n(10000)

## loop over all features and calculate error rate

r2 = plyr::ldply(census_training %>% 
                              select(starts_with("V")),
                          get_univariate_performance, 
                          target=census_training$target,
                          .progress="text")


library("dplyr") # used for  improved syntax
library("data.table") # used for fast file reading
library("tidyr") # used for fast data-munging
library("ggvis") # used for plotting

# loading the data from the file
# and a bit of cleaning

meta_data = read_tsv("./data/us_census_full/meta_data.txt") 

census_training = fread("./data/us_census_full/census_income_learn.csv",
                        colClasses=ifelse(meta_data$Class=="continuous", 
                                          "numeric", 
                                          "character"))

census_testing = fread("./data/us_census_full/census_income_test.csv",
                       colClasses=ifelse(meta_data$Class=="continuous", 
                                         "numeric", 
                                         "character")) 
# reset data names
setnames(census_training, colnames(census_training), meta_data$Variable)
setnames(census_testing, colnames(census_testing), meta_data$Variable)

# combine two sets together 
# to ensure consistent preprocessing
census = rbind(census_training %>% mutate(Cohort="Training"),
               census_testing %>% mutate(Cohort="Testing")) %>%
  mutate(id=paste0("ID", 1:nrow(.)))

# quick look at the data
str(census_training)


# i'm going to split the data into catagorical and continous
# for the next couple cleaning steps

continuous_vars = meta_data %>%
  filter(Class=="continuous") %>%
  .$Variable

categorical_vars = meta_data %>%
  filter(Class=="categorical") %>%
  filter(Variable!="target") %>%
  .$Variable

## CLEANING CATAGORICAL VARIABLES 

# i'm looking at feature counts for catagorical
# making sure all entries have a reasonable frequency
# these frequencies are based solely on the training data

categorical_counts = census_training %>%
  select(one_of(categorical_vars)) %>%
  gather(Variable, Value) %>%
  group_by(Variable, Value) %>%
  summarize(n=n()) %>%
  ungroup %>%
  mutate(f=n/nrow(census_training)) %>%
  ungroup

# let's plot these frequencies to see what's going on
categorical_counts %>%
  as.data.frame %>%
  ggvis(~f, fill =~ Variable) %>%
  layer_histograms(width=0.01)

# we see that the DetailedIndustry, StateOfResidency, Occupation all have 
# a lot of rare varaibles

rare_categories = categorical_counts %>%
  filter(f < 0.05)

# I'm going to also delete two columns (UnemployReason, VeteranAdmin) 
# with a max frequency > 95%
# these columns have very little variation

frequent_categories = categorical_counts %>%
  group_by(Variable) %>%
  summarize(f=max(f)) %>%
  filter(f > 0.95)

categorical_cleaned = census %>%
  select(one_of(categorical_vars), id) %>%
  gather(Variable, Value, -id) %>%
  anti_join(frequent_categories) %>%
  anti_join(rare_categories) %>%
  spread(Variable, Value, fill="RARE")

## CLEANING CONTINUOUS VARIABLES 

# looking at the number of distinct values in the continuous variables
# and the number of non-zero entries
# CapitalLosses has very few nonzero entries, but I'll leave it for now

continuous_counts = census_training %>%
  select(one_of(continuous_vars)) %>%
  gather(variable, value) %>%
  group_by(variable) %>%
  summarize(n=n_distinct(value),
            n_nonzero=sum(value!=0))

# I'm adding a few indicator variables here 
# to see whether or not the person reported CapitalGains, Losses, etc
continuous_cleaned = census %>%
  select(one_of(continuous_vars), id) %>%
  mutate(FiledCapitalGains=CapitalGains!=0) %>%
  mutate(FiledCapitalLosses=CapitalLosses!=0) %>%
  mutate(FiledCapitalGains=CapitalGains!=0) %>%
  mutate(EarnedWages=WagePerHour!=0)

## CREATING FINAL DATASET

census = census %>%
  select(id, Target, Cohort) %>%
  mutate(Target=factor(Target)) %>%
  inner_join(catagorical_cleaned) %>%
  inner_join(continuous_cleaned)

census_training = census %>%
  filter(Cohort=="Training")

census_testing = census %>%
  filter(Cohort=="Testing")

save(census_training, file="./data/census_training.Rdata")
save(census_testing, file="./data/census_testing.Rdata")

save(rare_categories, file="./data/rare_catagories.Rdata")
save(frequent_categories, file="./data/frequent_catagories.Rdata")

library("dplyr") # used for  improved syntax
library("data.table") # used for fast file reading
library("tidyr") # used for fast data-munging
library("ggvis") # used for plotting

# loading the data from the file
# and a bit of cleaning

census_training = fread("./data/us_census_full/census_income_learn.csv") %>%
  mutate(target=V42) %>%
  select(-V42) %>%
  as.data.frame %>%
  as.tbl

# quick look at the data
str(census_training)

# getting a little meta-info about the variables
classes = data.frame(variable=colnames(census_training),
                     class=sapply(census_training, class),
                     stringsAsFactors=F)

# i'm going to split the data into catagorical and continous
# for the next couple cleaning steps

continuous_vars = classes %>%
  filter(class %in% c("integer", "numeric")) %>%
  .$variable

catagorical_vars = classes %>%
  filter(class=="character") %>%
  filter(variable!="target") %>%
  .$variable

# now i'm looking at feature counts for catagorical
# making sure all levels have a reasonable frequency

catagorical_counts = census_training %>%
  select(one_of(catagorical_vars)) %>%
  gather(variable, value) %>%
  group_by(variable, value) %>%
  summarize(n=n(),
            f=n/nrow(census_training))

# let's plot these frequencies to see what's going on
catagorical_counts %>%
  ggvis(~f, fill = ~variable) %>%
  layer_histograms(width=0.01)

# we see that V35, V34 and V33 have a lot of rare catagories
# these look like "country of birth" or something like that
# 
# V22-V23 also have a lot of rare catagories
# I'm going to set all catagories with freq < 1% to "rare"
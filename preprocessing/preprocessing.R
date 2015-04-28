library("dplyr") # used for  improved syntax
library("readr") # used for fast file reading
library("tidyr") # used for fast data-munging
library("ggvis") # used for plotting

convert_to_numeric = function(x) as.numeric(gsub("^ ", "", x))

# loading the data from the file
# and a bit of cleaning
census_training = read_csv("./data/us_census_full/census_income_learn.csv",
                           col_names=F) %>%
  mutate(target=X42, age=X1) %>% # I'm guessing X1 is age
  mutate(X25=convert_to_numeric(X25)) %>%
  mutate(X6=convert_to_numeric(X6)) %>% 
  select(-X42, -X1)


# now i'm looking at feature counts
# making sure all levels have a reasonable frequency
feature_counts = census_training %>%
  select(-target, -age, -X25) %>% # remove continuous variables and target
  gather(variable, value) %>%
  group_by(variable, value) %>%
  summarize(n=n(),
            f=n/nrow(census_training))

# let's plot these frequencies to see what's going on
feature_counts %>%
  ggvis(~f, fill = ~variable) %>%
  layer_histograms()

# we see that X5 
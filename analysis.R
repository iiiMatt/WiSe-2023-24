source("preprocessing.R")
source("utils.R")

# preprocessed file is returned as 
# "titanic_cleaned.Rds" 
# in working directory 
generate_preprocessed_data()

dataset = readRDS("titanic_cleaned.Rds")

# start of exercise 4)
# call functions from utils.R (exercise 2) 
# and interprete results 

getwd()

#'2a- i)
#'median, min, max, first quantile, third quantile, mean, standard deviation
#'describing a metric variable as an example the age

##############################################
titanic_age <- describe_metric(titanic_cleaned$Age)

#      Min  erstesQuartil Median    Mean        SD        drittesQuartil  Max   NAs
# 25% 0.42            21      30    29.37168    13.25292              35   80     0

#We can see that the youngest passenger was only 0.42 (less than 1 year old) and the oldest 80 years old.
#The average age on the ship is 30 with a standard derivation of 13.25. 



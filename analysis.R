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

#'2a- ii)
#'descriptive statistics for a categorial variable
#'as an example we can use the categorial variable 

titanic_deck <- describe_categorial(titanic_cleaned$Deck)

#Descriptive statistics for the factor titanic_cleaned$Deck (NA values removed) 
#Levels:                              A, B, C, D, E, F, G, T 
#Not available:                       687 of 891 values
#Statistical mode:                    C 
#Boltzmann's empirical entropy:       0.8453196 
#Phi dispersion for categorial data:  0.6775701 

#We can see that we have 8 different levels where of the mode is deck C. Interpreted it means that most of the
#passengers were located on deck C. However only 204 passengers data were available.

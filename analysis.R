source("preprocessing.R")
source("utils.R")

# preprocessed file is returned as 
# "titanic_cleaned.Rds" 
# in working directory 
generate_preprocessed_data()

dataset = readRDS("titanic_cleaned.Rds")

# start of exercise 4)
# call functions from utils.R (exercise 2) 
# and interpret results 

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

#--------------------------------------------------------------------------
# iii) 
# 2 categorical  variables
#--------------------------------------------------------------------------
str(dataset)
#survived, embarked, salutation, deck, side
# test regarding association among variables
#'alpha = 0.05 but did in total 4 tests 
#'bonferront adjustment: alpha_adj = 0.05/4 = 0.0125


#######################################################################
#survived and embarked 
surv_embarked <- chisq_test_plus_table(dataset$Survived, dataset$Embarked)
surv_embarked[1]
'kat_var2
kat_var1 Cherbourg Queenstown Southhampton
no         75         47          427
yes        93         30          217'
#'interesting: almost 4 out of 7 survive when embarked in Cherbourg
#'embarked in the other cities people who died exceed the survivors
#'in southhampton almost 2 out of 3 individuals
surv_embarked[2]
#'p-value = 1.77e-06 < alpha_adj
#'statistically significant evidence to support the alternative hypothesis
#'evidence that there is a significant relationship between 
#'embarkation and survival

#####################################################################
#salutation and survival
surv_salutation <- chisq_test_plus_table(dataset$Survived, dataset$Salutation)
surv_salutation[1]
'kat_var2
kat_var1 Master. Miss. Mr. Mrs.
    no       17    55 451   26
    yes      23   130  87  102'
#' around 7 out of 10 Miss. survived 
#' around 8 out of 20 Mrs. survived 
#' around 2 out of 10 Mr. survived
#' around 6 out of 10 Master 
#' 
#' possible:
#' Female passenger got helped to get to the boats.
#' Men with a title might as well. 
#' Most men died 
#'also: less female than men: when they are prioritized ratio goes up quicker
surv_salutation[2]
#'p-value = 2.2e-16 < alpha_adj
#'statistically significant evidence to support the alternative hypothesis
#'evidence that there is a significant relationship between 
#'salutation and survival 
#'Numbers in contengency show more survival of female passangers 
#'and likely death for a Mr.

############################################################################
#survival and deck
sum(is.na(dataset$Deck))
#too many NAs to proper analyze 
############################################################################
#survival and Side
sum(is.na(dataset$Side))
#too many NAs to proper analyze 

#--------------------------------------------------------------------------
# iv) 
# Metric and a Dichotomous Variable
#--------------------------------------------------------------------------
#'t-test to assess the difference in a metric variable
#'across two groups defined by a dichotomous variable
#'alpha = 0.05 but did in total 4 tests 
#'bonferront adjustment: alpha_adj = 0.05/4 = 0.0125

str(dataset)
#'useful metric variables:age and fare 
#'useful dichotomous variable: survived yes or not 

##################################################################
#Fare and Survival Status
calculateBivariateStats(dataset, "Fare", "Survived")
#mean and sd regarding groups of survivors or not
'$no
mean       sd 
22.11789 31.38821 

$yes
mean       sd 
48.39541 66.59700 '
#'the individuals who died paid in average much less for their tickets
#'than the ones who survived
#'since sd of survivers regarding fare is big 
#'apparently still some survivors with much cheaper or even much more expensive fare
#'SD of people who died is much smaller
#'seems like the people who paid the most got rescued first and barely died or  
#'even not at all
#'----------------------------
#Welch two sample t-test result:
# p-value = 2.699e-11 < alpha_adj
#' indicates that the true difference in means between the groups 
#' is not equal to zero
# as seen before reagrding the means: seems like it made a difference of how much money 
#'a person paid to embark the titanic
#'likely that the rich people and women got rescued first 
#'suits the society back then

##################################################################
#Age and Survival Status
calculateBivariateStats(dataset, "Age", "Survived")
'$no
mean       sd 
30.18488 12.65199 

$yes
mean       sd 
28.06629 14.08538 '
#'Mean of age in both groups around 30 which is young
#'still a difference of around 2 years between the twio groups
#'the survivors slightly younger 
#'might be cause by children who are usually rescued first?
#'low SD shows that in general the age group did not differ too much
#'
#'----------------------------
#Welch two sample t-test result:
# p-value = 0.02358 > alpha_adj = 0.0125
#' not enough evidence against the null hypothesis that the true difference 
#' in means between the groups is equal to zero
#' maybe there was not enough data 

# 2a - v)
visualisation_categorial_variable(titanic_cleaned)
number_survivor = sum(titanic_cleaned$Survived == "yes")# 342 Person survive
number_not_survivor = sum(titanic_cleaned$Survived == "no")# 549 Person died

# 32% of survivors were men
# 68% of survivors were women
# 85% of deaths were men
# 15% of deaths were women
# more men have died than women

# 40% of survivors were in class1
# 25% of survivors were in class2
# 35% of survivors were in class3
# 15% of deaths were in class1
# 18% of deaths were in class2
# 68% of deaths were in class3
# the most deadly class was class 3
# the least deadly class was class 1

# 63% of survivors were embarked in Southhampton
# 9% of survivors were embarked in Queenstown
# 27% of survivors were embarked in Cherbourg
# 1% of survivors were embarked in a place we dont know
# 78% of deaths were embarked in Southhampton
# 9% of deaths were embarked in Queenstown
# 14% of deaths were embarked in Cherbourg
# the embarcation place with the highest death rate were Southhampton

# 2a - vi)
Mosaikplot (titanic_cleaned$Pclass , titanic_cleaned$Sex)
# the majority of people(man as woman) were in class 3 and there 
# were more men than women

Mosaikplot (titanic_cleaned$Embarked , titanic_cleaned$Sex)
# the majority of people(man as woman) had embarked in Southhampton and there
# were more men than women

Mosaikplot (titanic_cleaned$Embarked , titanic_cleaned$Pclass)
# the majority of people who embarked at southhampton or Queenstown were in class 3,
# but at Cherbourg almost half the passengers were in class 1

Boxplots_with_Trendline( titanic_cleaned$Pclass , titanic_cleaned$Fare)
# the correlation between ticket price and class is positive
# (the more you want to be in a good class, the more you have to pay)

Boxplots_with_Trendline( titanic_cleaned$Sex , titanic_cleaned$Fare)
# women generally paid more for tickets than men

Boxplots_with_Trendline( titanic_cleaned$Survived , titanic_cleaned$Fare)
# in general, those who survived were those who paid the highest ticket prices.
# this is in line with the fact that the survival rate in class 1 was high and the death rate low.  

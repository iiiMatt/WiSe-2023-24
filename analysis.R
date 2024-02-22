source("preprocessing.R")
source("utils.R")

#' preprocessed file is returned as 
#' "titanic_cleaned.Rds" 
#' in working directory 
generate_preprocessed_data()  

dataset = readRDS("titanic_cleaned.Rds")

#' start of exercise 4)
#' call functions from utils.R (exercise 2) 
#' and interprete results 

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
surv_embarked = chisq_test_plus_table(dataset$Survived, dataset$Embarked)
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
surv_salutation = chisq_test_plus_table(dataset$Survived, dataset$Salutation)
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
surv_salutation[2]
#'p-value = 2.2e-16 < alpha_adj
#'statistically significant evidence to support the alternative hypothesis
#'evidence that there is a significant relationship between 
#'embarkation and survival 
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

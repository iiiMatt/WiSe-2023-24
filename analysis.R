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
# test regarding association among variables, alpha = 0.05
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
#'p-value = 1.77e-06 < alpha 
#'statistically significant evidence to support the alternative hypothesis
#'evidence that there is a significant relationship between 
#'embarkation and survival

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
#'p-value = 2.2e-16 < alpha 
#'statistically significant evidence to support the alternative hypothesis
#'evidence that there is a significant relationship between 
#'embarkation and survival 
#'Numbers in contengency show more survival of female passangers 
#'and likely death for a Mr.

#survival and deck
#survival and Side
surv_deck = chisq_test_plus_table(dataset$Survived, dataset$Deck)
#warning
sum(is.na(dataset$Deck))
sum(is.na(dataset$Side))
#too many NAs to proper analyze 

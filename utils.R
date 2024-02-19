# contribution by Mia
# descriptive bivariate statistics
# for the relationship between two categorical variables

# function that outputs chi squared test result and
# contingency table of two categorical variables

chisq_test_plus_table <- function(kat_var1, kat_var2) {
  # check if both variables are categorical (purpose of the excercise)
  if (!is.factor(kat_var1) || !is.factor(kat_var2)) {
    stop("Both variables need to be categorical.")
  }
  # contingency table for output and test
  contingency_table <- table(kat_var1, kat_var2)

  # chi-squared-test to output results
  chi_squared_result <- chisq.test(contingency_table)

  #cat output to explain results/ output plus result on console
  
  cat("The results contain a contingency table and a chi squared test regarding the following hypotheses:\n
      Null Hypothesis (H0): There is no association between the two categorical variables. \n
      Alternative Hypothesis (H1): There is an association between the two categorical variables.\n
      If p-value < alpha: Reject the null hypothesis. \n
      If p-value >= alpha: Fail to reject the null hypothesis.")

  
  return( list(
    contingency_table = contingency_table, 
    chi_squared_result = chi_squared_result))
}

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

  # character vectors to explain the output of the function
  hypothesis_explanation <- c(
    "The results contain a contingency table and a
        chi squared test regarding the following hypotheses:",
    "Null Hypothesis (H0):
        There is no association between the two categorical variables.",
    "Alternative Hypothesis (H1):
        There is an association between the two categorical variables."
  )
  result_explanation <- c(
    "If p-value < alpha: Reject the null hypothesis.",
    "If p-value >= alpha: Fail to reject the null hypothesis."
  )
  # return results in list
  return(list(
    hypothesis_explanation,
    contingency_table = contingency_table,
    chi_squared_result = chi_squared_result,
    result_explanation
  ))
}

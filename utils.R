#' Calculate Descriptive and Inferential Statistics
#' for a Metric and a Dichotomous Variable
#'
#' This function calculates and outputs the mean, standard deviation,
#' and performs a t-test to assess the difference in a metric variable
#' across two groups defined by a dichotomous variable.
#' It's useful for understanding how the metric variable differs between
#' two groups, such as comparing the average fare paid by passengers who
#' survived vs. those who did not.
#'
#' @param data A dataframe containing the dataset.
#' @param metricVar The name of the metric variable as a string.
#' @param dichotomousVar The name of the dichotomous
#'  variable as a string.
#'
#' @example calculateBivariateStats(df, "Fare", "Survived")
calculateBivariateStats <- function(data, metricVar, dichotomousVar) {
    # Ensure the dichotomous variable is a factor
    data[[dichotomousVar]] <- as.factor(data[[dichotomousVar]])

    # Split data based on dichotomous variable
    splitData <- split(data[[metricVar]], data[[dichotomousVar]])

    # Calculate descriptive statistics
    statsList <- lapply(splitData, function(x) {
        c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))
    })

    # Perform t-test
    tTestResult <- t.test(data[[metricVar]] ~ data[[dichotomousVar]], data = data)

    # Output results
    cat("Descriptive Statistics:\n")
    print(statsList)
    cat("\nT-Test Results:\n")
    print(tTestResult)
}

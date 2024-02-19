source("helpers.R")

#' Prints descriptive statistics for a categorial variable x and additional
#' ones for ordered factors.
#' @param x A factor (may be ordered).
#' @param name The name of the factor. By default whatever was passed to x so
#' for describe_categorial(data$Pclass) name would default to "data$Pclass".
#' @param na.rm Indicates if NA values should be removed before calculation.
describe_categorial <- function(
    x, name = deparse(match.call()$x),
    na.rm = TRUE) {
    mode <- statistical_mode(x, na.rm = na.rm)
    entropy <- empirical_entropy(x, na.rm = na.rm)
    phi <- phi_dispersion(x, na.rm = na.rm)
    cat("\n")
    cat(
        "Descriptive statistics for the factor", name,
        ifelse(na.rm, "(NA values removed)", ""), "\n"
    )
    cat(
        "Levels:                             ",
        paste(levels(x), collapse = ifelse(is.ordered(x), " < ", ", ")), "\n"
    )
    cat(
        "Not available:                      ",
        sum(is.na(x)), "of", length(x), "values\n"
    )
    cat("Statistical mode:                   ", mode, "\n")
    cat("Boltzmann's empirical entropy:      ", entropy, "\n")
    cat("Phi dispersion for categorial data: ", phi, "\n")
    if (is.ordered(x)) {
        med <- levels(x)[quantile(x, 0.5, na.rm = na.rm, type = 1)]
        q1 <- levels(x)[quantile(x, 0.25, na.rm = na.rm, type = 1)]
        q3 <- levels(x)[quantile(x, 0.75, na.rm = na.rm, type = 1)]
        cat("Lower quartile:                     ", q1, "\n")
        cat("Median:                             ", med, "\n")
        cat("Upper quartile:                     ", q3, "\n")
    }
  
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

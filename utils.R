source("helpers.R")
library(gridExtra)
library(ggplot2)
library(dplyr)


describe_metric <- function(x){
  data.frame(Min = min(x, na.rm=T), erstesQuartil =quantile(x, 0.25, na.rm=T) , 
             Median = median(x, na.rm=T), Mean =mean(x, na.rm=T), 
             SD = sd(x, na.rm=T), drittesQuartil =quantile(x, 0.75, na.rm=T),
             Max = max(x, na.rm=T),
             NAs = sum(is.na(x))
             )
}

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
  

#' Calcualates and prints the results of descriptive bivariate statistics of a 
#' contengency table  and a chi squared test for two categorical variables 
#' @param kat_var1 A kategorical variable
#' @param kat_var2 A second kategorical variable with the same length as the first 
chisq_test_plus_table <- function(kat_var1, kat_var2) {
  # check if both variables are categorical (purpose of the excercise)
  if (!is.factor(kat_var1) || !is.factor(kat_var2)) {
    stop("Both variables need to be categorical.")
  }
  # contingency table for output and test
  contingency_table <- table(kat_var1, kat_var2)

  # chi-squared-test to output results
  chi_squared_result <- chisq.test(contingency_table)

  #cat output to explain results/ output plus print results on console
  
  cat("The results contain a contingency table and a chi squared test regarding the following hypotheses:\n
      Null Hypothesis (H0): There is no association between the two categorical variables. \n
      Alternative Hypothesis (H1): There is an association between the two categorical variables.\n
      If p-value < alpha: Reject the null hypothesis. \n
      If p-value >= alpha: Fail to reject the null hypothesis.")
  print(contingency_table)
  print(chi_squared_result)
  
  return( list(
    contingency_table = contingency_table, 
    chi_squared_result = chi_squared_result))
}

#2a - vi)
#Additional functions suitable for description and visualization
Mosaikplot <- function(var1, var2){
  mosaicplot(table(var1, var2 ), main = "Mosaic Plot", shade = TRUE, xlab = deparse(substitute(var1)) , ylab = deparse(substitute(var2)))
}

Boxplots_with_Trendline <- function(var1, var2) {
  plot(var1, var2, main = "Boxplot with Trendline",
       xlab = deparse(substitute(var1)), ylab = deparse(substitute(var2)))
  abline(lm(var2 ~ var1), col = "red")
}




#' Visualize Categorical Variables Relation to Survival on Titanic
#'
#' This function creates visualizations for the relationship between
#' survival and three categorical variables:
#' Passenger Class (Pclass), Sex, and Embarkation Point (Embarked)
#'
#' @param df A dataframe
#' @return Returns a grid of three plots visualizing the survival rates by
#' Passenger Class, Sex, and Embarkation Point.
visualize_categorical_variables <- function(df) {
    df$Embarked <- factor(df$Embarked, exclude = NULL)

    levels(df$Embarked)[is.na(levels(df$Embarked))] <- "Unknown"

    # Plot for Pclass
    plot_pclass <- ggplot(df, aes(x = Pclass, fill = Survived)) +
        geom_bar(position = "fill") +
        labs(title = "Survival Rate by Passenger Class",
             x = "Passenger Class", y = "Survival Rate") +
        scale_fill_brewer(palette = "Set1") +
        theme_minimal()

    # Plot for Sex
    plot_sex <- ggplot(df, aes(x = Sex, fill = Survived)) +
        geom_bar(position = "fill") +
        labs(title = "Survival Rate by Sex", x = "Sex", y = "Survival Rate") +
        scale_fill_brewer(palette = "Set2") +
        theme_minimal()

    # Plot for Embarked (Na is "Unknown")
    plot_embarked <- ggplot(df, aes(x = Embarked, fill = Survived)) +
        geom_bar(position = "fill") +
        labs(title = "Survival Rate by Embarkation Point",
             x = "Embarkation Point", y = "Survival Rate") +
        scale_fill_brewer(palette = "Set3") +
        theme_minimal()

    # Display all plots on the same page
    grid.arrange(plot_pclass, plot_sex, plot_embarked, nrow = 3)
}



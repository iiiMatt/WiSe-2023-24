source("helpers.R")
library(gridExtra)
library(ggplot2)
library(dplyr)

#' Descriptive statistics for a metric variable
#'
#' Returns some descriptive statistics for a metric variable in form of a
#' dataframe for simple formatting. All NA values are removed before
#' calculation.
#'
#' Task 2a i.
#' @param x A metric variable.
#' @return A one row dataframe with the different statistics in columns.
describe_metric <- function(x) {
    data.frame(
        Min = min(x, na.rm = T), erstesQuartil = quantile(x, 0.25, na.rm = T),
        Median = median(x, na.rm = T), Mean = mean(x, na.rm = T),
        SD = sd(x, na.rm = T), drittesQuartil = quantile(x, 0.75, na.rm = T),
        Max = max(x, na.rm = T),
        NAs = sum(is.na(x))
    )
}

#' Descriptive statistics for a categorial variable
#'
#' Prints descriptive statistics for a categorial variable x and additional
#' ones for ordered factors. Namely the calculated statistics are the
#' statistical mode, empirical entropy and phi measure of dispersion and
#' optionally the median and quartiles.
#'
#' Task 2a ii.
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

#' Bivariate descriptive statistics for categorial variables
#'
#' Calcualates and prints the results of descriptive bivariate statistics of a
#' contengency table  and a chi squared test for two categorical variables.
#'
#' Task 2a iii.
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

    # cat output to explain results/ output plus print results on console

    cat("The results contain a contingency table and a chi squared test regarding the following hypotheses:\n
      Null Hypothesis (H0): There is no association between the two categorical variables. \n
      Alternative Hypothesis (H1): There is an association between the two categorical variables.\n
      If p-value < alpha: Reject the null hypothesis. \n
      If p-value >= alpha: Fail to reject the null hypothesis.")
    print(contingency_table)
    print(chi_squared_result)

    return(list(
        contingency_table = contingency_table,
        chi_squared_result = chi_squared_result
    ))
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
#' Task 2a iv.
#'
#' @param data A dataframe containing the dataset.
#' @param metricVar The name of the metric variable as a string.
#' @param dichotomousVar The name of the dichotomous
#'  variable as a string.
#'
#' @example calculateBivariateStats(df, "Fare", "Survived")
#' @export
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

#' Visualize Categorical Variables Relation to Survival on Titanic
#'
#' This function creates visualizations for the relationship between
#' survival and three categorical variables:
#' Passenger Class (Pclass), Sex, and Embarkation Point (Embarked)
#'
#' Task 2a v.
#'
#' @param df A dataframe
#' @return Returns a grid of six plots visualizing the survival rates by
#' Passenger Class, Sex, and Embarkation Point.
visualisation_categorial_variable <- function(df){
  
  # I calculate the relative frequency of the  survivors according to the class
  # they have. 
  
  class1_suv <- sum(df$Pclass == 1 & df$Survived == "yes") / sum(df$Survived == "yes")
  class2_suv <- sum(df$Pclass == 2 & df$Survived == "yes") / sum(df$Survived == "yes")
  class3_suv <- sum(df$Pclass == 3 & df$Survived == "yes") / sum(df$Survived == "yes")
  
  # I calculate the Relative frequency of the  survivors according to the sex
  # they have. 
  
  survivemen <- sum(df$Sex == "male" & df$Survived == "yes") / sum(df$Survived == "yes")
  survivewomen <- sum(df$Sex == "female" & df$Survived == "yes") / sum(df$Survived == "yes")
  
  # I calculate the relative frequency of the  survivors according to the embarked place 
  # they have. 
  
  embarkedSouthhampton_suv <- sum(df$Embarked == "Southhampton" & df$Survived == "yes") / sum(df$Survived == "yes")
  embarkedQueenstown_suv <- sum(df$Embarked == "Queenstown" & df$Survived == "yes") / sum(df$Survived == "yes")
  embarkedCherbourg_suv <- sum(df$Embarked == "Cherbourg" & df$Survived == "yes") / sum(df$Survived == "yes")
  
  # In the column i replace the NA by an empty space. Then I count the lines whose content has length 0
  # and I calculate their relative frequenty of the  survivors according that we dont know where they embarked .
  
  df$Embarked[is.na(df$Embarked)] <- ""
  embarkedUnknown_suv <- sum(nchar(as.character(df$Embarked)) == 0 & df$Survived == "yes")/ sum(df$Survived == "yes")
  
  # here i create  dataframes for the survivors variables with two columns (the variable and relative frequency) 
  
  df_class_suv <- data.frame(Pclass = c("Class1", "Class2", "Class3"),
                         relativeHaeufigkeit = c(class1_suv , class2_suv , class3_suv))
  df_sex_suv <- data.frame(Sex = c("Man", "Woman"),
                       relativeHaeufigkeit = c(survivemen , survivewomen))
  df_embarked_suv <- data.frame(Embarked = c("South", "Queen" , "Cher", "Unknown"),
                            relativeHaeufigkeit = c(embarkedSouthhampton_suv , embarkedQueenstown_suv ,embarkedCherbourg_suv, embarkedUnknown_suv))
  
  # I construct  histograms showing the number of people who survive according to the variable
  
  plot_sex_suv <- ggplot(data = df_sex_suv, aes(x = factor(Sex, Sex), y = relativeHaeufigkeit, fill = Sex)) +
    labs(title = "Survival Rate by Passenger Sex", x = "Sex", y = "relative Frequencies") +
    geom_bar(stat = "identity") +
    geom_text(aes(label = round(relativeHaeufigkeit, 2)), nudge_y = 0.05) +
    scale_fill_brewer(palette = "Set1") +
    theme_minimal()
  
  plot_Embarked_suv <- ggplot(data = df_embarked_suv, aes(x = factor(Embarked, Embarked), y = relativeHaeufigkeit, fill = Embarked)) +
    labs(title = "Survival Rate by Embarked Place", x = "Embarked", y = "relative Frequencies") +
    geom_bar(stat = "identity") +
    geom_text(aes(label = round(relativeHaeufigkeit, 2)), nudge_y = 0.05) +
    scale_fill_brewer(palette = "Set1") +
    theme_minimal()
  
  plot_class_suv <- ggplot(data = df_class_suv, aes(x = factor(Pclass, Pclass), y = relativeHaeufigkeit, fill = Pclass)) +
    labs(title = "Survival Rate by Passenger Class", x = "Pclass", y = "relative Frequencies") +
    geom_bar(stat = "identity") +
    geom_text(aes(label = round(relativeHaeufigkeit, 2)), nudge_y = 0.05) +
    scale_fill_brewer(palette = "Set1") +
    theme_minimal()
  
  
  # I calculate the relative frequency of the  dead persons according to the class
  # they have.
  
  class1_died <- sum(df$Pclass == 1 & df$Survived == "no") / sum(df$Survived == "no")
  class2_died <- sum(df$Pclass == 2 & df$Survived == "no") / sum(df$Survived == "no")
  class3_died <- sum(df$Pclass == 3 & df$Survived == "no") / sum(df$Survived == "no")
  
  # I calculate the relative frequency of the  dead persons according to the sex
  # they have
  
  diedmen <- sum(df$Sex == "male" & df$Survived == "no") / sum(df$Survived == "no")
  diedwomen <- sum(df$Sex == "female" & df$Survived == "no") / sum(df$Survived == "no")
  
  # I calculate the relative frequency of the  dead persons according to the the embarked place
  # they have 
  
  embarkedSouthhampton_died <- sum(df$Embarked == "Southhampton" & df$Survived == "no") / sum(df$Survived == "no")
  embarkedQueenstown_died <- sum(df$Embarked == "Queenstown" & df$Survived == "no") / sum(df$Survived == "no")
  embarkedCherbourg_died <- sum(df$Embarked == "Cherbourg" & df$Survived == "no") / sum(df$Survived == "no")
  
  # In the column i replace the NA by an empty space. Then I count the lines whose content has length 0
  # and I calculate their relative frequenty of the  dead person according that we dont know where they embarked .
  
  df$Embarked[is.na(df$Embarked)] <- ""
  embarkedUnknown_died <- sum(nchar(as.character(df$Embarked)) == 0 & df$Survived == "no")/ sum(df$Survived == "no")
  
  # here i create  dataframes for the dead persons variables with two columns (the variable and relative frequency)
  
  df_class_died <- data.frame(Pclass = c("Class1", "Class2", "Class3"),
                             relativeHaeufigkeit = c(class1_died , class2_died , class3_died))
  df_sex_died <- data.frame(Sex = c("Man", "Woman"),
                           relativeHaeufigkeit = c(diedmen , diedwomen))
  df_embarked_died <- data.frame(Embarked = c("South", "Queen" , "Cher", "Unknown"),
                                relativeHaeufigkeit = c(embarkedSouthhampton_died , embarkedQueenstown_died ,embarkedCherbourg_died, embarkedUnknown_died))
  
  # I construct  histograms showing the number of people who died according to the variable
  
  plot_sex_died <- ggplot(data = df_sex_died, aes(x = factor(Sex, Sex), y = relativeHaeufigkeit, fill = Sex)) +
    labs(title = "Death Rate by Passenger Sex", x = "Sex", y = "relative Frequencies") +
    geom_bar(stat = "identity") +
    geom_text(aes(label = round(relativeHaeufigkeit, 2)), nudge_y = 0.05) +
    scale_fill_brewer(palette = "Set1") +
    theme_minimal()
  
  plot_Embarked_died <- ggplot(data = df_embarked_died, aes(x = factor(Embarked, Embarked), y = relativeHaeufigkeit, fill = Embarked)) +
    labs(title = "Death Rate by Embarked Place", x = "Embarked", y = "relative Frequencies") +
    geom_bar(stat = "identity") +
    geom_text(aes(label = round(relativeHaeufigkeit, 2)), nudge_y = 0.05) +
    scale_fill_brewer(palette = "Set1") +
    theme_minimal()
  
  plot_class_died <- ggplot(data = df_class_died, aes(x = factor(Pclass, Pclass), y = relativeHaeufigkeit, fill = Pclass)) +
    labs(title = "DeathRate by Passenger Class", x = "Pclass", y = "relative Frequencies") +
    geom_bar(stat = "identity") +
    geom_text(aes(label = round(relativeHaeufigkeit, 2)), nudge_y = 0.05) +
    scale_fill_brewer(palette = "Set1") +
    theme_minimal()
  
  # we display all the histograms on the same page 
  
  result <- grid.arrange(plot_sex_suv, plot_class_suv, plot_Embarked_suv, plot_sex_died ,plot_class_died , plot_Embarked_died , nrow = 2)
  
  return(result)
  
  
}

#' Mosaicplot
#'
#' The function takes two variables, var1 and var2, as inputs. It then creates a mosaic plot using these variables
#'
#' Task 2a vi.
#' @param var1 One variable
#' @param var2 Another variable
#' @return a mosaicplot
Mosaikplot <- function(var1, var2) {
    mosaicplot(table(var1, var2), main = "Mosaic Plot", shade = TRUE, xlab = deparse(substitute(var1)), ylab = deparse(substitute(var2)))
}

#' Boxplots with Trendline
#'
#' The function takes two variables, var1 and var2, as inputs. It then creates a boxplot of var2 against var1, with a trendline added.
#'
#' Task 2a vi.
#' @param var1 One variable
#' @param var2 Another variable
#' @return a boxplot with trendlinie
Boxplots_with_Trendline <- function(var1, var2) {
    plot(var1, var2,
        main = "Boxplot with Trendline",
        xlab = deparse(substitute(var1)), ylab = deparse(substitute(var2))
    )
    abline(lm(var2 ~ var1), col = "red")
}

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




library(gridExtra)
library(ggplot2)
library(dplyr)

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

df <- readRDS("titanic_cleaned.Rds")
visualize_categorical_variables(df)


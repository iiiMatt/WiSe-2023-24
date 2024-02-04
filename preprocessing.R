#' Extract Salutations
#'
#' Extracts the salutations from the name column and creates a new column
#' with the extracted salutations in the dataset.
#'
#' @param dataset
extract_salutations <- function(dataset) { }

encode_variables <- function(dataset) {
  dataset$Sex <- factor(dataset$Sex, levels = c("male", "female"), labels = c("male", "female"))
  dataset$Survived <- factor(dataset$Survived, levels = c(0,1), labels = c("no", "yes"))
  dataset$Embarked <- factor(dataset$Embarked, levels = c("C", "Q", "S"), labels = c("Cherbourg", "Queenstown", "Southhampton"))
  dataset$Pclass <- factor(data[["Pclass"]], levels = c(3, 2, 1), ordered = TRUE)
}

infere_age <- function(dataset) { }

cleanup <- function(dataset) { 
  dataset <- subset(dataset, select = -PassengerId)
  dataset <- subset(dataset, select = -Name)
  dataset <- subset(dataset, select = -Ticket)
  dataset <- subset(dataset, select = -Cabin)
}


#' Generate Preprocessed Data
#'
#' Generates the preprocessed data from the `titanic.csv` by
#' running all the preprocessing functions.
#'
#' @returns preprocessed data
generate_preprocessed_data <- function() {
    dataset <- read.csv("titanic.csv")

    extract_salutations(dataset)
    encode_variables(dataset)
    infere_age(dataset)
    cleanup(dataset)

    dataset
}

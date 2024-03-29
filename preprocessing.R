#' Extract Salutations
#'
#' Extracts the salutations from the name column and creates a new column
#' with the extracted salutations in the dataset.
#'
#' Task 1
#'
#' @param dataset The titanic dataset.
#' @return The same dataset with the additional column "Salutation".
extract_salutations <- function(dataset) {
    SALUTS <- c(
        "Mr.", "Mrs.", "Miss.", "Master.",

        # Mr.
        "Dr.", "Rev.", "Col.", "Sir.", "Major.", "Don.", "Capt.", "Jonkheer.",

        # Mrs.
        "Mme.", "the Countess.", "Lady.",

        # Miss.
        "Ms.", "Mlle."
    )

    MAPPING <- list(

        # Later on for the age inferring, it'll be easier and more accurate
        # this way (so by converting Dr., Rev., etc... to Mr.)
        "Mr." = "Mr.",
        "Dr." = "Mr.",
        "Rev." = "Mr.",
        "Col." = "Mr.",
        "Sir." = "Mr.",
        "Major." = "Mr.",
        "Don." = "Mr.",
        "Capt." = "Mr.",
        "Jonkheer." = "Mr.",
        "Mrs." = "Mrs.",
        "Mme." = "Mrs.",
        "the Countess." = "Mrs.",
        "Lady." = "Mrs.",
        "Miss." = "Miss.",
        "Ms." = "Miss.",
        "Mlle." = "Miss.",
        "Master." = "Master."
    )

    # Used salutations: Mr., Mrs., Miss. and Master.

    # Rev. -> Typically, a Protestant minister will be addressed as "Reverend"
    #         or in some cases "Pastor." (will be Mr.)
    # Col. -> Colonel (will be Mr.)
    # Major. -> Army Major (will be Mr.)
    # Don. -> Mr.
    # Capt. -> Captain (will be Mr.)
    # Jonkheer. -> "Jonkheer" is a Dutch honorific that is used
    #              for the lowest rank of the untitled nobility.
    #              It is often translated as
    #              "Esquire" or "Young Lord" in English. (will be Mr.)

    # Mme. -> madam, french equivalent of Mrs.
    # the Countess. -> could be Lady. (will be Mrs.)

    # Ms. -> Miss.
    # Mlle. -> Mademoiselle, traditional alternative for an unmarried woman,
    #          so basically Miss.

    # Master. -> used for boys and young men


    names <- dataset[["Name"]]
    salutations <- rep_len("", length.out = length(names))

    for (salut in SALUTS) {
        salutations[grepl(salut, names, fixed = TRUE)] <- MAPPING[[salut]]
    }


    dataset[["Salutation"]] <- factor(salutations)

    dataset
}

#' Encode Variables
#'
#' Turns the columns "Sex", "Survived" and "Embarked" into a
#' factor and "Pclass" into an ordered factor.
#'
#' Task 1
#' @param dataset The titanic dataset.
#' @return The same dataset with the above mentioned columns as factors.
encode_variables <- function(dataset) {
    dataset$Sex <- factor(dataset$Sex,
        levels = c("male", "female"),
        labels = c("male", "female")
    )
    dataset$Survived <- factor(dataset$Survived,
        levels = c(0, 1),
        labels = c("no", "yes")
    )
    dataset$Embarked <- factor(dataset$Embarked,
        levels = c("C", "Q", "S"),
        labels = c("Cherbourg", "Queenstown", "Southhampton")
    )
    dataset$Pclass <- factor(dataset[["Pclass"]],
        levels = c(3, 2, 1),
        ordered = TRUE
    )
    dataset
}

#' Infer Age
#'
#' Fills in the missing ages in the dataset using the salutations
#'
#' Task 1
#'
#' @param dataset The titanic dataset.
#' @return The same dataset with a mutated "Age" column
infer_age <- function(dataset) {
    SALUTS <- c("Mr.", "Mrs.", "Miss.", "Master.")
    salutations <- dataset[["Salutation"]]
    age <- dataset[["Age"]]

    for (salut in SALUTS) {
        inferred <- median(age[salutations == salut], na.rm = TRUE)
        dataset[["Age"]][is.na(age) & (salutations == salut)] <- inferred
    }

    dataset
}

cleanup <- function(dataset) {
    dataset <- subset(dataset, select = -PassengerId)
    dataset <- subset(dataset, select = -Name)
    dataset <- subset(dataset, select = -Ticket)
    dataset <- subset(dataset, select = -Cabin)
    dataset
}

#' Extract Cabin Data
#'
#' Deck (which is the letter the cabin starts with) and Side (which is starboard
#' for odd and port for even cabin numbers) are extracted from Cabin and encoded
#' as factors.
#'
#' @details
#' Task 1
#'
#' In the event that a combination of cabins is listed for each a group
#' of multiple people the cabins are distributed arbitrarily among the group.
#' Example: If there are four people in the dataset that have "A11 B12 C13" as
#' the cabin, these four people will be assigned "A11" "B12" "C13" "A11". This
#' way the number of people with a cabin can be guaranteed to be the same as in
#' the original dataset and the distribution of cabins should roughly match the
#' original data.
#'
#' @param dataset The original dataset
#' @return The same dataset with the additional columns Deck and Side
extract_cabin_data <- function(dataset) {
    dataset[["Deck"]] <- NA
    dataset[["Side"]] <- NA
    cabins <- as.character(dataset[["Cabin"]])


    # deciding which cabin to use if there are multiple cabins

    split_cabins <- strsplit(cabins, " ")
    # contains all cabin entries that list more than one cabin
    multiple_cabins <- unique(cabins[grepl(" ", cabins, fixed = TRUE)])
    # the number of cabins in each row of the dataset
    lengths <- sapply(split_cabins, length)
    # enumerate occurrences of each distinct element
    occurrences <- ave(seq_along(cabins), cabins, FUN = function(x) match(x, x))
    # if you reach the end of the list start again with the first element
    occurrences <- (occurrences - 1) %% lengths + 1
    # in every row with only one cabin we pick that first cabin
    indices <- ifelse(cabins %in% multiple_cabins, occurrences, 1)
    # set each cabins[index] to split_cabins[index][indices[index]] which
    # means if multiple values are present pick the one specified in indices
    table <- sapply(split_cabins, "[", indices)
    cabins <- table[row(table) == col(table)]


    # extracting deck and side from the cabin

    dataset[["Deck"]] <- factor(gsub("[[:digit:]]", "", cabins))
    # the numeric value 1 will be coerced to TRUE and 0 to FALSE
    dataset[["Side"]] <- factor(ifelse(
        as.numeric(gsub("[^[:digit:]]", "", cabins)) %% 2,
        "starboard", "port"
    ))


    dataset
}

#' Generate Preprocessed Data
#'
#' Generates the preprocessed data from the dataset in "titanic.csv" by
#' running all the preprocessing functions and saves the cleaned dataset
#' into the file "titanic_cleaned.Rds".
#'
#' Task 1
generate_preprocessed_data <- function() {
    dataset <- read.csv("titanic.csv")

    dataset <- extract_salutations(dataset)
    dataset <- infer_age(dataset)
    dataset <- extract_cabin_data(dataset)
    dataset <- encode_variables(dataset)
    dataset <- cleanup(dataset)

    saveRDS(dataset, file = "titanic_cleaned.Rds")
}

generate_preprocessed_data()

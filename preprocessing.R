#' Extract Salutations
#'
#' Extracts the salutations from the name column and creates a new column
#' with the extracted salutations in the dataset.
#'
#' @param dataset
extract_salutations <- function(dataset) { }

encode_variables <- function(dataset) { }

infere_age <- function(dataset) { }

cleanup <- function(dataset) { }

#' Extracts the factors Deck and Side from the column Cabin
#'
#' Deck (which is the letter the cabin starts with) and Side (which is starboard
#' for odd and port for even cabin numbers) are extracted from Cabin and coded
#' as factors.
#'
#' @details In the event that a combination of cabins is listed for each a group
#' of multiple people the cabins are distributed arbitrarily among the group.
#' Example: If there are four people in the dataset that have "A11 B12 C13" as
#' the cabin, these four people will be assigned "A11" "B12" "C13" "A11". This
#' way the number of people with a cabin can be guaranteed to be the same as in
#' the original dataset.
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
#' Generates the preprocessed data from the `titanic.csv` by
#' running all the preprocessing functions.
#'
#' @returns preprocessed data
generate_preprocessed_data <- function() {
    dataset <- read.csv("titanic.csv")

    dataset <- extract_cabin_data(dataset)

    extract_salutations(dataset)
    encode_variables(dataset)
    infere_age(dataset)
    cleanup(dataset)

    dataset
}

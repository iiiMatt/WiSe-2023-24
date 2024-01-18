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

statistical_mode <- function(x) {
    levels <- unique(x)
    frequencies <- tabulate(match(x, levels))
    levels[frequencies == max(frequencies)]
}

extract_cabin_data <- function(dataset) {
    dataset[["Deck"]] <- NA
    dataset[["Side"]] <- NA
    cabins <- dataset[["Cabin"]]
    # side <- factor(levels = c(0, 1), labels = c("port", "starboard"))
    for (current_cabins in cabins) {
        if (current_cabins == "") next
        cabins_list <- strsplit(current_cabins, " ")
        deck <- statistical_mode(gsub("[[:digit:]]", "", cabins_list))
        dataset[["Deck"]][cabins == current_cabins] <- ifelse(length(deck) == 1, deck, NULL)
        starboard <- statistical_mode(as.logical(as.numeric(gsub("[^[:digit:]]", "", cabins_list)) %% 2))
        dataset[["Side"]][cabins == current_cabins] <- ifelse(length(starboard) == 1, ifelse(starboard, "starboard", "port"), NULL)
        print(deck)
        print(starboard)
    }
    # dataset[["Side"]] <- cabin
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

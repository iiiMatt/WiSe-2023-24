#' Extract Salutations
#'
#' Extracts the salutations from the name column and creates a new column
#' with the extracted salutations in the dataset.
#'
#' @param dataset
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

    # Rev. -> Typically, a Protestant minister will be addressed as “Reverend”
    #         or in some cases “Pastor.” (will be Mr.)
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

    for (salut in SALUTS)
        salutations[grepl(salut, names, fixed = TRUE)] <- MAPPING[[salut]]


    dataset[["Salutation"]] <- factor(salutations)

    dataset
}

encode_variables <- function(dataset) {
    dataset
}

#' Infer Age
#'
#' Fills in the missing ages in the dataset using the salutations
#'
#' @param dataset
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

    dataset <- extract_salutations(dataset)
    dataset <- encode_variables(dataset)
    dataset <- infer_age(dataset)
    dataset <- cleanup(dataset)

    dataset
}


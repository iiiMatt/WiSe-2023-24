#' Statistical Mode
#'
#' Returns the most frequently occuring value in x. If there are multiple values
#' that occur most frequently this function returns all of them.
#'
#' Task 2b
#' @param x Can be a list, vector or factor.
#' @param na.rm Indicates if NA values should be removed before calculation.
#' @return The statistical mode of x. One or multiple values.
statistical_mode <- function(x, na.rm = TRUE) {
    if (na.rm) x <- na.omit(x)
    levels <- levels(x)
    freq <- tabulate(match(x, levels))
    levels[freq == max(freq)]
}

#' Empirical Entropy
#'
#' Computes the average empirical entropy (according to Boltzmann's formula) of
#' all levels. This is a measure of dispersion that returns 1 if every level
#' occurs with the same frequency and 0 if only one level occurs 100\% of the
#' time.
#'
#' Task 2b
#' @param x A categorial variable.
#' @param na.rm Indicates if NA values should be removed before calculation.
#' @return A floating point number between 0 and 1.
empirical_entropy <- function(x, na.rm = TRUE) {
    if (na.rm) x <- na.omit(x)
    freq <- tabulate(match(x, levels(x))) / length(x)
    1 / log(nlevels(x)) * sum(freq * log(1 / freq))
}

#' Phi Measure of Dispersion for Categorial Data
#'
#'  Computes the phi measure of dispersion which produces more intuitive numbers
#' between 0 and 1 but is not popular unlike entropy.
#'
#' Task 2b
#' @param x A categorial variable.
#' @param na.rm Indicates if NA values should be removed before calculation.
#' @return A floating point number between 0 and 1 (lower than entropy).
phi_dispersion <- function(x, na.rm = TRUE) {
    if (na.rm) x <- na.omit(x)
    levels <- levels(x)
    freq <- tabulate(match(x, levels)) / length(x)
    # The distance from a distribution with minimal dispersion
    phi_min <- 2 * (1 - max(freq))
    # The distance from a distribution with maximal dispersion
    phi_max <- sum(abs(freq - 1 / nlevels(x)))
    phi_min / (phi_min + phi_max)
}

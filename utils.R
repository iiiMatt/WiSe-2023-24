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
}

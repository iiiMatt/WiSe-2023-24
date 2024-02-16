source("helpers.R")

describe_categorial <- function(
    x, name = deparse(match.call()$x),
    na.rm = TRUE) {
    mode <- statistical_mode(x)
    entropy <- empirical_entropy(x)
    phi <- phi_dispersion(x)
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
        # qd <- (q3 - q1) / 2
        cat("Lower quartile:                     ", q1, "\n")
        cat("Median:                             ", med, "\n")
        cat("Upper quartile:                     ", q3, "\n")
    }
}

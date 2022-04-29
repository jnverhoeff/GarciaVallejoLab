#' @title Internal - Returns the mode of a numeric vector
#' @description This function is used internally to determine the mode of a numeric vector. Adapted from \pkg{SPADEVizR}
#' @param x a numeric vector
#' @return a list with 2 numeric values specifying the mode "x" and it associated density "y"
#' @export
computemode <- function(x) {
    den <- stats::density(x, kernel = c("gaussian"))
    return(list(x = den$x[den$y == max(den$y)], y = max(den$y)))
}

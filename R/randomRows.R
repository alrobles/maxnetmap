#' @title randomRows
#' @description Take a sample of the rows of a data frame
#' @param df A data frame
#' @param n A number.
#' @return The \code{n} sampled rows of \code{df}.
#' @examples
#' data(iris)
#' randomRows(iris, 10)
#' @export
randomRows <- function(df, n) {
    return(df[base::sample(nrow(df), n), ])
}

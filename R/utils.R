#' vnums
#' @param n number of labels
#' @param (optional) logical value to convert output to factor
#' @examples
#' tidyecon::vnums(5)
#' tidyecon::vnums(10)
#' tidyecon::vnums(10, factor = TRUE)
vnums = function(n, factor = FALSE) {

  stopifnot(n > 0)
  x = sprintf(paste0("v%.", floor(log10(n) + 1), "d"), seq_len(n))

  if (factor) {
    x = factor(x, x)
  }

  x

}

#' vnums
#' @param n number of labels
#' @param factor (optional) logical value to convert output to factor
#' @examples
#' tidyecon:::vnums(5)
#' tidyecon:::vnums(10)
#' tidyecon:::vnums(100)
#' tidyecon:::vnums(10, factor = TRUE)
vnums = function(n, factor = FALSE) {

  stopifnot(n > 0)
  x = sprintf(paste0("v%.", floor(log10(n) + 1), "d"), seq_len(n))

  if (factor) {
    x = factor(x, x)
  }

  x

}


#' make_id
#' @description makes row ids for a matrix or data.frame using existing row
#'   names or row numbers
#' @param X Data frame or model matrix
#' @examples
#' tidyecon:::make_id(mtcars)
#' tidyecon:::make_id(airquality)
make_id = function(X) {
  if (.row_names_info(X) > 0){
    row.names(X)
  } else {
    seq_len(nrow(X))
  }
}

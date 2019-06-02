#' decomp_lm
#' @description linear model decomposition
#' @param x model matrix
#' @param y target variable
#' @param options list of additional options:
#' \describe{
#'   \item{coefficients}{coefficient vector}
#'   \item{id}{observation ids}
#'   \item{resolve}{either \code{"actual"} or \code{"fitted"}}
#' }
#' @export
#' @examples
#' x = cbind(intercept = 1, mtcars[-match("mpg", names(mtcars))])
#' y = mtcars[["mpg"]]
#' decomp = decomp_lm(x, y)
#' plot(decomp)
decomp_lm = function(x, y, options = list()){

  if (!exists("coefficients", options)) {
    options[["coefficients"]] = lm_fit(x, y)[["coefficients"]]
  }

  options[["resolve"]] = match.arg(options[["resolve"]], c("actual", "fitted"))

  x = as.matrix(x)
  if (is.null(colnames(x))) colnames(x) = vnums(ncol(x))

  if (is.null(options[["variables"]])) {
    options[["variables"]] = colnames(x)
  } else {
    options[["variables"]] = as.character(options[["variables"]])
  }

  X = as.data.frame(x)
  names(X) = vnums(ncol(X))

  if (is.null(options[["id"]])) {
    options[["id"]] = make_id(X)
  }

  X[["id"]] = options[["id"]]

  X = tidyr::gather(X, "vnum", "value", -.data[["id"]])

  table = data.frame(
    vnum = vnums(length(options[["coefficients"]])),
    variable = options[["variables"]],
    coefficient = ifelse(is.na(options[["coefficients"]]),
                         0,
                         options[["coefficients"]]),
    stringsAsFactors = FALSE
  )

  X = dplyr::left_join(X, table, "vnum")
  X[["value"]] = X[["value"]] * X[["coefficient"]]
  X = X[c("id", "vnum", "variable", "value")]

  if (options[["resolve"]] == "actual"){
    X[["value"]] = X[["value"]] * as.numeric(y / (x %*% table[["coefficient"]]))
  }

  class(X) = c("decomp", "data.frame")
  X

}

globalVariables(".data")

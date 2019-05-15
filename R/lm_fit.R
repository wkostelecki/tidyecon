#' lm_fit
#' @param x design matrix (N x n)
#' @param y target (vector or N x 1 matrix)
#' @param w vector of weights
#' @param lower lower bounds for coefficients
#' @param upper upper bounds for coefficients
#' @param tol QR decomposition tolerance
#' @return lm_fit model object
#' @export
#' @examples
#' lm_fit(as.matrix(cbind(1, mtcars[c("cyl", "disp", "hp")])), mtcars[["mpg"]])
lm_fit = function(x, y,
                  w = NULL,
                  lower = rep(-Inf, ncol(x)),
                  upper = rep(Inf, ncol(x)),
                  tol = 1e-7) {

  stopifnot(all(lower <= upper))

  x = as.matrix(x)

  # TODO:
  # - lower/upper with box constrained optimization
  # - sparse implementation

  model = if (is.null(w)) {
    stats::lm.fit(x, y, tol = tol)
  } else {
    stats::lm.wfit(x, y, w, tol = tol)
  }

  model = list(x = x,
               y = y,
               coefficients = model[["coefficients"]],
               model = model)

  class(model) = "lm_fit"

  model

}

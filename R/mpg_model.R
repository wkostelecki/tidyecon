#' mpg_model
#' @description Uses tidyecon functions to estimate model for mpg with mtcars
#'   data.
#' @return lm_fit model object
#' @export
mpg_model = function() {

  lm_fit(datasets::mtcars[setdiff(names(datasets::mtcars), "mpg")],
         datasets::mtcars[["mpg"]])

}


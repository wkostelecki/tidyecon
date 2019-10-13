#' estimate
#' @description wrapper for model estimation using lm_fit
#' @param data data.frame
#' @param model_spec Model specification data frame
#' \describe{
#'   \item{expr}{expressions for explanatory variables}
#'   \item{lower}{(optional) coefficient lower boundaries}
#'   \item{upper}{(optional) coefficient upper boundaries}
#'   \item{category}{(optional) decomp categories}
#'   \item{link}{(optional) coefficient links}
#' }
#' @param options list of options
#' \describe{
#'   \item{y}{target variable expression}
#'   \item{w}{(optional) regression weight expression}
#'   \item{link}{(optional) either "linear" (default) or "binomial"}
#'   \item{env}{(optional) environment for extra variables in variable
#'   expressions}
#' }
#' @export
#' @examples
#' model_spec = data.frame(expr = c("1", setdiff(names(mtcars), "mpg")))
#' options = list(y = "mpg")
#' model = estimate(mtcars, model_spec, options)
#' decomp = decomp(model)
#' plot(decomp)
estimate = function(data,
                    model_spec,
                    options) {

  data = data_prep(data)
  model_spec = model_spec_prep(model_spec)
  options = options_prep(options)

  cols = c(
    y = options[["y"]],
    stats::setNames(model_spec[["expr"]],
                    model_spec[["vnum"]]),
    w = options[["w"]]
  )

  X = data %>%
    dplyr::arrange(.data[["key"]], .data[["id"]]) %>%
    dplyr::group_by(.data[["key"]]) %>%
    dplyr::transmute(
      !!!lapply(c("id", cols),
                function(x) rlang::parse_quo(x, env = options[["env"]]))
    ) %>%
    dplyr::ungroup()

  ind = stats::complete.cases(X) # TODO: move this inside lm_fit?

  model = lm_fit(x = X[ind, model_spec[["vnum"]]],
                 y = X[["y"]][ind],
                 w = X[["w"]][ind],
                 lower = model_spec[["lower"]],
                 upper = model_spec[["upper"]],
                 link = model_spec[["link"]])

  model = list(model = model,
               data = data,
               model_spec = model_spec,
               options = options)

  class(model) = "estimate"

  model

}

globalVariables(".data")

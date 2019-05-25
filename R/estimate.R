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
#' }
#' @export
#' @examples
#' data = mtcars
#' model_spec = data.frame(expr = c("1", "cyl", "hp", "disp"))
#' options = list(y = "mpg")
#' estimate(data, model_spec, options)
estimate = function(data,
                    model_spec,
                    options,
                    env = parent.frame()){

  data = data_prep(data)
  model_spec = model_spec_prep(model_spec)
  options = options_prep(options)

  cols = c(
    y = options[["y"]],
    setNames(model_spec[["expr"]],
             model_spec[["vnum"]]),
    w = options[["w"]]
  )

  X = data %>%
    dplyr::arrange(key, id) %>%
    dplyr::group_by(key) %>%
    dplyr::transmute(
      !!!lapply(cols, function(x) rlang::parse_quo(x, env = env))
    ) %>%
    dplyr::ungroup()

  model = lm_fit(x = X[model_spec[["vnum"]]],
                 y = X[["y"]],
                 w = X[["w"]],
                 lower = model_spec[["lower"]],
                 upper = model_spec[["upper"]],
                 link = model_spec[["link"]])

  if (!is.null(model_spec[["category"]])){
    model[["categories"]] = model_spec[c("vnum", "category")]
  }

  model[["id"]] = X[c("key", "id")]

  model

}

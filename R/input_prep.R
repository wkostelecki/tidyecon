#' data_prep
#' @description Adds id and key columns (necessary for model estimation and
#'   decomposition) to model data frame.
#' @param data Data frame.
#' @importFrom magrittr %>%
#' @examples
#' tidyecon:::data_prep(mtcars)
data_prep = function(data) {

  data = as.data.frame(data)

  if (!exists("id", data)) {
    data[["id"]] = make_id(data)
  }

  if (!exists("key", data)) {
    data = data %>%
      dplyr::group_by(id) %>%
      dplyr::mutate(key = dplyr::row_number()) %>%
      dplyr::ungroup()
  }

  data

}

#' model_spec_prep
#' @description Checks model spec for required fields and infers additional
#'   inputs where possible. See estimate() documentation.
#' @param model_spec data frame or vector containing variable expressions.
#' @examples
#' tidyecon:::model_spec_prep(data.frame(expr = "1"))
model_spec_prep = function(model_spec){

  if (!is.data.frame(model_spec)) {
    if (is.list(model_spec)) {
      model_spec = as.data.frame(model_spec)
    } else {
      model_spec = data.frame(expr = as.character(model_spec),
                              stringsAsFactors = FALSE)
    }
  }

  stopifnot(exists("expr", model_spec))

  if (!exists("lower", model_spec)) {
    model_spec[["lower"]] = -Inf
  } else {
    stopifnot(is.numeric(model_spec[["lower"]]))
    model_spec[["lower"]][is.na(model_spec[["lower"]])] = -Inf
  }

  if (!exists("upper", model_spec)) {
    model_spec[["upper"]] = Inf
  } else {
    stopifnot(is.numeric(model_spec[["upper"]]))
    model_spec[["upper"]][is.na(model_spec[["upper"]])] = Inf
  }

  model_spec[["expr"]] = as.character(
    parse(text = as.character(model_spec[["expr"]]))
  )

  if (exists("link", model_spec)) {
    model_spec = model_spec %>%
      dplyr::mutate(link = ifelse(link == "",
                                  NA_character_,
                                  as.character(link))) %>%
      dplyr::group_by(link) %>%
      dplyr::mutate(lower = ifelse(is.na(link), lower, max(lower)),
                    upper = ifelse(is.na(link), upper, min(upper))) %>%
      dplyr::ungroup()
  }

  stopifnot(all(model_spec[["lower"]] <= model_spec[["upper"]]))

  model_spec[["vnum"]] = vnums(nrow(model_spec))

  model_spec

}

#' options_prep
#' @description Checks options list for required inputs. See estimate()
#'   documentation.
#' @param options List of options.
#' @examples
#' tidyecon::options_prep(list(y = "mpg"))
options_prep = function(options){
  stopifnot(exists("y", options))
  if (!exists("env", options)) options[["env"]] = parent.frame()
  options
}

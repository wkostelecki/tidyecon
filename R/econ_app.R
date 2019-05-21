#' econ_app
#' @description Runs app for estimating and decomposing models
#' @param data data.frame to use for modelling
#' @import shiny
#' @export
#' @examples
#' \dontrun{
#'   econ_app(data = mtcars)
#' }
econ_app = function(data = NULL) {

  app = shiny::shinyApp(
    ui = econ_ui(),
    server = econ_server(data)
  )

  shiny::runGadget(app, viewer = shiny::browserViewer())

}

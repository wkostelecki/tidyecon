#' @import miniUI
#' @export
econ_ui = function() {
  miniUI::miniPage(
    miniUI::gadgetTitleBar(
      shiny::textOutput("title"),
      left = miniUI::miniTitleBarCancelButton(inputId = "cancel"),
      right = miniUI::miniTitleBarButton("done",
                                         "Done",
                                         primary = TRUE)
    ),
    miniUI::miniTabstripPanel(
      id = "bottom_tabstrip",
      miniUI::miniTabPanel(
        "Data",
        value = "data_tab",
        miniUI::miniContentPanel(
          shiny::fillRow(
            DT::DTOutput("data_table", height = "100%")

          )
        ),
        icon = shiny::icon("database")
      ),
      miniUI::miniTabPanel(
        "Model",
        value = "model_tab",
        miniUI::miniContentPanel(
          fillRow(
            flex = c(1, 3),
            column(width = 12,
                   shiny::uiOutput("target"),
                   shiny::uiOutput("regressors")),
            fillCol(shiny::plotOutput("model_plot"))
          )
        ),
        icon = shiny::icon("chart-line")
      )
    )
  )
}

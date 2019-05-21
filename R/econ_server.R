#' @export
econ_server = function(data = NULL) {

  function(input, output, session) {

    raw_data = reactive({
      data
    })

    output[["target"]] = renderUI({
      shiny::selectInput("target",
                         label = "Select Target",
                         choices = names(raw_data()))
    })

    output[["regressors"]] = renderUI({
      shiny::selectInput("regressors",
                         label = "Select Regressors",
                         choices = names(raw_data()),
                         multiple = TRUE)
    })

    output[["title"]] = renderText({
      req(input[["bottom_tabstrip"]])
      input[["bottom_tabstrip"]]
    })

    output[["data_table"]] = DT::renderDT({
      req(raw_data())
      raw_data()
    })

    model = reactive({
      req(raw_data(),
          input[["target"]],
          input[["regressors"]])
      lm_fit(raw_data()[input[["regressors"]]],
             raw_data()[[input[["target"]]]])
    })

    output[["model_plot"]] = renderPlot({

      req(model())

      ezplot::model_plot(data.frame(id = seq_len(length(model()[["y"]])),
                                    actual = model()[["y"]],
                                    fitted = model()[["residuals"]]),
                         x = "id",
                         actual = "actual",
                         fitted = "fitted")

    })

    observeEvent(input[["cancel"]], {
      print(1)
    })

    observeEvent(input[["done"]], {
      print(2)
      stopApp(2)
      # stopApp(model())
    })

  }
}

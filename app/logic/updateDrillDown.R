box::use(
  shiny[freezeReactiveValue, updateSelectInput, bindEvent, observe, moduleServer,
        is.reactive],
  shiny.fluent[updateDropdown.shinyInput]

)

box::use(
  app/logic/choices_if_all[choices_if_all]
)



updateDrillDown <- function(inputId, .data, .upper_input,
                            parent_session, id = NULL) {

  stopifnot(!is.reactive(inputId))
  # stopifnot(is.reactive(.data))
  stopifnot(is.reactive(.upper_input))

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      freezeReactiveValue(input, inputId)
      choices <- .upper_input() |>
        choices_if_all(
          col = .data()[[inputId]]
          )
      # updateSelectInput(session = parent_session, inputId, choices = choices)

      updateDropdown.shinyInput(
        session = parent_session,
        inputId,
        options = choices |> lapply(\(x) list(key = x, text = x)),
        value = choices[1]
        )

    }) |>
      bindEvent(.data(), .upper_input())

  })
}

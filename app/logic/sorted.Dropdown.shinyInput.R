box::use(
  stats[na.omit],
  shiny.fluent[Dropdown.shinyInput]
)

box::use(app/logic/sort_unique[sort_unique])

#' @export

sorted.Dropdown.shinyInput <- function(inputId, label, col) {
  sorted_unique <- sort_unique(col) |> na.omit()
  choices <- c(as.character(sorted_unique))

  Dropdown.shinyInput(
    inputId,
    label = label,
    options = choices |> lapply(\(x) list(key = x, text = x)),
    value = choices[1],
    multiSelect = TRUE
  )
}

box::use(
  stats[na.omit],
  shiny[selectInput]
)

box::use(app/logic/sort_unique[sort_unique])

#' @export

selectInput_all <- function(inputId, label, col) {
  sorted_unique <- sort_unique(col) |> na.omit()
  choices_with_all <- c("All", as.character(sorted_unique))

  selectInput(
    inputId,
    label = label,
    choices = choices_with_all,
    # multiple = TRUE,
    selected = "All"
  )
}

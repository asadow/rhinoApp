box::use(
  dplyr[filter, pull]
)

box::use(app/logic/sort_unique[sort_unique])

# app/logic/filter_chosen.R
#' @export


filter_chosen <- function(.data, col, .input) {
  stopifnot(!is.null(.input))
  chosen <- if(.input == "All") {
    .data |> pull( {{ col }} ) |> sort_unique()
  } else {.input}
  .data |> filter( {{ col }} %in% chosen)
}

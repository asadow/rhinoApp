box::use(
  stringr[str_sort]
)

#' Extract and sort unique elements alphabetically
#'
#' @description `sort_unique()` returns a character vector with
#' the unique elements sorted by alphabetical order.
#'
#'
#' @return A character vector.
#' @export

sort_unique <- function(x){
  stringr::str_sort(unique(x))
}

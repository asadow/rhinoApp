box::use(
  app/logic/sort_unique[sort_unique]
)


choices_if_all <- function(.input_parent, col) {

  choices <- sort_unique(col)

  if(.input_parent == "All"){
    c("All", choices)
  } else {choices}

}

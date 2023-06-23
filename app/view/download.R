box::use(
  shiny[NS, tagList, downloadButton, downloadHandler, is.reactive, moduleServer],
)

#' @export
ui <- function(id){
  ns <- NS(id)
  tagList(
    downloadButton(ns("downloadData"), "Download .xlsx")
  )
}

#' @export
server <- function(id, .data, .filename = "shiny-export"){
  # stopifnot(is.reactive(.data))
  # stopifnot(is.reactive(.filename))

  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$downloadData <- downloadHandler(
      filename = "shiny-export.xlsx",
      # filename = function() paste0(.filename(), ".xlsx"),
      content = function(file) {
        openxlsx::write.xlsx(.data(), file)
      }
    )
  })
}

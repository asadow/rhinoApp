box::use(
  shiny[NS, tagList, downloadButton, downloadHandler, is.reactive, moduleServer,
        verbatimTextOutput, renderPrint],
  shinymeta[metaRender, metaReactive, expandChain, buildScriptBundle]

)

#' @export
ui <- function(id){
  ns <- NS(id)
  tagList(
      verbatimTextOutput(ns("code")),
      downloadButton(ns("download_script"), "Download reproducible report with code.")
  )
}

#' @export
server <- function(id, .output, .filename = "shiny-export"){
  # stopifnot(is.reactive(.data))
  # stopifnot(is.reactive(.filename))

  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$download_script <- downloadHandler(
      filename = "code.zip",
      content = function(file) {
        code <- expandChain(
          "#' ---",
          "#' title: 'Some code'",
          "#' author: ''",
          "#' ---",
          "#' Some text that appears above the plot",
          "#+ plot, message=FALSE, tidy=TRUE, fig.show='hold', fig.height=2",
          quote({
            library(dplyr)
            library(stringr)
          }),
          .output()
        )
        buildScriptBundle(
          code, file,
          render_args = list(output_format = "pdf_document")
        )
      }
    )

    output$code <- renderPrint({
      expandChain(
        quote({
          library(dplyr)
          library(stringr)
        }),
        .output()
      )
    })
  })
}

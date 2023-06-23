box::use(
  dplyr,
  stringr,
  targets[tar_read],
  purrr[set_names, imap, list_flatten],
  reactable[reactable, reactableOutput, renderReactable],
  bslib[page_sidebar, sidebar],
  shinyjs[show],
  shinyFeedback[hideFeedback],
  shiny[p, uiOutput, renderUI, req, h3, moduleServer, div, NS, validate, tagList,
        fluidRow, column, wellPanel, selectInput, reactive, is.reactive,
        dateRangeInput, sliderInput, verbatimTextOutput, renderPrint,
        downloadButton, downloadHandler],
  shinymeta[metaRender, metaReactive, expandChain, buildScriptBundle],
  shiny.fluent[Dropdown.shinyInput, Text, Stack, DetailsList, DatePicker.shinyInput, Separator, Slider.shinyInput],
  shinyWidgets[pickerInput]
)

box::use(
  app/logic/filter_chosen[filter_chosen],
  app/logic/sorted.Dropdown.shinyInput[sorted.Dropdown.shinyInput],
  app/logic/updateDrillDown[updateDrillDown],
  app/logic/makeCard[makeCard],
  app/logic/sort_unique[sort_unique],


  )


#' @export
ui <- function(id, data) {

  stopifnot(!is.reactive(data))
  stopifnot(is.data.frame(data))

  ns <- NS(id)

  uniques <- c("barg", "dept", "crew", "tr_code") |>
    set_names() |>
    lapply(
      \(x) data[[x]] |>
        sort_unique() |>
        lapply(\(x) list(key = x, text = x))
      )

  pickers <- c("barg", "dept", "crew", "tr_code") |>
    set_names(
      c("Bargaining Unit", "Department", "Crew", "Transaction Code") |>
        paste0("(s)")
    ) |>
    imap(
      \(x, idx) sorted.Dropdown.shinyInput(
        ns(x),
        label = idx,
        col = data[[x]]
      )
    )

  flatten <- function(x) {x |> list_flatten() |> unique()}
  tagList(
    ## inputs = tagList(...) instead of Stack(...) causes:
    ## Warning: Error in checkNames: When passing a list to React, either all or no elements must be named
    inputs = Stack(
      tokens = list(childrenGap = 10),
      Dropdown.shinyInput(
        ns("barg"),
        label = "Bargaining Unit(s)",
        options =  uniques$barg,
        value = uniques$barg |> flatten(),
        multiSelect = TRUE
      ),
      Dropdown.shinyInput(
        ns("dept"),
        label = "Department(s)",
        options =  uniques$dept,
        value = uniques$dept |> flatten(),
        multiSelect = TRUE
      ),
      Dropdown.shinyInput(
        ns("crew"),
        label = "Crew(s)",
        options =  uniques$crew,
        value = uniques$crew |> flatten(),
        multiSelect = TRUE
      ),
      Dropdown.shinyInput(
        ns("tr_code"),
        label = "Transaction Code(s)",
        options =  uniques$tr_code,
        value = uniques$tr_code |> flatten(),
        multiSelect = TRUE
      ),
      DatePicker.shinyInput(
        ns("fromDate"), value = min(data$date), label = "From date"
      ),
      DatePicker.shinyInput(
        ns("toDate"), value = max(data$date), label = "To date"
      ),
      Slider.shinyInput(
        ns("days_range"),
        min = 0,
        max = 100,
        label = "Choose number of days with above codes and dates",
        value = 16,
        step = 1
      )
    ),
    main =
      # Stack(
        uiOutput(ns("data_selected"))
        # ,
        # verbatimTextOutput(ns("code")),
        # downloadButton(ns("download_script"), "Download reproducible report with code.")
      # )
  )

}

#' @export
server <- function(id) {

  .data <- metaReactive({
    "# ideally, we query a database instead of tar_read()"
    tar_read(
      filtered_tr_runs,
      store = "C:/Users/asadowsk/OneDrive - University of Guelph/r/work/physr/_targets"
    ) |>
      dplyr$slice(1:20)
  })

  moduleServer( id, function(input, output, session){

    .table <- metaReactive({

      ..(.data()) |>
        dplyr$filter(
          barg %in% ..(input$barg)
          & dept %in% ..(input$dept)
          & crew %in% ..(input$crew)
          & tr_code %in% ..(input$tr_code)
          ) |>
        dplyr$select(
          employee,
          tr_code,
          date,
          hours,
          activity_code,
          crew,
          barg
        ) |>
        dplyr$rename_with(
        \(x) x |>
            stringr$str_replace_all("(dept|barg|desc|trans)", "\\1.") |>
            stringr$str_replace_all("_", " ") |>
            stringr$str_to_title()
        )
    })

      .items_list <- reactive({
        if(nrow(.table()) > 0){
          DetailsList(items = .table())
        } else {
          p("No matching transactions.")
        }
    })

    output$data_selected <- renderUI(.items_list())

    list(
      data_behind_table = .table,
      items_list = .items_list
    )
  })
}

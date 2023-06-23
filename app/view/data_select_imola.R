box::use(
  dplyr,
  stringr,
  targets[tar_read],
  purrr[set_names, imap],
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
  shinyWidgets[pickerInput, pickerOptions]
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

  pickers <- c("barg", "dept", "crew", "tr_code") |>
    set_names(
      c("Bargaining Unit", "Department", "Crew", "Transaction Code") |>
        paste0("(s)")
    ) |>
    imap(
      \(x, idx) pickerInput(
        ns(x),
        label = idx,
        choices = data[[x]] |> sort_unique(),
        options = pickerOptions(actionsBox = TRUE),
        multiple = TRUE
      )
    )


  uniques <- c("barg", "dept", "crew", "tr_code") |>
    set_names() |>
    lapply(
      \(x) data[[x]] |>
        sort_unique() |>
        lapply(\(x) list(key = x, text = x))
    )

  tagList(
    inputs = tagList(
      # tokens = list(childrenGap = 10),
      pickers
      # Dropdown.shinyInput(
      #   "barg",
      #   label = "Bargaining Unit(s)",
      #   options =  uniques$barg,
      #   value = uniques$barg[[1]],
      #   multiSelect = TRUE
      # ),
      # Dropdown.shinyInput(
      #   "dept",
      #   label = "Department(s)",
      #   options =  uniques$dept,
      #   value = uniques$dept[[1]],
      #   multiSelect = TRUE
      # ),
      # Dropdown.shinyInput(
      #   "crew",
      #   label = "Crew(s)",
      #   options =  uniques$crew,
      #   value = uniques$crew[[1]],
      #   multiSelect = TRUE
      # ),
      # Dropdown.shinyInput(
      #   "tr_code",
      #   label = "Transaction Code(s)",
      #   options =  uniques$tr_code,
      #   value = uniques$tr_code[[1]],
      #   multiSelect = TRUE
      # ),
      # DatePicker.shinyInput(
      #   ns("fromDate"), value = min(data$date), label = "From date"
      # ),
      # DatePicker.shinyInput(
      #   ns("toDate"), value = max(data$date), label = "To date"
      # ),
      # Slider.shinyInput(
      #   ns("days_range"),
      #   min = 0,
      #   max = 100,
      #   label = "Choose number of days with above codes and dates",
      #   value = 16,
      #   step = 1
      # )
    ),
    main =
      # tagList(
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
          barg %in% input$barg
          # & dept %in% input$dept
          # & crew %in% input$crew
          # & tr_code %in% input$tr_code
        ) |>
        dplyr$select(
          ! c(
            ends_with("at_trans"),
            year_half,
            tr_code_general,
            employee_no,
            year,
            tr_id,
            surname,
            given_names,
            hours_day
          )
        ) |>
        dplyr$rename_with(
          \(x) x |>
            stringr$str_replace_all("(dept|barg|desc|trans)", "\\1.") |>
            stringr$str_replace_all("_", " ") |>
            stringr$str_to_title()
        )
    })

    output$data_selected <- metaRender(renderUI, {
      items_list <- if(nrow(..(.table())) > 0){
          DetailsList(items = ..(.table()))
        } else {
          p("No matching transactions.")
        }

      makeCard(
        "Employee Data",
        content = div(style="max-height: 500px; overflow: auto", items_list)
      )
    })

    list(
      data_behind_table = .table,
      data_selected = output$data_selected
    )
  })
}

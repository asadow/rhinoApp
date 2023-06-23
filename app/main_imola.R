box::use(
  bslib[page_sidebar, sidebar, bs_theme],
  targets[tar_read],
  dplyr,
  imola[flexPanel, gridPage],
  shiny.fluent[Stack, fluentPage, Nav, CommandBarItem, CommandBar, Text],
  shiny[bootstrapPage, div, moduleServer, NS, h1, renderUI, tags, icon,
        downloadButton, downloadHandler,
        verbatimTextOutput, renderPrint, img, tagList],
  shinymeta[expandChain, buildScriptBundle]
)


box::use(
  app/view/data_select_imola,
  app/view/download,
  app/view/download_script,
  app/logic/makePage[makePage],
  app/logic/makeCard[makeCard]
)

uog_theme <- bs_theme(
  version = 5,
  bg = "#FFFFFF",
  fg = "#000000",
  primary = "#A30808",
  secondary = "#F8D505"
)

commandbar_header <- CommandBar(
  items = list(
    CommandBarItem("New", "Add", subitems = list(
      CommandBarItem("Email message", "Mail", key = "emailMessage", href = "mailto:me@example.com"),
      CommandBarItem("Calendar event", "Calendar", key = "calendarEvent")
    )
    ),
    CommandBarItem("Upload", "Upload"),
    CommandBarItem("Share analysis", "Share"),
    CommandBarItem("Download report", "Download")
  ),
  farItems = list(
    CommandBarItem("Grid view", "Tiles", iconOnly = TRUE),
    CommandBarItem("Info", "Info", iconOnly = TRUE)
  ),
  style = list(width = "100%")
)

app_header <- flexPanel(
  id = "header",
  align_items = "center",
  flex = c(0, 1, 0),
  img(src = "static/images/pr-logo.jpg", style = "width: 250px"),
  div(Text(variant = "xLarge", "Transaction App."), class = "title"),
  commandbar_header,
  style = "box-shadow: 0 0 10px #000;"
)

app_navigation <- flexPanel(
  groups = list(
    list(links = list(
      list(name = 'Home', url = '#!/', key = 'home', icon = 'Home'),
      list(name = 'Analysis', url = '#!/other', key = 'analysis', icon = 'AnalyticsReport'),
      list(name = 'shiny.fluent', url = 'http://github.com/Appsilon/shiny.fluent', key = 'repo', icon = 'GitGraph'),
      list(name = 'shiny.react', url = 'http://github.com/Appsilon/shiny.react', key = 'shinyreact', icon = 'GitGraph'),
      list(name = 'Appsilon', url = 'http://appsilon.com', key = 'appsilon', icon = 'WebAppBuilderFragment')
    ))
  ),
  initialSelectedKey = 'home',
  styles = list(
    root = list(
      height = '100%',
      boxSizing = 'border-box',
      overflowY = 'auto'
    )
  )
)

app_footer <- flexPanel(
  id = "footer",
  justify_content = 'space-between',
  gap = "20px",
  Text(variant = "medium", "Built with ❤ by Adam", block=TRUE),
  Text(variant = "medium", nowrap = FALSE, "If you have suggestions, reach out to us at asadowsk@uoguelph.ca"),
  Text(variant = "medium", nowrap = FALSE, "Data source: Megamation Database")
)

data <- tar_read(
  filtered_tr_runs,
  store = "C:/Users/asadowsk/OneDrive - University of Guelph/r/work/physr/_targets"
  ) |>
  dplyr$slice(1:20)

#' @export
ui <- function(id) {
  ns <- NS(id)

  d <- data_select_imola$ui(
    ns("d"),
    data
    )

  app_sidebar <- div(
    id = "sidebar",
    d$inputs
  )

  app_content <- div(
    d$main,
    download$ui(ns("download")),
    # download_script$ui("download_script")
    # ,
    verbatimTextOutput(ns("code")),
    downloadButton(ns("download_script"), "Download reproducible report with code")
    )

  gridPage(
    tags$head(tags$link(rel="stylesheet", href = "static/css/mine.css")),
    template = "grail-left-sidebar",
    gap = "10px",
    rows = list(
      default = "70px 1fr 30px"
    ),

    header = app_header,
    sidebar = app_sidebar,
    content = app_content,
    footer = app_footer
  )
}

#' @export
server <- function(id) {

  moduleServer(id, function(input, output, session) {

    data_selected <- data_select_imola$server("d")

    download$server("download", .data = data_selected$data_behind_table)
    # Does not work: Why?
    # download_script$server("download_script", .output = data_select_imola$output_table)

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
          data_selected$data_behind_table()
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
        data_selected$data_behind_table()
      )
    })


  })

}

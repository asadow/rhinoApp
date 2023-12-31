box::use(
  bslib[page_sidebar, sidebar, bs_theme],
  targets[tar_read],
  dplyr,
  imola,
  shiny.fluent[Stack, fluentPage, Nav, CommandBarItem, CommandBar, Text],
  shiny[uiOutput, bootstrapPage, div, moduleServer, NS, h1, renderUI, tags, icon,
        downloadButton, downloadHandler,
        verbatimTextOutput, renderPrint, img, tagList],
  shinymeta[expandChain, buildScriptBundle],
  shiny.router[route, router_server, router_ui]
)


box::use(
  app/view/data_select,
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

card1 <- makeCard(
  "Welcome to a shiny.fluent demo!",
  div(
    Text("shiny.fluent is a package that allows you to build Shiny apps using Microsoft's Fluent UI."),
    Text(" Use the menu on the left to explore live demos of all available components.")
  ))

home_page <- makePage(
  "This is a Fluent UI app built in Shiny",
  "",
  # "shiny.react + Fluent UI = shiny.fluent",
  div(card1)
)

app_navigation <- Nav(
  groups = list(
    list(
      links = list(
        list(name = 'Home', url = '#!/', key = 'home', icon = 'Home'),
        list(name = 'Analysis', url = '#!/other', key = 'analysis', icon = 'AnalyticsReport'),
        list(name = 'Calendar', url = 'http://github.com/Appsilon/shiny.fluent', key = 'repo', icon = 'GitGraph'),
        list(name = 'Code', url = '#!/code', key = 'code', icon = 'GitGraph'),
        list(name = 'PR Website', url = 'http://www.pr.uoguelph.ca', key = 'appsilon', icon = 'WebAppBuilderFragment')
      )
    )
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


app_footer <- Stack(
  horizontal = TRUE,
  horizontalAlign = 'space-between',
  tokens = list(childrenGap = 20),
  Text(variant = "medium", "Built with ❤ by Adam", block=TRUE),
  Text(variant = "medium", nowrap = FALSE, "If you have suggestions, please reach out to asadowsk@uoguelph.ca"),
  Text(variant = "medium", nowrap = FALSE, "Data source: Megamation Database")
)


#' @export
ui <- function(id) {
  ns <- NS(id)

  commandbar_header <- CommandBar(
    items = list(
      # CommandBarItem("New", "Add", subitems = list(

      # Can't use some of these yet  ------------------------------------------------
      ## See https://github.com/Appsilon/shiny.fluent/issues/44

      CommandBarItem("Make email", "Mail", key = ns("emailMessage"), href = "mailto:asadowsk@uoguelph.ca"),
      # CommandBarItem("Calendar event", "Calendar", key = ns("calendarEvent"))
      # )
      # ),
      # CommandBarItem("Upload", "Upload"),
      CommandBarItem("Share analysis", "Share"),
      CommandBarItem("Download data", "Download", key = ns("ms_button"))
    ),
    farItems = list(
      # CommandBarItem("Grid view", "Tiles", iconOnly = TRUE, key = ns("grid")),
      CommandBarItem("Info", "Info", iconOnly = TRUE, key = ns("info"))
    ),
    style = list(width = "100%")
  )


  app_header <- tagList(
    img(src = "static/images/pr-logo-words2.jpg", style = "width: 250px"),
    div(Text(variant = "xLarge", "Transaction App."), class = "title"),
    commandbar_header
    # ,
    # style = "box-shadow: 0 0 10px #000;"
  )
  data <- tar_read(filtered_tr_runs, store = "C:/Users/asadowsk/OneDrive - University of Guelph/r/work/physr/_targets") |> dplyr$slice(1:20)

  d <- data_select$ui(ns("d"), data)

  layout <- function(main_ui){
    div(class = "grid-container",
        div(class = "header", app_header),
        div(class = "sidenav", app_navigation),
        div(class = "analysis_page", main_ui),
        div(class = "footer", app_footer)
    )
  }

  analysis_page <- makePage(
    "Analysis",
    "",
    ## Why we need div here? W/o we get Error in makePage: unused arguments
    contents = div(
      Stack(
        horizontal = TRUE,
        maxWidth = 12000,
        tokens = list(childrenGap = 10),
        makeCard(
          "Filters",
          d$inputs,
          size = 4,
          style = "max-height: 550px; max-width: 200px"
        ),
        makeCard(
          "Employee Data",
          style = "max-height: 550px; overflow: auto",
          content = uiOutput(ns("data"))
        )
      )
    )
  )

  code_page <- makePage(
    "Code to Reproduce Analysis",
    "",
    makeCard(
      "",
      content =
        tagList(
          verbatimTextOutput(ns("display_code")),
          downloadButton(
            ns("download_script"),
            "Download reproducible report with code"
          )
        )
    )
  )

  router <- router_ui(
    route("/", home_page),
    route("other", analysis_page),
    route("code", code_page)

  )

  fluentPage(
    layout(router),
    tags$head(tags$link(rel="stylesheet", href = "static/css/fluent.css"))
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    router_server()

    data_selected <- data_select$server("d")
    # download$server("download", .data = data_selected$data_behind_table)
    # Does not work: Why?
    # download_script$server("download_script", .output = data_selected$output_table)

    output$data <- renderUI(data_selected$items_list())

    output$display_code <- renderPrint({
      expandChain(
        quote({
          library(dplyr)
          library(stringr)
        }),
        data_selected$data_behind_table()
      )
    })

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


  })

}

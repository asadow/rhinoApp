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
  "Welcome to shiny.fluent demo!",
  div(
    Text("shiny.fluent is a package that allows you to build Shiny apps using Microsoft's Fluent UI."),
    Text("Use the menu on the left to explore live demos of all available components.")
  ))

card2 <- makeCard(
  "shiny.react makes it easy to use React libraries in Shiny apps.",
  div(
    Text("To make a React library convenient to use from Shiny, we need to write an R package that wraps it - for example, a shiny.fluent package for Microsoft's Fluent UI, or shiny.blueprint for Palantir's Blueprint.js."),
    Text("Communication and other issues in integrating Shiny and React are solved and standardized in shiny.react package."),
    Text("shiny.react strives to do as much as possible automatically, but there's no free lunch here, so in all cases except trivial ones you'll need to do some amount of manual work. The more work you put into a wrapper package, the less work your users will have to do while using it.")
  ))

home_page <- makePage(
  "This is a Fluent UI app built in Shiny",
  "shiny.react + Fluent UI = shiny.fluent",
  div(card1, card2)
)

app_navigation <- Nav(
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

app_footer <- Stack(
  horizontal = TRUE,
  horizontalAlign = 'space-between',
  tokens = list(childrenGap = 20),
  Text(variant = "medium", "Built with ❤ by Adam", block=TRUE),
  Text(variant = "medium", nowrap = FALSE, "If you have suggestions, reach out to us at asadowsk@uoguelph.ca"),
  Text(variant = "medium", nowrap = FALSE, "Data source: Megamation Database")
)


#' @export
ui <- function(id) {
  ns <- NS(id)

  commandbar_header <- CommandBar(
    items = list(
      CommandBarItem("New", "Add", subitems = list(
        CommandBarItem("Email message", "Mail", key = ns("emailMessage"), href = "mailto:me@example.com"),
        CommandBarItem("Calendar event", "Calendar", key = ns("calendarEvent"))
      )
      ),
      CommandBarItem("Upload", "Upload"),
      CommandBarItem("Share analysis", "Share"),
      CommandBarItem("Download report", "Download")
    ),
    farItems = list(
      CommandBarItem("Grid view", "Tiles", iconOnly = TRUE, key = ns("grid")),
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
    "Employees",
    "Filtered",
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
        ),
    makeCard(
      "Code to Make Table",
      content = verbatimTextOutput(ns("display_code"))
      ),
    downloadButton(
      ns("download_script"),
      "Download reproducible report with code"
      )
    )
    )

  router <- router_ui(
    route("/", home_page),
    route("other", analysis_page)
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

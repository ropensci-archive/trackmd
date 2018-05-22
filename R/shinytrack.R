#' Edit and track changes within markdown
#' @import shiny
#' @import rstudioapi
#' @import miniUI
#' @import shinyAce
trackChangesViewer <- function() {

  # necessary paths
  tmp <- file.path(tempdir(),"trackmd")
  dir.create(tmp, showWarnings = FALSE)
  tmpfle <- tempfile(tmpdir = tmp, fileext = ".html")
  readr::write_file(x = "here is some html", path = tmpfle)
  shiny::addResourcePath("trackmd", tmp)

  # Get the document context.
  context <- rstudioapi::getSourceEditorContext()
  doc <- context$contents
  tags <- shiny::tags
  cmLink <- tags$a(href = "http://criticmarkup.com/", "CriticMarkup")

  ui <- miniPage(
    # includeHighlightJs(),
    # title
    gadgetTitleBar("Track Changes"),

    miniContentPanel(
      h4("Inspired by ", cmLink, " for tracking changes in Markdown."),
      hr(),
      miniTabstripPanel(
        # edit panel
        miniTabPanel( title = "Edit",
          fluidRow(

            # render the markup as html
            column(6,
              includeHTML(tmpfle)
            ),

            # editor for marking up
            column(6,
              shinyAce::aceEditor("editor", value = paste(doc, collapse = "\n"))
              #uiOutput("document", container = rCodeContainer)
            )
          )
        ),

        # review panel
        miniTabPanel(title = "Review",
            fluidRow(
                column(8,
                  includeHTML(tmpfle)
                       ),
                column(3,
                  actionButton("next", "Next"))
            )

        )

      )
      # stableColumnLayout(
      #   checkboxInput("brace.newline", "Place left braces '{' on a new line?", FALSE),
      #   numericInput("indent", "Indent size: ", 2),
      #   numericInput("width", "Column width: ", 60)
      # ),
      #uiOutput("document", container = rCodeContainer)
    )
  )

  server <- function(input, output, session) {

    observe({
      updateAceEditor(
        session, "editor"
      )
    })

    observeEvent(input$done, {
      contents <- input$editor
      rstudioapi::setDocumentContents(contents, id = context$id)
      invisible(stopApp())
    })

  }

  viewer <- dialogViewer("Track Changes", width = 1000, height = 800)
  runGadget(ui, server, viewer = viewer)

}


#' Make an R script to render changes
#' 
#' Makes an R script to convert markdown with critic markup to an HTML with track changes.
#' 
#' @param siteDir The directory to save the script in.
#' 
#' @keywords internal
makeBuildScript <- function(siteDir) {
  readr::write_file("trackmd:::render_changes(file = commandArgs()[1], output = commandArgs()[2])",
                    path = file.path(siteDir, "build.R"))
}
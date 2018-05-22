#' Edit and track changes within markdown
#' @import shiny
#' @import rstudioapi
#' @import miniUI
#' @import shinyAce
trackChangesViewer <- function() {

  # necessary paths
  # tmp <- file.path(tempdir(),"trackmd")
  # dir.create(tmp, showWarnings = FALSE)
  # tmpfle <- tempfile(tmpdir = tmp, fileext = ".html")
  # readr::write_file(x = "here is some html", path = tmpfle)
  # shiny::addResourcePath("trackmd", tmp)

  # Get the document context.
  context <- rstudioapi::getSourceEditorContext()
  doc <- context$contents
  tags <- shiny::tags
  cmLink <- tags$a(href = "http://criticmarkup.com/", "CriticMarkup")

  ui <- miniPage(
    # includeHighlightJs(),
    # title
    gadgetTitleBar("Record Additions & Deletions"),

    miniContentPanel(
      h4("Inspired by ", cmLink, " for tracking changes in Markdown."),
      hr(),
#      miniTabstripPanel(
        # edit panel
#        miniTabPanel( title = "Edit",
#          fluidRow(

            # # render the markup as html
            # column(6,
            #   includeHTML(tmpfle)
            # ),

            # editor for marking up
#            column(6,
          shinyAce::aceEditor("editor", value = paste(doc, collapse = "\n"))
              #uiOutput("document", container = rCodeContainer)
#            )
#          )
#        ),

        # # review panel
        # miniTabPanel(title = "Review",
        #     fluidRow(
        #         column(8,
        #           includeHTML(tmpfle)
        #                ),
        #         column(3,
        #           actionButton("next", "Next"))
        #     )
        #
        # )

#      )
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
      aftr <- paste0(contents, collapse = "\n")
      markedup <- trackmd:::diff_to_markup(paste0(doc, collapse = "\n"), aftr)
      rstudioapi::setDocumentContents(markedup, id = context$id)
      invisible(stopApp())
    })

  }

  viewer <- dialogViewer("Record Changes", width = 1000, height = 800)
  runGadget(ui, server, viewer = viewer)

}

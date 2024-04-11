#' 02_input_file UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_02_input_file_ui <- function(id){
  ns <- NS(id)
  tagList(

    bslib::accordion_panel(
      title = "Input file",
      value = "get_file",
      icon = bsicons::bs_icon("file-earmark-spreadsheet"),
      shiny::fileInput(
        inputId = ns("file"),
        label = "Excel file",
        accept = c(".xlsx", "xls")
      ),
      shiny::selectInput(
        inputId = ns("sheet"),
        label = "Sheet",
        choices = NULL,
        selected = NULL
      ),
      shiny::actionButton(
        inputId = ns("save"),
        label = "Load"
      )

    )
 
  )
}
    
#' 02_input_file Server Functions
#'
#' @noRd 
mod_02_input_file_server <- function(id, r6){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # initialize reactive container of sheets
    file <- shiny::reactiveValues(sheets = NULL)

    # react to file choice
    shiny::observeEvent(input$file, ignoreInit = TRUE, {

      shiny::req(input$file)

      # extract sheet names from file
      file$sheets <- readxl::excel_sheets(path = input$file$datapath)

      # update sheets choices
      shiny::updateSelectInput(
        inputId = "sheet",
        choices = file$sheets,
        selected = NULL
      )

    })

    # react to saving inputs
    shiny::observeEvent(input$save, {

      shiny::req(input$file, input$sheet)

      # validate the contents of the selected sheet

      # load contents of the selected sheet
      sheet_df <- readxl::read_excel(
        path = input$file$datapath, sheet = input$sheet
      )

      # check that expected columns exist
      cols_expected <- c("interviewId", "decision", "status", "comment")
      cols_found <- names(sheet_df)
      if (!all(cols_expected %in% cols_found)) {

        cols_missing <- cols_expected[! cols_expected %in% cols_found]

        cols_missing_msg <- glue::glue(
          "Columns expected: {glue::glue_collapse(cols_expected, sep = ', ')}",
          "Columns missing: {glue::glue_collapse(cols_missing, sep = ', ')}",
          .sep = "\n"
        )

        shinyFeedback::showToast(
          type = "error",
          title = "Expected columns not found in selected sheet",
          message = cols_missing_msg
        )

        cols_ok <- shiny::reactive({FALSE})

      } else {
        cols_ok <- shiny::reactive({TRUE})
      }

      # check that `decision` contains expected values
      if (cols_ok()) {

        vals_expected <- c("reject", "approve")
        vals_found <- unique(sheet_df$decision)

        if (!all(vals_found %in% vals_expected)) {

          vals_missing_msg <- glue::glue(
            "Values expected: {glue::glue_collapse(vals_expected, sep = ', ')}",
            "Values found: {glue::glue_collapse(vals_found, sep = ', ')}",
            .sep = "\n"
          )

          shinyFeedback::showToast(
            type = "error",
            title = "Unexpected values found in the `decision` column",
            message = vals_missing_msg
          )

          content_ok <- shiny::reactive({FALSE})

        } else {

          content_ok <- shiny::reactive({TRUE})

        }

      }

      if (cols_ok() & content_ok()) {

        # write inputs to R6 and to local storage
        r6$excel_path <- input$file$datapath
        r6$excel_sheet <- input$sheet
        r6$interviews_df <- sheet_df
        r6$file_provided <- TRUE
        r6$write()

        # signal that file selection saved
        gargoyle::trigger("file_provided")

      }

    })
 
  })
}
    
## To be copied in the UI
# mod_02_input_file_ui("02_input_file_1")
    
## To be copied in the server
# mod_02_input_file_server("02_input_file_1")

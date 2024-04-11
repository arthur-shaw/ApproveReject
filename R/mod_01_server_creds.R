#' 01_server_creds UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_01_server_creds_ui <- function(id){
  ns <- NS(id)
  tagList(

    bslib::accordion_panel(
      title = "Server credentials",
      value = "get_creds",
      icon = bsicons::bs_icon("shield-lock-fill"),
      shiny::textInput(
        inputId = ns("server"),
        label = shiny::tags$p(
          "Server URL",
          bsicons::bs_icon("browser-chrome")
        )
      ),
      shiny::textInput(
        inputId = ns("workspace"),
        label = shiny::tags$p(
          "Workspace",
          bsicons::bs_icon("diagram-3-fill")
        ),
      ),
      shiny::textInput(
        inputId = ns("user"),
        label = shiny::tags$p(
          "API user name",
          fontawesome::fa(name = "user-shield")
        )
      ),
      shiny::passwordInput(
        inputId = ns("password"),
        label = shiny::tags$p(
          "API user's password",
          bsicons::bs_icon("unlock-fill")
        )
      ),
      shiny::actionButton(
        inputId = ns("save"),
        label = "Connect"
      )
    )
 
  )
}
    
#' 01_server_creds Server Functions
#'
#' @noRd 
mod_01_server_creds_server <- function(id, r6){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    shiny::observeEvent(input$save, {

      # require input fields
      shiny::req(input$server, input$workspace, input$user, input$password)

      # set credentials
      susoapi::set_credentials(
        server = input$server,
        workspace = input$workspace,
        user = input$user,
        password = input$password
      )

      # check credentials are valid
      creds_ok <- shiny::reactive({
        susoapi::check_credentials(verbose = TRUE)
      })

      # react to validity of credentials
      if (creds_ok()) {

        # write credentials to R6 and local storage
        r6$server <- input$server
        r6$workspace <- input$workspace
        r6$user <- input$user
        r6$password <- input$password
        r6$creds_provided <- TRUE
        r6$write()

        # signal that credentials saved
        gargoyle::trigger("creds_saved")

      } else {

        # inform the user credentials invalid
        shinyFeedback::showToast(
          type = "error",
          message = "Server credentials invalid"
        )

      }

    })

  })
}
    
## To be copied in the UI
# mod_01_server_creds_ui("01_server_creds_1")
    
## To be copied in the server
# mod_01_server_creds_server("01_server_creds_1")

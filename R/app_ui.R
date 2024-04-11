#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    bslib::page_fluid(
      # load dependencies
      shinyFeedback::useShinyFeedback(),
      # define UI
      theme = bslib::bs_theme(version = 5),
      h1("ApproveReject"),
      bslib::accordion(
        id = "main_panel",
        # server credentials
        mod_01_server_creds_ui("01_server_creds_1"),
        # input file
        mod_02_input_file_ui("02_input_file_1"),
        # accept / reject interviews
        mod_03_accept_reject_ui("03_accept_reject_1")
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "ApproveReject"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

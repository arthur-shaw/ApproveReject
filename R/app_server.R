#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # create application directory
  app_dir <- create_user_app_dir()

  # initialize R6 object
  r6 <- r6$new()
  r6$app_dir <- app_dir

  # handle R6 object as a function of (past) app state
  # case 1: app never used => ; past R6 not found as RDS in local storage
  # write new R6 to local storage
  if (!fs::file_exists(fs::path(app_dir, "saved_r6.rds"))) {
    r6$write()
  # case 2: app used => RDS exists in app's user data folder
  # restore R6 from past session by reading its values from RDS to R6
  } else {
    r6$read()
  }

  # initialize event listeners
  gargoyle::init("creds_saved")
  gargoyle::init("file_provided")
  gargoyle::init("processing_launched")

  # load server definitions
  mod_01_server_creds_server("01_server_creds_1", r6 = r6)
  mod_02_input_file_server("02_input_file_1", r6 = r6)
  mod_03_accept_reject_server("03_accept_reject_1", r6 = r6)

  # manage opening and collapsing of accordion panels
  # creds to qnr
  gargoyle::on("creds_saved", {
    # close current
    bslib::accordion_panel_close(id = "main_panel", values = "get_creds")
    # open next
    bslib::accordion_panel_open(id = "main_panel", values = "get_file")
  })
  # qnr to action
  gargoyle::on("file_provided", {
    # close current
    bslib::accordion_panel_close(id = "main_panel", values = "get_file")
    # open next
    bslib::accordion_panel_open(id = "main_panel", values = "do_actions")
  })

}

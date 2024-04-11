#' 03_accept_reject UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_03_accept_reject_ui <- function(id){
  ns <- NS(id)
  tagList(

    bslib::accordion_panel(
      title = "Accept / reject interviews",
      value = "do_actions",
      icon = fontawesome::fa(name = "list-check"),
      shiny::actionButton(
        inputId = ns("run"),
        label = "Run"
      )
    )
 
  )
}
    
#' 03_accept_reject Server Functions
#'
#' @noRd 
mod_03_accept_reject_server <- function(id, r6){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

# TODO: add this choice to the UI
statuses_to_reject <- c(100, 120)
statuses_to_approve <- c(100, 120)

    shiny::observeEvent(input$run, {

      # =======================================================================
      # Prepare data
      # =======================================================================

      # injest interview list, filtering to those with a decision
      interview_list <- r6$interviews_df |>
        dplyr::select(
          interview__id = .data$interviewId, 
          .data$decision, 
          interview__status = .data$status, 
          comment
        ) |>
        dplyr::filter(!is.na(.data$decision))

      # interviews to approve
      to_approve <- interview_list |>
        dplyr::filter(.data$decision == "approve") |>
        dplyr::select(
          .data$interview__id, .data$interview__status,
          approve_comment = .data$comment
        ) |>
        dplyr::mutate(approve_comment = as.character(.data$approve_comment))

      # interviews to reject
      to_reject <- interview_list |> 
        dplyr::filter(.data$decision == "reject") |>
        dplyr::select(
          .data$interview__id, .data$interview__status,
          reject_comment = .data$comment
        ) |>
        dplyr::mutate(reject_comment = as.character(.data$reject_comment))

      # =======================================================================
      # Reject
      # =======================================================================

      # reject: apply the `reject_interview()` function to each interview
      if (nrow(to_reject) > 0) {

        # reject from current status to next status lower in statuses
        purrr::pwalk(
          .l = to_reject,
          .f = susoreview::reject_interview,
          statuses_to_reject = statuses_to_reject
        )

        # reject intially HQ-approved cases to interviewers
        obs_hq_approved <- to_reject |> 
          dplyr::filter(.data$interview__status == 130) |>
          dplyr::select(
            interview_id = .data$interview__id, 
            comment = .data$reject_comment
          )

        if (nrow(obs_hq_approved) > 0) {
          purrr::pwalk(
            .l = obs_hq_approved,
            .f = susoapi::reject_interview_as_sup
          )
        }

      }

      # =======================================================================
      # Approve
      # =======================================================================

      # approve: apply `approve_interview()` function to each interview
      if (nrow(to_approve) > 0) {
        purrr::pwalk(
          .l = to_approve,
          .f = susoreview::approve_interview,
          statuses_to_approve = statuses_to_approve
        )
      }

      # siginal that approve/reject action run
      gargoyle::trigger("processing_launched")

    })
  })
}
    
## To be copied in the UI
# mod_03_accept_reject_ui("03_accept_reject_1")
    
## To be copied in the server
# mod_03_accept_reject_server("03_accept_reject_1")

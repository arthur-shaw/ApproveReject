#' Container for Shiny usage
#' 
#' @field app_dir Character, atomic. Path to Shiny app's user data directory
#' @field server Character, atomic. URL of SuSo server.
#' @field workspace Character, atomic. Name of SuSo workspace.
#' @field user Character, atomic. API user name.
#' @field password Character, atomic. API user's password.
#' @field creds_provided Logical. Whether SuSo creds already provided.
#' @field excel_path Character, atomic. Path to Excel file containing
#' interviews to process.
#' @field excel_sheet Character, atomic. Name of sheet containing interviews
#' to process.
#' @field interviews_df Data frame. Interviews read from the Excel sheet
#' @field file_provided Logical. Whether file already provided.
r6 = R6::R6Class(
  classname = "r6",
  public = list(

    # ==========================================================================
    # Fields
    # ==========================================================================

    app_dir = NULL,
    # server credentials
    server = NULL,
    workspace = NULL,
    user = NULL,
    password = NULL,
    creds_provided = NULL,
    # input file
    excel_path = NULL,
    excel_sheet = NULL,
    interviews_df = NULL,
    file_provided = NULL,

    # ==========================================================================
    # Methods
    # ==========================================================================

    #' Read past R6 values from disk
    #' 
    #' @description
    #' Perform the following tasks:
    #' 
    #' - Read RDS on disk
    #' - Populate R6 fields with values from RDS
    #' 
    #' @param path Character. Path to the RDS file containing R6 values.
    read = function(path = fs::path(self[["app_dir"]], "saved_r6.rds")) {

      # read setup file from disk
      input_file <- readRDS(path)

      # collect names of fields in setup file
      fields <- names(input_file)

      # populate the R6 object with the corresponding setup file value
      # data frame fields need to be extracted from a list
      # "scalar" fields can be extracted directly
      for (field in fields) {
          field_type <- typeof(input_file[[field]])
          if (field_type == "list") {
            self[[field]] <- input_file[[field]][[1]]
          } else {
            self[[field]] <- input_file[[field]]
          }
      }

    },

    #' Write R6 to disk as RDS file
    #' 
    #' @description
    #' Write all R6 fields to a single RDS file, from which they can be 
    #' "restored" with the `read()` method above
    #' 
    #' @param Character. Path where RDS files should be written
    #' 
    #' @noRd
    write = function(path = fs::path(self[["app_dir"]], "saved_r6.rds")) {

      # data frame fields
      df_fields <- c("interviews_df")

      # "scalar" fields
      # introspect to obtain vector fields and methods
      fields <- names(self)

      # remove system components and methods
      fields <- fields[
          ! fields %in% c(
              # system components
              ".__enclos_env__", "clone",
              # methods
              "write", "update", "read",
              # omitting data frames
              df_fields
          )
      ]

      # put fields in data frame
      # create empty 1-row data frame
      df <- tibble::tibble(.rows = 1)

      # iteratively populate it with the value of all non-df fields
      for (field in fields) {
          df[[field]] <- self[[field]]
      }
    
      # put data frames in data frame
      for (df_field in df_fields) {
        df[[df_field]] <- list(self[[df_field]])
      }

      # write data frame to disk
      saveRDS(
          object = df,
          file = path
      )

    }

  )

)

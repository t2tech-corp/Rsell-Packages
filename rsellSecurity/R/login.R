#' Login Function to Validate Password
#'
#' This function is the Password Validation and other checks.
#'
#' Requires: shinyjs, sodium
#'
#' @param input Session input
#' @param output Session output
#' @param session Session info
#' @param log_out Logout check
#' @return Authorized Credentials
#' @export
#' @examples
#'

login <- function(input, output, session, log_out = NULL) {

    credentials <- reactiveValues(user_auth = FALSE)

    ###

    observeEvent(log_out(), {

        credentials$user_auth <- FALSE

        updateTextInput(session, "password", value = "")

    })

    ###

    observeEvent(credentials$user_auth, ignoreInit = TRUE, {

        toggle(id = "panel")

    })

    ##### Validate Submitted User ID and Password ####

    observeEvent(input$button, {

        db_error <- valid_user <- password_match <- active_account <- authorized_user <- FALSE

        #### Check for Valid Tables ####

        if( !user_table_exists | !pm_sales_table_exists | !inv_table_exists | !user_goals_table_exists ) {

            db_error <- TRUE

        }

        #### Fetch User Records and Validate User ####

        if(!db_error) {

            user_record <- Get_User_Record(input$user_name)

            if(nrow(user_record) > 0) { valid_user <- TRUE }

        }

        #### Validate Password ####

        if(valid_user) {

            password_match <- sodium::password_verify(user_record$pwd_hash, input$password)

        }

        #### Validate Status ####

        if(password_match) {

            if(user_record$status == "Active") { active_account <- TRUE }

        }

        #### Validate App Access and Update Last Logon ####

        if(active_account) {

            authorized_user <- TRUE

            credentials$user_auth <- TRUE

            session_user_record <<- user_record

            user_record$last_logon <- as.character(Sys.time())

            Update_User_Last(user_record)

        }

        #### Generate Error Message Based on Statuses ####

        if(db_error) {

            toggle(id = "db_error", anim = TRUE, time = 1, animType = "fade")
            delay(2500, toggle(id = "db_error", anim = TRUE, time = 1, animType = "fade"))

        } else

        if(!valid_user) {

            toggle(id = "id_error", anim = TRUE, time = 1, animType = "fade")
            delay(2500, toggle(id = "id_error", anim = TRUE, time = 1, animType = "fade"))

        } else

        if(!password_match) {

            toggle(id = "pw_error", anim = TRUE, time = 1, animType = "fade")
            delay(2500, toggle(id = "pw_error", anim = TRUE, time = 1, animType = "fade"))

        } else

        if(!active_account) {

            toggle(id = "ia_error", anim = FALSE, time = 1, animType = "fade")
            delay(2500, toggle(id = "ia_error", anim = TRUE, time = 1, animType = "fade"))

        } else

        if(!authorized_user) {

            toggle(id = "au_error", anim = FALSE, time = 1, animType = "fade")
            delay(2500, toggle(id = "au_error", anim = TRUE, time = 1, animType = "fade"))

        }

    })

    #### return reactive list containing auth boolean and user information

    reactive({

        reactiveValuesToList(credentials)

    })

}

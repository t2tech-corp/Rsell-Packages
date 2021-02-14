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

    ####  Initialize Credentials ---------------------------------------------------------------------------------------------------- ####
    ##                                                                                                                                  ##

    credentials <- reactiveValues(user_auth = FALSE)

    ##                                                                                                                                  ##
    #### ---------------------------------------------------------------------------------------------------------------------------- ####


    ####  Logout Observer ----------------------------------------------------------------------------------------------------------- ####
    ##                                                                                                                                  ##

    observeEvent(log_out(), {

        credentials$user_auth <- FALSE

        updateTextInput(session, "password", value = "")

    })

    ##                                                                                                                                  ##
    #### ---------------------------------------------------------------------------------------------------------------------------- ####


    ####  User Auth Observer -------------------------------------------------------------------------------------------------------- ####
    ##                                                                                                                                  ##

    observeEvent(credentials$user_auth, ignoreInit = TRUE, {

        toggle(id = "panel")

    })

    ##                                                                                                                                  ##
    #### ---------------------------------------------------------------------------------------------------------------------------- ####



    ####  Login Button Observer ----------------------------------------------------------------------------------------------------- ####
    ##                                                                                                                                  ##

    observeEvent(input$button, {

        ####  Initialize Login Sequence Statuses ------------------------------------------------------------------------------------ ####
        ##                                                                                                                              ##

        db_error <- valid_user <- password_match <- active_account <- authorized_user <- admin_user <- FALSE

        ##                                                                                                                              ##
        #### ------------------------------------------------------------------------------------------------------------------------ ####


        ####  Check for Valid Tables ------------------------------------------------------------------------------------------------ ####
        ##                                                                                                                              ##

        if( !user_table_exists | !pm_sales_exists | !inv_table_exists | !user_goals_exists ) { db_error <- TRUE }

        ##                                                                                                                              ##
        #### ------------------------------------------------------------------------------------------------------------------------ ####


        ####  Fetch User Records and Validate User ---------------------------------------------------------------------------------- ####
        ##                                                                                                                              ##

        if(!db_error) {

            user_record <- DB_Read_Table_User(user_table_name, input$user_name)

            if(nrow(user_record) == 1) { valid_user <- TRUE }

        }

        ##                                                                                                                              ##
        #### ------------------------------------------------------------------------------------------------------------------------ ####


        ####  Validate Password ----------------------------------------------------------------------------------------------------- ####
        ##                                                                                                                              ##

        if(valid_user) { password_match <- sodium::password_verify(user_record$pwd_hash, input$password) }

        ##                                                                                                                              ##
        #### ------------------------------------------------------------------------------------------------------------------------ ####


        ####  Validate Status ------------------------------------------------------------------------------------------------------- ####
        ##                                                                                                                              ##

        if(password_match) { if(user_record$status == "Active") { active_account <- TRUE } }

        ##                                                                                                                              ##
        #### ------------------------------------------------------------------------------------------------------------------------ ####


        ####  Validate App Access --------------------------------------------------------------------------------------------------- ####
        ##                                                                                                                              ##

        if(active_account) { credentials$user_auth <- authorized_user <- TRUE }

        ##                                                                                                                              ##
        #### ------------------------------------------------------------------------------------------------------------------------ ####


        ####  Update User Last Logon ------------------------------------------------------------------------------------------------ ####
        ##                                                                                                                              ##

        if(authorized_user) {

            user_record$last_logon <- as.character(Sys.time())

            session_user_record <<- user_record

            #

            changed_idx <- which(names(user_record) == "last_logon")

            User_Record_Update(user_record, changed_idx)

            #

            if(user_record$role == "Admin") { admin_user <- TRUE }

        }

        ##                                                                                                                              ##
        #### ------------------------------------------------------------------------------------------------------------------------ ####


        ####  Load Inventory Table for User ----------------------------------------------------------------------------------------- ####
        ##                                                                                                                              ##

        if(authorized_user & !admin_user) {

            if(is.null(inv_table)) { inv_table <<- DB_Read_Table_User(inv_table_name) }

            inv_table <<- Sort_Inv_Table(inv_table)

        }

        ##                                                                                                                              ##
        #### ------------------------------------------------------------------------------------------------------------------------ ####


        ####  Generate Error Message Based on Statuses ------------------------------------------------------------------------------ ####
        ##                                                                                                                              ##

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

        ##                                                                                                                              ##
        #### ------------------------------------------------------------------------------------------------------------------------ ####

    })



    ####  Return Reactive List - Credentials ---------------------------------------------------------------------------------------- ####
    ##                                                                                                                                  ##

    reactive({

        reactiveValuesToList(credentials)

    })

    ##                                                                                                                                  ##
    #### ---------------------------------------------------------------------------------------------------------------------------- ####

}

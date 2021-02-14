#' Deletes User Record from Database
#'
#' This function Deletes User Record from Database.
#'
#' @param  d_user_record The user record to update in the database.
#' @export
#'

User_Record_Delete <- function(d_user_record) {

    ####  Format Query ---------------------------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    my_query <- "DELETE FROM TABLENAME WHERE user_id = 'USERID'"

    #

    my_query <- gsub("TABLENAME", user_table_name,       my_query)
    my_query <- gsub("USERID",    d_user_record$user_id, my_query)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Execute Query --------------------------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    DB_Execute_Command(my_query)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####

}

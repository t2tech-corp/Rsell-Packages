#' Updates User Record in Database
#'
#' This function Updates User Record in Database.
#'
#' Requires: RPostgres, dplyr, shinytoastr
#'
#' @param  e_user_record The user record to update in the database.
#' @param  changed_idx The list of fields to update.
#' @export
#'

User_Record_Update <- function(e_user_record, changed_idx) {

    ####  Create Table Names ---------------------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    table_names <- names(e_user_record)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Handle Lone Apostrophe in Fields -------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    e_user_record$first_name <- gsub("'", "''", e_user_record$first_name)
    e_user_record$last_name  <- gsub("'", "''", e_user_record$last_name)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Format Query ---------------------------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    query_1 <- "UPDATE TABLENAME SET"
    query_2 <- "VARNAME = 'VARVALUE'"
    query_3 <- "WHERE user_id = 'USERID'"

    #

    query_1 <- gsub("TABLENAME", user_table_name,       query_1)
    query_3 <- gsub("USERID",    e_user_record$user_id, query_3)

    #

    for (i in 1:length(changed_idx)) {

        t_query <- gsub("VARNAME",  table_names[changed_idx[i]],      query_2)
        t_query <- gsub("VARVALUE", e_user_record[1, changed_idx[i]], t_query)

        if(i == 1) { m_query_2 <- t_query } else
                   { m_query_2 <- paste(m_query_2, t_query, sep = ", ") }

    }

    my_query <- paste(query_1, m_query_2, query_3, sep = " ")

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Execute Update -------------------------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    DB_Execute_Command(my_query)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


}

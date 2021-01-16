#' Update User Profile
#'
#' This function Updates User Profile information in User Table.
#'
#' Requires: RPostgres
#'
#' @param user_record The user record.
#' @param changed_idx The index of changed fields.
#' @return session_user_record
#' @export
#' @examples
#'

Update_User_Profile <- function(user_record, changed_idx) {

    table_names <- names(user_record)

    #### Handle Lone Apostrophe in Fields ####

    t_user_record <- user_record

    user_record$first_name <- gsub("'", "''", user_record$first_name)
    user_record$last_name  <- gsub("'", "''", user_record$last_name)

    ##### Format Query for 1 to Many Value Changes ####

    query_1 <- "UPDATE user_table SET"
    query_2 <- "VARNAME = 'VARVALUE'"
    query_3 <- "WHERE user_id = 'USERID'"

    query_3 <- gsub("USERID", user_record$user_id, query_3)

    for (i in 1:length(changed_idx)) {

        t_query <- gsub("VARNAME",  table_names[changed_idx[i]],    query_2)
        t_query <- gsub("VARVALUE", user_record[1, changed_idx[i]], t_query)

        if(i == 1) { m_query_2 <- t_query } else
                   { m_query_2 <- paste(m_query_2, t_query, sep = ", ") }

    }

    my_query <- paste(query_1, m_query_2, query_3, sep = " ")

    #### Refresh DB Connector if Needed ####

    if(!RPostgres::dbIsValid(db_con)) { db_con <- DB_Connect(db_name, db_host, db_user, db_pass) }

    ##### Process Update #####

    RPostgres::dbBegin(db_con)

    RPostgres::dbExecute(db_con, my_query)

    RPostgres::dbCommit(db_con)

    ##### Refresh Credentials Info #####

    session_user_record <<- t_user_record

}

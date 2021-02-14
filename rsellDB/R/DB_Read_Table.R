#' Reads a Database Table
#'
#' This function Reads a Database Table.
#'
#' Requires: RPostgres
#'
#' @param table_name The name of the DB Table to check.
#' @return query_table Data frame containing the table data.
#' @export
#'

DB_Read_Table <- function(table_name) {

    ####  Refresh DB Connector if Needed ---------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    if(!RPostgres::dbIsValid(db_con)) { db_con <- DB_Connect(db_name, db_host, db_user, db_pass) }

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Execute Query --------------------------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    query_table <- RPostgres::dbReadTable(conn = db_con, name = table_name)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Return Query Result --------------------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    return(query_table)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####

}

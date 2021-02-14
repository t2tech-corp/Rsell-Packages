#' List Tables in Database
#'
#' This function lists the tables in the database.
#'
#' Requires: RPostgres
#'
#' @param db_con The DB Connection parameters.
#' @return table_list Character array.
#' @export
#'

DB_List_Tables <- function(db_con) {

    ####  Refresh DB Connector if Needed ---------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    if(!RPostgres::dbIsValid(db_con)) { db_con <- DB_Connect(db_name, db_host, db_user, db_pass) }

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Execute Query --------------------------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    table_list <- RPostgres::dbListTables(conn = db_con)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Return Query Result --------------------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    return(table_list)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####

}

#' Appends a Record or Records to a Database Table
#'
#' This function appends to a Database Table.
#'
#' Requires: RPostgres
#'
#' @param table_name The name of the DB Table to check.
#' @param table_record The record(s) to write to the Database Table.
#' @return 
#' @export
#'

DB_Append_Table <- function(table_name, table_record) {
    
    ####  Refresh DB Connector if Needed ---------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##
    
    if(!RPostgres::dbIsValid(db_con)) { db_con <- DB_Connect(db_name, db_host, db_user, db_pass) }
    
    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####
    
    
    ####  Execute DB Write ------------------------------------------------------------------------------------------------------ ####
    ##                                                                                                                              ##
    
    RPostgres::dbWriteTable(db_con, name = table_name, value = table_record, append = TRUE, overwrite = FALSE)
    
    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####
    
}
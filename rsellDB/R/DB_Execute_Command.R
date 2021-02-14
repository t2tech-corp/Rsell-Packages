#' Executes a Command to the Database
#'
#' This function Executes a Command to the Database.
#'
#' Requires: RPostgres
#'
#' @param  my_command The command to execute.
#' @export
#'

DB_Execute_Command <- function(my_command) {
    
    ####  Refresh DB Connector if Needed ---------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##
    
    if(!RPostgres::dbIsValid(db_con)) { db_con <- DB_Connect(db_name, db_host, db_user, db_pass) }
    
    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####
    
    
    ####  Execute DB Update ----------------------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##
    
    RPostgres::dbBegin(db_con)
    
    RPostgres::dbExecute(db_con, my_command)
    
    RPostgres::dbCommit(db_con)
    
    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####
    
}
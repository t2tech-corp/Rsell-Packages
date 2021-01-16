#' Check for Table in Database
#'
#' This function checks for the existence of a specific table in the database.
#'
#' Requires: RPostgres
#'
#' @param db_con The DB Connection parameters.
#' @param table_name The name of the DB Table to check.
#' @return table_exist Logical value.
#' @export
#' @examples
#'

DB_Table_Exists <- function(db_con, table_name) {

    table_exist <- RPostgres::dbExistsTable(conn = db_con, name = table_name)

    return(table_exist)

}

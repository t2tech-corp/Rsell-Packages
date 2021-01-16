#' Disconnect the PostgreSQL Database
#'
#' This function disconnects the PostgreSQL Database.
#'
#' Requires: RPostgres
#'
#' @param  db_con
#' @return db_con The connection parameters to the database.
#' @export
#' @examples
#'

DB_Disconnect <- function(db_con) {

    RPostgres::dbDisconnect(db_con)

}

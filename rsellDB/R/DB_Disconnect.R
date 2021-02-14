#' Disconnect the PostgreSQL Database
#'
#' This function disconnects the PostgreSQL Database.
#'
#' Requires: RPostgres
#'
#' @param  db_con The connection parameters to the database.
#' @return db_con The connection parameters to the database.
#' @export
#'

DB_Disconnect <- function(db_con) {

    RPostgres::dbDisconnect(db_con)

}

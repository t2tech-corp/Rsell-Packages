#' Connect to PostgreSQL Database
#'
#' This function connects to the PostgreSQL Database.
#'
#' Requires: RPostgres
#'
#' @param db_name The name of the database.
#' @param db_host The database hostname.
#' @param db_user The database user ID.
#' @param db_pass The database password for the user ID.
#' @return db_con The connection parameters to the database.
#' @export
#'

DB_Connect <- function(db_name, db_host, db_user, db_pass) {

    db_con <- RPostgres::dbConnect(

        drv      = RPostgres::Postgres(),
        dbname   = db_name,
        host     = db_host,
        port     = "5432",
        user     = db_user,
        password = db_pass

    )

    return(db_con)

}

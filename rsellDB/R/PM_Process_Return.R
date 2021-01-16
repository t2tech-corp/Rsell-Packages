#' Deletes Poshmark Sales Activity Record from Database
#'
#' This function Deletes Poshmark Sales Activity Record from Database.
#'
#' Requires: RPostgres
#'
#' @param  inv_record
#' @return Poshmark Sales Table as data frame.
#' @export
#' @examples
#'

PM_Process_Return <- function(inv_record) {

    #### Refresh DB Connector if Needed ####

    if(!RPostgres::dbIsValid(db_con)) { db_con <- DB_Connect(db_name, db_host, db_user, db_pass) }

    #### Create Query ####

    my_query <- "DELETE FROM pm_sales WHERE user_id = 'USERID' AND sku = 'SKU' "

    my_query <- gsub("USERID", session_user_record$user_id, my_query)
    my_query <- gsub("SKU",    inv_record$sku,              my_query)

    #### Send to DB ####

    RPostgres::dbExecute(db_con, my_query)

    #### Update Record in Memory

    pm_index <- which(pm_sales$sku == inv_record$sku)

    pm_sales <<- pm_sales[-pm_index, ]

}

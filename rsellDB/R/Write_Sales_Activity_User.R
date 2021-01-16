#' Writes Poshmark Sales Activity to Database
#'
#' This function Writes Poshmark Sales Activity to Database for user.
#'
#' Requires: RPostgres, dplyr
#'
#' @param  sales_activity
#' @return Poshmark Sales Table as data frame.
#' @export
#' @examples
#'

Write_Sales_Activity_User <- function(sales_activity) {

    #### Refresh DB Connector if Needed ####

    if(!RPostgres::dbIsValid(db_con)) { db_con <- DB_Connect(db_name, db_host, db_user, db_pass) }

    #### Append Data to Table ####

    sales_activity$date_add_db <- as.character(Sys.time())

    RPostgres::dbWriteTable(db_con, name = "pm_sales", value = sales_activity, append = TRUE, overwrite = FALSE)

    #### Add to In Memory pm_sales ####

    pm_sales <<- bind_rows(pm_sales, sales_activity)

}

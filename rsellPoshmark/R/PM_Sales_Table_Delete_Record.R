#' Deletes Poshmark Sales Activity Record from Database
#'
#' This function Deletes Poshmark Sales Activity Record from Database.
#'
#' Requires: RPostgres
#'
#' @param  inv_record
#' @return
#' @export
#'

PM_Sales_Table_Delete_Record <- function(inv_record) {

    ####  Build Query ----------------------------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    my_query <- "DELETE FROM TABLENAME WHERE user_id = 'USERID' AND sku = 'SKU' "

    my_query <- gsub("TABLENAME", pm_sales_name,      my_query)
    my_query <- gsub("USERID",    inv_record$user_id, my_query)
    my_query <- gsub("SKU",       inv_record$sku,     my_query)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Execute Query --------------------------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    DB_Execute_Command(my_query)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####

}

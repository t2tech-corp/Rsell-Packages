#' Queries Database and Returns Inventory Table
#'
#' This function Queries the Database and Returns an Inventory Table for the user.
#'
#' Requires: RPostgres, dplyr
#'
#' @return Inventory Table as a data frame.
#' @export
#' @examples
#'

Get_Inventory_User <- function() {

    #### Refresh DB Connector if Needed ####

    if(!RPostgres::dbIsValid(db_con)) { db_con <- DB_Connect(db_name, db_host, db_user, db_pass) }

    #### Build Query ####

    my_query <- "SELECT * FROM inv_table WHERE user_id = 'USERID'"

    my_query <- gsub("USERID", session_user_record$user_id, my_query)

    my_query <- RPostgres::dbSendQuery(db_con, my_query)

    inv_table <- RPostgres::dbFetch(my_query)

    RPostgres::dbClearResult(my_query)

    #### Fix Bin Number Change ####

    inv_table <- Fix_Bin_Number(inv_table)

    #### Arrange by Year, Month, Counter ####

    inv_table <- inv_table %>%
                 mutate(t_year  = substr(sku, 3, 4),
                        t_month = substr(sku, 1, 2),
                        t_count = substr(sku, 6, 9)) %>%
                 arrange(desc(t_year), desc(t_month), desc(t_count)) %>%
                 select(-t_year, -t_month, -t_count)

    ###

    return(inv_table)

}

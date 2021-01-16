#' Queries Database for Poshmark Sales Activity
#'
#' This function Queries Database for Poshmark Sales Activity for user.
#'
#' Requires: RPostgres, dplyr
#'
#' @return Poshmark Sales Table as data frame.
#' @export
#' @examples
#'

Get_PM_Sales_Activity_User <- function() {

    #### Refresh DB Connector if Needed ####

    if(!RPostgres::dbIsValid(db_con)) { db_con <- DB_Connect(db_name, db_host, db_user, db_pass) }

    #### Build Query ####

    my_query <- "SELECT * FROM pm_sales WHERE user_id = 'USERID'"

    my_query <- gsub("USERID", session_user_record$user_id, my_query)

    my_query <- RPostgres::dbSendQuery(db_con, my_query)

    pm_sales <- RPostgres::dbFetch(my_query)

    RPostgres::dbClearResult(my_query)

    #### Adjustments for category and sub-category changes ####

    pm_sales <- pm_sales %>%
                mutate(category = case_when(department == "Women" & category == "Pants" ~ "Pants & Jumpsuits",
                                            TRUE ~ category))

    pm_sales <- pm_sales %>%
                mutate(sub_category = case_when(department == "Women" & category == "Jackets & Coats" & sub_category == "Blazers" ~ "Blazers & Suit Jackets",
                                                TRUE ~ sub_category))

    #### Return Function Data ####

    if(nrow(pm_sales) > 0) { pm_sales <- pm_sales %>% arrange(as.Date(order_date), as.Date(listing_date), order_id) }

    ###

    return(pm_sales)

}

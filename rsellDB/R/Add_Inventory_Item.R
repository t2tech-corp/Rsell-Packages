#' Adds Inventory Item to Database
#'
#' This function Adds Inventory Item to Database.
#'
#' Requires: RPostgres, dplyr, shinytoastr
#'
#' @param  inv_record The inventory items to add to the database.
#' @return Updated Inventory Table as data frame.
#' @export
#' @examples
#'

Add_Inventory_Item <- function(inv_record) {

    #### Show Toaster Info ####

    t_message <- paste(nrow(inv_record), "Inventory Item(s) Being Added", sep = " ")

    Toast_Message(t_message, "Info")

    #### Refresh DB Connector if Needed ####

    if(!RPostgres::dbIsValid(db_con)) { db_con <- DB_Connect(db_name, db_host, db_user, db_pass) }

    RPostgres::dbWriteTable(db_con, name = "inv_table", value = inv_record, append = TRUE, overwrite = FALSE)

    #### After DB Commit, Update inv_table in Memory ####

    inv_table <<- bind_rows(inv_table, inv_record)

    #### Arrange by Year, Month, Counter ####

    inv_table <<- inv_table %>%
                  mutate(t_year  = substr(sku, 3, 4),
                         t_month = substr(sku, 1, 2),
                         t_count = substr(sku, 6, 9)) %>%
                  arrange(desc(t_year), desc(t_month), desc(t_count)) %>%
                  select(-t_year, -t_month, -t_count)

}

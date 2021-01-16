#' Updates Inventory Item(s) in Database
#'
#' This function Updates Inventory Item(s) in Database.
#'
#' Requires: RPostgres, dplyr, shinytoastr
#'
#' @param  inv_record The inventory items to update in the database.
#' @return Updated Inventory Table as data frame.
#' @export
#' @examples
#'

Update_Inventory_Item <- function(inv_record, changed_idx, table_names, t_action) {

    #### Check for Multiple Inventory Items Requiring Update ####

    if(nchar(inv_record$sku) == 14 & t_action == "EDIT") {

        inv_idx <- grep(substr(inv_record$sku, 1, 9), inv_table$sku)

        #### Check Item Status - Inventory or Listed Only

        sku_list <- inv_table[inv_idx,] %>% filter(item_status == "Inventory" | item_status == "Listed")

        sku_list <- as.character(sku_list$sku)

    } else {

        sku_list <- inv_record$sku[1]
    }

    #### Show Toaster Info ####

    t_message <- paste(length(sku_list), "Inventory Items Being Updated", sep = " ")

    Toast_Message(t_message, "Info")

    #### Process Each Item in SKU List ####

    for (j in 1:length(sku_list)) {

        inv_record$sku[1] <- sku_list[j]

        #### Handle Lone Apostrophe in Fields - Use t_inv_record for In Memory Table Update to Avoid Double ' ####

        t_inv_record <- inv_record

        inv_record$brand[1]            <- gsub("'", "''", inv_record$brand[1])
        inv_record$item_title[1]       <- gsub("'", "''", inv_record$item_title[1])
        inv_record$item_description[1] <- gsub("'", "''", inv_record$item_description[1])
        inv_record$item_source[1]      <- gsub("'", "''", inv_record$item_source[1])

        ##### Format Query for 1 to Many Value Changes ####

        query_1 <- "UPDATE inv_table SET"
        query_2 <- "VARNAME = 'VARVALUE'"
        query_3 <- "WHERE user_id = 'USERID' AND sku = 'SKU'"

        query_3 <- gsub("USERID", session_user_record$user_id, query_3)
        query_3 <- gsub("SKU",    inv_record$sku[1],           query_3)

        for (i in 1:length(changed_idx)) {

            t_query <- gsub("VARNAME",  table_names[changed_idx[i]],   query_2)
            t_query <- gsub("VARVALUE", inv_record[1, changed_idx[i]], t_query)

            if(i == 1) { m_query_2 <- t_query } else
                       { m_query_2 <- paste(m_query_2, t_query, sep = ", ") }

        }

        my_query <- paste(query_1, m_query_2, query_3, sep = " ")

        #### Refresh DB Connector if Needed ####

        if(!RPostgres::dbIsValid(db_con)) { db_con <- DB_Connect(db_name, db_host, db_user, db_pass) }

        ##### Process Update #####

        RPostgres::dbBegin(db_con)

        db_return <- RPostgres::dbExecute(db_con, my_query)

        RPostgres::dbCommit(db_con)

        #### After DB Commit, Update inv_table in Memory ####

        inv_index <- which(inv_table$sku == t_inv_record$sku)

        inv_table[inv_index,] <- t_inv_record

        inv_table <<- inv_table

    }

}

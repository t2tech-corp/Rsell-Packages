#' Updates Inventory Record(s) in Database
#'
#' This function Updates Inventory Record(s) in Database.
#'
#' Requires: RPostgres, dplyr, shinytoastr
#'
#' @param  inv_record The inventory items to update in the database.
#' @param  changed_idx The index to changed fields.
#' @param  t_action The type of update action being performed.
#' @return Updated Inventory Table as data frame.
#' @export
#'

DB_Inventory_Record_Update <- function(inv_record, changed_idx, t_action) {

    ####  Create Table Names for Index ------------------------------------------------------------------------------------------ ####
    ##                                                                                                                              ##

    table_names <- names(inv_record)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Check for Multiple Inventory Items Requiring Update ------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    if(nchar(inv_record$sku) == 14 & t_action == "EDIT") {

        inv_idx <- grep(substr(inv_record$sku, 1, 9), inv_table$sku)

        sku_list <- inv_table[inv_idx,] %>% filter(item_status == "Inventory" | item_status == "Listed" | item_status == "Draft")

        sku_list <- as.character(sku_list$sku)

    } else {

        sku_list <- inv_record$sku[1]
    }

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Show Toaster Info ----------------------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    if(t_action != "Settle") {

        t_message <- paste(length(sku_list), "Inventory Items Being Updated", sep = " ")

        Toast_Message(t_message, "Info")


    }

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Process Each Item in SKU List ----------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    for (j in 1:length(sku_list)) {

        inv_record$sku[1] <- sku_list[j]

        ####  Handle Lone Apostrophe in Fields -------------------------------------------------------------------------------------- ####
        ##                                                                                                                              ##

        t_inv_record <- inv_record

        inv_record$brand[1]            <- gsub("'", "''", inv_record$brand[1])
        inv_record$item_title[1]       <- gsub("'", "''", inv_record$item_title[1])
        inv_record$item_description[1] <- gsub("'", "''", inv_record$item_description[1])
        inv_record$item_source[1]      <- gsub("'", "''", inv_record$item_source[1])

        ##                                                                                                                              ##
        #### ------------------------------------------------------------------------------------------------------------------------ ####


        ####  Format Query ---------------------------------------------------------------------------------------------------------- ####
        ##                                                                                                                              ##

        query_1 <- "UPDATE TABLENAME SET"
        query_2 <- "VARNAME = 'VARVALUE'"
        query_3 <- "WHERE user_id = 'USERID' AND sku = 'SKU'"

        query_1 <- gsub("TABLENAME", inv_table_name,              query_1)
        query_3 <- gsub("USERID",    inv_record$user_id[1],       query_3)
        query_3 <- gsub("SKU",       inv_record$sku[1],           query_3)

        for (i in 1:length(changed_idx)) {

            t_query <- gsub("VARNAME",  table_names[changed_idx[i]],   query_2)
            t_query <- gsub("VARVALUE", inv_record[1, changed_idx[i]], t_query)

            if(i == 1) { m_query_2 <- t_query } else
                       { m_query_2 <- paste(m_query_2, t_query, sep = ", ") }

        }

        my_query <- paste(query_1, m_query_2, query_3, sep = " ")

        ##                                                                                                                              ##
        #### ------------------------------------------------------------------------------------------------------------------------ ####


        ####  Execute Update -------------------------------------------------------------------------------------------------------- ####
        ##                                                                                                                              ##

        DB_Execute_Command(my_query)

        ##                                                                                                                              ##
        #### ------------------------------------------------------------------------------------------------------------------------ ####

    }

}

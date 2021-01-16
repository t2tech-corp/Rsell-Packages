#' Fixes Blank SKU in Poshmark Sales Activity Tables
#'
#' This function Fixes Blank SKUs in Poshmark Sales Activity Tables.
#'
#' @param sales_activity Poshmark Sales Activity Table
#' @return sales_activity Updated Poshmark Sales Activity Table
#' @export
#' @examples
#'

Fix_Blank_SKU <- function(sales_activity) {

    for (i in 1:nrow(sales_activity)) {

        sku_len <- nchar(sales_activity$sku[i])

        #### SKU is Good - Next ####

        if(sku_len == 9) { next }

        #### SKU is Blank - Check Against PM Table using Order ID ####

        if(sku_len == 0 & sales_activity$bundled_order[i] == "No") {

            pm_idx <- which(pm_sales$order_id == sales_activity$order_id[i])

            if(length(pm_idx) == 1) {

                sales_activity$sku[i] <- pm_sales$sku[pm_idx]

                next

            }

        }

        #### SKU is Blank - Check Against PM Table using Title ####

        if(sku_len == 0) {

            pm_idx <- which(pm_sales$listing_title == sales_activity$listing_title[i])

            if(length(pm_idx) == 1) {

                sales_activity$sku[i] <- pm_sales$sku[pm_idx]

                next

            }

        }

        #### SKU is Blank - Check Against INV Table using Title ####

        if(sku_len == 0) {

            inv_idx <- which(inv_table$item_title == sales_activity$listing_title[i])

            if(length(inv_idx) == 1) {

                sales_activity$sku[i] <- inv_table$sku[inv_idx]

                next

            }

        }

        #### SKU is Blank - Perform Advanced Search

        if(sku_len == 0) {

            ### Filter Inventory Table by Sales Activity Values

            sku_inv_table <- inv_table %>%
                             filter(department  == sales_activity$department[i],
                                    category    == sales_activity$category[i],
                                    subcategory == sales_activity$sub_category[i],
                                    brand       == sales_activity$brand[i],
                                    item_status == "Sold")

            if(nrow(sku_inv_table) == 1) {

                sales_activity$sku[i] <- sku_inv_table$sku[1]

                next

            }

        }

    }

    return(sales_activity)

}

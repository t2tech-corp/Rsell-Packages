#' Update Inventory Table with Poshmark Sales Activity Table
#'
#' This function Updates the Inventory Table with the Poshmark Sales Activity Table.
#'
#' Requires:
#'
#' @param d_sales_activity Poshmark Sales Activity Report
#' @return Updates Inventory Table
#' @export
#' @examples
#'

Update_Inventory_PM <- function(d_sales_activity) {

    for (i in 1:nrow(d_sales_activity)) {

        #### Get Inventory Record ####

        inv_record <- inv_record_o <- inv_table %>% filter(sku == d_sales_activity$sku[i])

        if(nrow(inv_record) == 0) { next() }

        #### Update inv_record ####

        inv_record$item_title         <- d_sales_activity$listing_title[i]
        inv_record$department         <- d_sales_activity$department[i]
        inv_record$category           <- d_sales_activity$category[i]
        inv_record$subcategory        <- d_sales_activity$sub_category[i]
        inv_record$brand              <- d_sales_activity$brand[i]
        inv_record$color              <- d_sales_activity$color[i]
        inv_record$size               <- d_sales_activity$size[i]
        inv_record$new_with_tags      <- d_sales_activity$new_with_tags[i]
        inv_record$list_poshmark      <- "Yes"
        inv_record$list_date_poshmark <- d_sales_activity$listing_date[i]
        inv_record$item_status        <- "Settled"
        inv_record$sold_date          <- d_sales_activity$order_date[i]
        inv_record$sold_site          <- "Poshmark"
        inv_record$bundled            <- d_sales_activity$bundled_order[i]
        inv_record$sell_price         <- d_sales_activity$order_price[i]
        inv_record$net_earnings       <- d_sales_activity$net_earnings[i]
        inv_record$net_profit         <- d_sales_activity$net_profit[i]
        inv_record$other_info         <- d_sales_activity$other_info[i]
        inv_record$last_update        <- as.character(Sys.time())
        inv_record$last_action        <- "Settled"

        ####

        changed_idx <- Get_Changed_Data(inv_record_o, inv_record)
        changed_idx <- which(changed_idx == FALSE)

        table_names <- names(inv_record)

        ###

        Update_Inventory_Item(inv_record, changed_idx, table_names)

    }

}

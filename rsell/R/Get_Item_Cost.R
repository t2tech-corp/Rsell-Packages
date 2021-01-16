#' Get Item Cost for Poshmark Sales Activity
#'
#' This function finds the item cost for the Poshmark Sales Activity table.
#'
#' Requires:
#'
#' @param sales_activity Poshmark Sales Activity Report
#' @return sales_activity Poshmark Sales Activity Report
#' @export
#' @examples
#'

Get_Item_Cost <- function(sales_activity) {

    for (i in 1:nrow(sales_activity)) {

        inv_index <- which(inv_table$sku == sales_activity$sku[i])

        sales_activity$item_cost[i] <- inv_table$item_cost[inv_index]

        sales_activity$net_profit[i] <- sales_activity$net_earnings[i] - sales_activity$item_cost[i]

    }

    return(sales_activity)

}

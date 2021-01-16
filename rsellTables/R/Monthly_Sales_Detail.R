#' Creates a Monthly Sales Detail Table
#'
#' This function creates a Monthly Sales Detail Table from the Inventory Table.
#'
#' Requires: dplyr
#'
#' @param inv_table The Inventory Table for the user.
#' @return A Monthly Sales Detail Table as a data frame.
#' @export
#' @examples
#'

Monthly_Sales_Detail <- function(inv_table) {

    sell_status <- c("Sold", "Settled")

    sales_detail <- inv_table %>%
                    filter(item_status %in% sell_status) %>%
                    select(sku, item_title, sold_date, sold_site, sell_price, net_earnings, item_cost, net_profit, item_status) %>%
                    arrange(desc(as.Date(sold_date)))

    ###

    return(sales_detail)

}

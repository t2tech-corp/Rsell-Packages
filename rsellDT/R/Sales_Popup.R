#' Creates the Sales Popup Data Table
#'
#' This function creates a Sales Popup Data Table.
#'
#' Requires: DT, dplyr
#'
#' @param temp_table The table to be used for the report.
#' @return dt_report A Sales Popup Data Table.
#' @export
#' @examples
#'

Sales_Popup <- function(temp_table) {

    tbl_names <- c("SKU", "Title", "Sold Date", "Sold Site", "Sell Price", "Earnings", "Item Cost", "Profit", "Status")

    t_rec <- temp_table[1,]

    t_rec[1, c(1:4,9)] <- ""

    t_rec$sell_price   <- sum(temp_table$sell_price)
    t_rec$net_earnings <- sum(temp_table$net_earnings)
    t_rec$item_cost    <- sum(temp_table$item_cost)
    t_rec$net_profit   <- sum(temp_table$net_profit)

    temp_table <- bind_rows(temp_table, t_rec)

    tot_row <- nrow(temp_table)

    dt_report <- DT::datatable(temp_table, rownames = FALSE, editable = FALSE, colnames = tbl_names,
                               options = list(dom = 'lfrtip',
                                              initComplete = DT::JS(
                                                  "function(settings, json) {",
                                                  "$(this.api().table().header()).css({'background-color': '#333', 'color': '#fff'});",
                                                  "}"),
                                              lengthMenu = list(c(20, 40, 60, -1), c(20, 40, 60, "All")))) %>%
                 DT::formatCurrency(c("sell_price", "net_earnings", "item_cost", "net_profit"))

    ###

    return(dt_report)

}

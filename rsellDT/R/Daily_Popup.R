#' Creates the Daily Popup Data Table
#'
#' This function creates a Daily Popup Data Table.
#'
#' Requires: DT
#'
#' @param temp_table The table to generate the report.
#' @return dt_report A Daily Popup Data Table.
#' @export
#' @examples
#'

Daily_Popup <- function(temp_table) {

    tbl_names <- c("Sold Date", "Total Items Sold", "Total Sell Price", "Total Earnings", "Total Cost", " Total Profit")

    dt_report <- DT::datatable(temp_table, rownames = FALSE, editable = FALSE, colnames = tbl_names,
                               options = list(dom = 'lfrtip',
                                              initComplete = DT::JS(
                                                  "function(settings, json) {",
                                                  "$(this.api().table().header()).css({'background-color': '#333', 'color': '#fff'});",
                                                  "}"),
                                              lengthMenu = list(c(7, 14, 21, -1), c(7, 14, 21, "All")))) %>%
                 DT::formatCurrency(c("tot_sell", "tot_earn", "tot_cost", "tot_prof"))

    ###

    return(dt_report)

}

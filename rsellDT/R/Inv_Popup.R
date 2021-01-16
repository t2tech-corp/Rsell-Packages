#' Creates the Inventory Popup Data Table
#'
#' This function creates a Inventory Popup Data Table.
#'
#' Requires: DT
#'
#' @param temp_table The table to generate the report.
#' @return dt_report An Inventory Popup Data Table.
#' @export
#' @examples
#'

Inv_Popup <- function(temp_table) {

    tbl_names <- c("Date", "Starting Inventory", "Inventory Added", "Inventory Sold", "Ending Inventory")

    dt_report <- DT::datatable(temp_table, rownames = FALSE, editable = FALSE, colnames = tbl_names,
                               options = list(dom = 'lfrtip',
                                              initComplete = DT::JS(
                                                  "function(settings, json) {",
                                                  "$(this.api().table().header()).css({'background-color': '#333', 'color': '#fff'});",
                                                  "}"),
                                              lengthMenu = list(c(10, 20, 30, -1), c(10, 20, 30, "All"))))

    ###

    return(dt_report)

}

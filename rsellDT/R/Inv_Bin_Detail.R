#' Creates the Inventory Bin Detail Data Table
#'
#' This function creates a Inventory Bin Detail Data Table.
#'
#' Requires: DT
#'
#' @param b_inv_table The Inventory Table for the user.
#' @return A Inventory Bin Detail Data Table.
#' @export
#' @examples
#'

Inv_Bin_Detail <- function(b_inv_table) {

    file_name <- paste("Bin-Inventory-Detail", b_inv_table$bin_nbr[1], sep = "-")

    dt_col_names <- c("Bin Number", "SKU", "Status", "Date Added", "Title", "Brand", "Color", "Size", "New With Tags", "Item Source", "Item Cost")

    bin_DT <- DT::datatable(b_inv_table,
                            rownames = FALSE,
                            editable = FALSE,
                            colnames = dt_col_names,
                            extensions = c('Buttons'),
                            options = list(dom = 'Blfrtip',
                                           initComplete = DT::JS("function(settings, json)
                                                             {","$(this.api().table().header()).css({'background-color': '#fbe7e9', 'color': '#404041'});","}"),
                                           scroller = TRUE,
                                           scrollX = TRUE,
                                           scrollY = 700,
                                           buttons = list(list(extend = 'copy'),
                                                          list(extend = 'csv',   filename = file_name),
                                                          list(extend = 'excel', filename = file_name),
                                                          list(extend = 'print')),
                                           lengthMenu = list(c(20, 40, 60, -1), c(20, 40, 60, "All"))),
                            class = "display nowrap") %>%
              DT::formatCurrency(c('item_cost'))

    ###

    return(bin_DT)

}

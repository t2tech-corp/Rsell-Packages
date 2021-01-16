#' Creates the Poshmark Summary Data Table
#'
#' This function creates a Poshmark Summary Data Table.
#'
#' Requires: DT
#'
#' @param summary_table The table to be used for the report.
#' @param table_type The table type for the report.
#' @return A Poshmark Summary Data Table.
#' @export
#' @examples
#'

PM_Summary_Table <- function(summary_table, table_type) {

    file_name <- paste("Poshmark-XXXXX-Summary-Download", as.character(Sys.Date()), sep = "-")

    dt_col_names <- c("XXXXX", "Count", "Avg Days Listed", "Total Sales", "Average Sale", "Total Earnings", "Average Earnings", "Total Profit", "Average Profit")

    #### Customize Default Fields by Table Type ####

    file_name     <- gsub("XXXXX", table_type, file_name)
    dt_col_names  <- gsub("XXXXX", table_type, dt_col_names)

    #### Build Data Table ####

    summary_DT <- DT::datatable(summary_table,
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
                  DT::formatCurrency(c('tot_order_price', 'avg_order_price', 'tot_net_earnings', 'avg_net_earnings', 'tot_net_profit', 'avg_net_profit')) %>%
                  DT::formatRound(c('avg_days_listed'), 1)

    ###

    return(summary_DT)

}

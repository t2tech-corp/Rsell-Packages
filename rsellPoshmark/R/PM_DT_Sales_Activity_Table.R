#' Creates the Sales Activity Data Table
#'
#' This function creates a Sales Activity Data Table.
#'
#' Requires: DT, dplyr
#'
#' @param sales_activity The table to be used for the report.
#' @return sar_DT A Sales Activity Data Table.
#' @export
#' @examples
#'

PM_DT_Sales_Activity_Table <- function(sales_activity) {

    file_name <- paste("Poshmark-Sales-Activity-Download", as.character(Sys.Date()), sep = "-")

    dt_col_names <- c("Listing Date",  "Order Date",   "Days Listed", "Order ID",      "SKU",             "Department",    "Category",    "Sub-Category",    "Brand",
                      "Listing Title", "Color",        "Size",        "Bundled Order", "Offered Order",   "New With Tags", "Order Price", "Item Cost",       "Seller Ship Disc",
                      "Upg Ship Fee",  "Net Earnings", "Net Profit",  "Merchant Fee",  "Buyer User Name", "Buyer State",   "Buyer Zip",   "Sales Tax Buyer", "Sales Tax Seller",
                      "Notes",         "Other Info",   "Date Added to App")

    sales_activity <- sales_activity %>%
                      select(-user_id) %>%
                      arrange(desc(order_date), desc(listing_date), desc(order_id))

    sar_DT <- DT::datatable(sales_activity,
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
              DT::formatCurrency(c('order_price', 'item_cost', 'seller_ship_disc', 'upg_ship_fee', 'net_earnings', 'net_profit', 'merch_fee', 'sales_tax_buyer', 'sales_tax_seller'))

    ###

    return(sar_DT)

}

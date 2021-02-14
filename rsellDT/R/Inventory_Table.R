#' Creates the Inventory Detail Data Table
#'
#' This function creates a Inventory Detail Data Table.
#'
#' Requires: DT, data.table
#'
#' @param inv_table The inventory table to generate the report.
#' @param rem_flag The flag to indicate whether the Edit Button should be removed from output.
#' @return An Inventory Detail Data Table.
#' @export
#' @examples
#'

Inventory_Table <- function(inv_table, rem_flag = FALSE) {

    if(nrow(inv_table) == 0) { inv_DT <- NULL ; return(inv_DT) }

    #

    vals <- reactiveValues()

    button_def <- "<button id=\"XXXX\" type=\"button\" class=\"btn btn-default action-button\" style=\"color: #565455; background-color: #fbe7e9; border-color: #565455;\"
                   onclick=\"Shiny.onInputChange(&quot;lastClick&quot;,  this.id)\">View/Edit</button>"

    t_data <- data.table::data.table(inv_table %>%
                                     mutate(edit_btns = button_def) %>%
                                     select(39, 2, 5, 3, 4, 6:38))

    for (i in 1:nrow(t_data)) {

        t_data$edit_btns[i] <- gsub("XXXX", paste0("editbtn_", t_data$sku[i]), t_data$edit_btns[i])

    }

    if(rem_flag) { t_data$edit_btns <- "" }

    vals$Data <- t_data

    ###

    file_name <- paste("Inventory-Table", as.character(Sys.Date()), sep = "-")

    dt_col_names <- c("",
                      "SKU",           "Title",         "Status",      "Date Added",     "Description",    "Department",
                      "Category",      "Sub-Category",  "Brand",       "Color",          "Size",           "New With Tags",
                      "Item Source",   "Bin Number",    "Item Cost",   "Listing Price",  "List Poshmark",  "Poshmark Date",
                      "List Mercari",  "Mercari Date",  "List Ebay",   "Ebay Date",      "List Thredup",   "Thredup Date",
                      "List Tradesy",  "Tradesy Date",  "Sold Date",   "Sold Site",      "Bundled",        "Sale Price",
                      "Earnings",      "Profit",        "Other Info",  "Last Update",    "Last Action",    "Remove Date",
                      "Remove Reason")

    DT <- vals$Data

    inv_DT <- DT::datatable(DT,
                            escape = FALSE,
                            selection = 'single',
                            rownames = FALSE,
                            editable = FALSE,

                            filter = list(position = 'top', plain = TRUE, clear = TRUE),

                            colnames = dt_col_names,
                            extensions = c('Buttons', 'FixedColumns'),
                            options = list(dom = 'Blfrtip',

                                           columnDefs = list(

                                               list(
                                                   targets = c(5, 33),
                                                   render = DT::JS(
                                                       "function(data, type, row, meta) {",
                                                       "return type === 'display' && data.length > 30 ?",
                                                       "'<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;",
                                                       "}")
                                               ),

                                               list(
                                                   searchable = FALSE,
                                                   targets    = c(0)

                                               )

                                           ),

                                           initComplete = DT::JS("function(settings, json)
                                                             {","$(this.api().table().header()).css({'background-color': '#fbe7e9', 'color': '#404041'});","}"),
                                           scroller = TRUE,
                                           scrollX = TRUE,
                                           scrollY = 700,
                                           fixedColumns = list(leftColumns = 4),
                                           buttons = list(list(extend = 'copy'),
                                                          list(extend = 'csv',   filename = file_name),
                                                          list(extend = 'excel', filename = file_name),
                                                          list(extend = 'print')),
                                           lengthMenu = list(c(20, 40, 60, -1), c(20, 40, 60, "All"))),
                            class = "display nowrap") %>%
              DT::formatCurrency(c('item_cost', 'listing_price', 'sell_price', 'net_earnings', 'net_profit'))

    ###

    return(inv_DT)

}

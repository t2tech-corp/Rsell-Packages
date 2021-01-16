#' Creates a Daily Sales Summary Table
#'
#' This function creates a Daily Sales Summary Table from the Inventory Table.
#'
#' Requires: dplyr
#'
#' @param inv_table The Inventory Table for the user.
#' @return A Daily Sales Summary Table as a data frame.
#' @export
#' @examples
#'

Daily_Sales_Summary <- function(inv_table) {

    sell_status <- c("Sold", "Settled")

    date_seq <- data.frame(as.character(seq.Date(from = as.Date("2019-01-01"), to = Sys.Date(), by = 1)), stringsAsFactors = FALSE)

    colnames(date_seq) <- c("sold_date")

    sales_detail <- inv_table %>%
                    filter(item_status %in% sell_status) %>%
                    group_by(sold_date) %>%
                    summarize(tot_sold = n(),
                              tot_sell = sum(sell_price),
                              tot_earn = sum(net_earnings),
                              tot_cost = sum(item_cost),
                              tot_prof = sum(net_profit)) %>%
                    ungroup()

    sales_detail <- left_join(date_seq, sales_detail, by = "sold_date")

    sales_detail <- sales_detail %>% arrange(desc(as.Date(sold_date)))

    sales_detail[is.na(sales_detail)] <- 0

    ###

    return(sales_detail)

}

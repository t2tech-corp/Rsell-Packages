#' Creates a Year, Week Cash Flow In Table
#'
#' This function Creates a Year, Week Cash Flow In Table.
#' 
#' Requires: dplyr, lubridate
#'
#' @param inv_table The Inventory Table to use.
#' @return yw_cash_flow_in The Year, Week Cash Flow In Table as Data Frame.
#' @export
#'

Create_YW_Cash_Flow_In <- function(inv_table) {
    
    sell_status <- c("Sold", "Settled")
    
    #
    
    yw_cash_flow_in <- inv_table %>%
        
                       filter(item_status %in% sell_status) %>%
        
                       select(sku, sold_date, net_earnings, net_profit) %>%
        
                       mutate(sold_date = as.Date(sold_date)) %>%
        
                       arrange(sold_date) %>%
        
                       mutate(seq_year = lubridate::year(sold_date),
                              seq_week = lubridate::week(sold_date)) %>%
        
                       group_by(seq_year, seq_week) %>%
        
                       summarize(week_in = sum(net_earnings),
                                 week_np = sum(net_profit)) %>%
        
                       ungroup()
    
    #
    
    return(yw_cash_flow_in)
    
}
#' Creates a Year, Week Cash Flow Out Table
#'
#' This function Creates a Year, Week Cash Flow Out Table.
#' 
#' Requires: dplyr, lubridate
#'
#' @param inv_table The Inventory Table to use.
#' @return yw_cash_flow_out The Year, Week Cash Flow Out Table as Data Frame.
#' @export
#'

Create_YW_Cash_Flow_Out <- function(inv_table) {
    
    rem_status  <- c("Removed")
    
    #
    
    yw_cash_flow_out <- inv_table %>%
        
                        filter(!item_status %in% rem_status) %>%
        
                        select(sku, date_added_inv, item_cost) %>%
        
                        mutate(date_added_inv = as.Date(date_added_inv)) %>%
        
                        arrange(date_added_inv) %>%
        
                        mutate(seq_year = lubridate::year(date_added_inv),
                               seq_week = lubridate::week(date_added_inv)) %>%
        
                        group_by(seq_year, seq_week) %>%
        
                        summarize(week_out = sum(item_cost)) %>%
        
                        ungroup()
    
    #
    
    return(yw_cash_flow_out)
    
}
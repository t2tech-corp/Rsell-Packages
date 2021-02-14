#' Creates a Year, Month Inventory Sell Through Table
#'
#' This function creates a Year, Month Inventory Sell Through Table.
#' 
#' Requires: dplyr, lubridate
#'
#' @param ym_inv_summary The Inventory Summary Table to use.
#' @return ym_inv_add_summary The Year Month Inventory Sold Summary Table as Data Frame.
#' @export
#'

Create_YM_Inventory_Sell_Through <- function(ym_inv_summary) {
    
    ym_inv_sell_through <- ym_inv_summary %>%
        
                           mutate(m_net_inv = m_inv_month - m_sld_month) %>%
        
                           mutate(m_cum_inv = cumsum(m_net_inv)) %>%
        
                           mutate(m_bom_inv = lag(m_cum_inv, default = 0)) %>%
        
                           mutate(sell_thru = round(m_sld_month / m_bom_inv, 4) * 100) %>%
        
                           select(m_year, m_month, sell_thru)
    
    #
    
    ym_inv_sell_through[is.na(ym_inv_sell_through)] <- 0
    
    #
    
    return(ym_inv_sell_through)
    
}
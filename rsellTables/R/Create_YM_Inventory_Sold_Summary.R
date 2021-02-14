#' Creates a Year, Month Inventory Sold Summary Table
#'
#' This function creates a Year, Month Inventory Sold Summary from an Inventory Table.
#' 
#' Requires: dplyr, lubridate
#'
#' @param inv_table The Inventory Table to use.
#' @return ym_inv_add_summary The Year Month Inventory Sold Summary Table as Data Frame.
#' @export
#'

Create_YM_Inventory_Sold_Summary <- function(inv_table) {
    
    sell_status <- c("Sold", "Settled")
    
    #
    
    ym_inv_sold_summary <- inv_table %>%
        
                           filter(item_status %in% sell_status) %>%
        
                           mutate(m_year  = lubridate::year(sold_date),
                                  m_month = lubridate::month(sold_date)) %>%
        
                           group_by(m_year, m_month) %>%
        
                           summarize(m_sld_month = n()) %>%
        
                           ungroup()
    
    #
    
    return(ym_inv_sold_summary)
    
}
#' Creates a Year, Month Inventory Added Summary Table
#'
#' This function creates a Year, Month Inventory Added Summary from an Inventory Table.
#' 
#' Requires: dplyr, lubridate
#'
#' @param inv_table The Inventory Table to use.
#' @return ym_inv_add_summary The Year Month Inventory Added Summary Table as Data Frame.
#' @export
#'

Create_YM_Inventory_Added_Summary <- function(inv_table) {
    
    rem_status  <- c("Removed")
    
    #
    
    ym_inv_add_summary <- inv_table %>%
        
                          filter(!item_status %in% rem_status) %>%
        
                          mutate(m_year  = lubridate::year(date_added_inv),
                                 m_month = lubridate::month(date_added_inv)) %>%
        
                          group_by(m_year, m_month) %>%
        
                          summarize(m_inv_month = n()) %>%
        
                          ungroup()
    
    #
    
    return(ym_inv_add_summary)
    
}
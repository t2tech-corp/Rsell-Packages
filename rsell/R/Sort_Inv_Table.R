#' Sorts the Inventory Table
#'
#' This function Sorts the Inventory Table by Year, Month, Counter.
#'
#' @param inv_table Inventory Table to Sort
#' @return inv_table Sorted Inventory Table
#' @export
#'

Sort_Inv_Table <- function(inv_table) {
    
    inv_table <- inv_table %>%
                 mutate(t_year  = substr(sku, 3, 4),
                        t_month = substr(sku, 1, 2),
                        t_count = substr(sku, 6, 9)) %>%
                 arrange(desc(t_year), desc(t_month), desc(t_count)) %>%
                 select(-t_year, -t_month, -t_count)
    
    #
    
    return(inv_table)
    
}
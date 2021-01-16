#' Creates a Summary Table for Charting
#'
#' This function Creates a Summary Table for Charting.
#'
#' Requires: dplyr
#'
#' @param t_summary The summary table.
#' @param var_name The variable name for primary sorting.
#' @return A summary table as a data frame, top 10.
#' @export
#' @examples
#'

PM_Summary_For_Chart <- function(t_summary, var_name) {
    
    if(var_name == "avg_days_listed") {
        
        pm_summary_chart <- t_summary %>%
                            arrange(!!as.symbol(var_name), desc(avg_order_price), desc(avg_net_profit)) %>%
                            slice(1:10)
        
    } else {
        
        pm_summary_chart <- t_summary %>%
                            arrange(desc(!!as.symbol(var_name)), desc(avg_order_price), desc(avg_net_profit)) %>%
                            slice(1:10)
        
    }
    
    ##
    
    return(pm_summary_chart)
    
}

#' Creates a Poshmark Summary Table by Variable Name
#'
#' This function Creates a Poshmark Summary Table by Variable Name.
#'
#' @param pm_sales The Poshmark Sales Table for the user.
#' @param pm_var The variable name for grouping
#' @return A Poshmark Summary Table as a data frame.
#' @export
#' @examples
#'

PM_Table_Var_Summary <- function(pm_sales, pm_var) {

    pm_var_summary <- pm_sales %>%
                      filter(!!as.symbol(pm_var) != "") %>%
                      group_by(!!as.symbol(pm_var)) %>%
                      summarize(count = n(),
                                avg_days_listed  = round(mean(days_listed),  1),
                                tot_order_price  = sum(order_price),
                                avg_order_price  = round(mean(order_price),  2),
                                tot_net_earnings = sum(net_earnings),
                                avg_net_earnings = round(mean(net_earnings), 2),
                                tot_net_profit   = sum(net_profit),
                                avg_net_profit   = round(mean(net_profit),   2)) %>%
                      ungroup()

    ###

    return(pm_var_summary)

}

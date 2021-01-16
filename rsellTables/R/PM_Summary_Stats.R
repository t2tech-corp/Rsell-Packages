#' Creates Poshmark Summary Stats for File Upload
#'
#' This function Creates Poshmark Summary Stats for File Upload.
#'
#' Requires: dplyr
#'
#' @param sales_activity Poshmark Sales Activity table.
#' @return summary_stats Poshmark summary statistics.
#' @export
#' @examples
#'

PM_Summary_Stats <- function(sales_activity) {

    summary_stats <- sales_activity %>%
                     summarize(user_id   = first(user_id),
                               tot_items = n(),
                               min_date  = as.character(min(as.Date(order_date))),
                               max_date  = as.character(max(as.Date(order_date))),
                               tot_sale  = scales::dollar(sum(order_price)),
                               net_earn  = scales::dollar(sum(net_earnings)),
                               tot_bun   = sum(bundled_order == "Yes"),
                               tot_off   = sum(offered_order == "Yes"),
                               tot_nwt   = sum(new_with_tags == "Yes"))

    ###

    return(summary_stats)

}

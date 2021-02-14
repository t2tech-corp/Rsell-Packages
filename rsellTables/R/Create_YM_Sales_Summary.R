#' Creates a Year, Month Sales Summary Table
#'
#' This function creates a Year, Month Sales Summary from an Inventory Table.
#'
#' Requires: dplyr, lubridate
#'
#' @param inv_table The Inventory Table to use.
#' @return ym_sales_summary The Year Month Sales Summary Table as Data Frame.
#' @export
#'

Create_YM_Sales_Summary <- function(inv_table) {

    sell_status <- c("Sold", "Settled")

    #

    ym_sales_summary <- inv_table %>%

                        filter(item_status %in% sell_status) %>%

                        mutate(m_year  = lubridate::year(sold_date),
                               m_month = lubridate::month(sold_date)) %>%

                        group_by(m_year, m_month) %>%

                        summarize(m_item_cost    = sum(item_cost),
                                  m_sell_price   = sum(sell_price),
                                  m_net_earnings = sum(net_earnings),
                                  m_net_profit   = sum(net_profit)) %>%

                        ungroup()

    #

    return(ym_sales_summary)

}

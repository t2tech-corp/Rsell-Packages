#' Creates a Monthly Sales Summary Table
#'
#' This function creates a Monthly Sales Summary Table from the Inventory Table.
#'
#' Requires: dplyr, lubridate
#'
#' @param inv_table The Inventory Table for the user.
#' @return A Monthly Sales Summary Table as a data frame.
#' @export
#' @examples
#'

Monthly_Sales_Summary <- function(inv_table) {

    sell_status <- c("Sold", "Settled")

    sales_summary <- inv_table %>%
                     filter(item_status %in% sell_status) %>%
                     mutate(m_year  = lubridate::year(as.Date(sold_date)),
                            m_month = lubridate::month(as.Date(sold_date))) %>%
                     group_by(m_year, m_month) %>%
                     summarize(m_net_earnings = sum(net_earnings),
                               m_net_profit   = sum(net_profit)) %>%
                     ungroup()

    min_year <- min(sales_summary$m_year)
    max_year <- max(sales_summary$m_year)

    m_month <- rep(1:12, max_year - min_year + 1)
    m_year  <- sort(rep(min_year:max_year, 12))

    temp_table <- data.frame(m_year, m_month)

    sales_summary <- left_join(temp_table, sales_summary, by = c("m_year", "m_month"))

    sales_summary[is.na(sales_summary)] <- 0

    ###

    return(sales_summary)

}

#' Creates a Monthly Inventory Summary Table
#'
#' This function creates a Monthly Inventory Summary Table from the Inventory Table.
#'
#' Requires: dplyr, lubridate, stats
#'
#' @param inv_table The Inventory Table for the user.
#' @return A Monthly Inventory Summary Table as a data frame.
#' @export
#' @examples
#'

Monthly_Inventory_Summary <- function(inv_table) {

    rem_status  <- c("Removed")
    sell_status <- c("Sold", "Settled")

    ### Build Year Month Table

    min_year <- min(lubridate::year(inv_table$date_added_inv))
    max_year <- lubridate::year(Sys.Date())

    m_month <- rep(1:12, max_year - min_year + 1)
    m_year  <- sort(rep(min_year:max_year, 12))

    temp_table <- data.frame(m_year, m_month)

    ### Build Inventory Add by Year, Month

    m_inv_summ <- inv_table %>%
                  filter(!item_status %in% rem_status) %>%
                  mutate(m_year  = lubridate::year(as.Date(date_added_inv)),
                         m_month = lubridate::month(as.Date(date_added_inv))) %>%
                  group_by(m_year, m_month) %>%
                  summarize(m_inv_month = n()) %>%
                  ungroup()

    ### Build Inventory Sold by Year, Month

    m_inv_sold <- inv_table %>%
                  filter(item_status %in% sell_status) %>%
                  mutate(m_year  = lubridate::year(as.Date(sold_date)),
                         m_month = lubridate::month(as.Date(sold_date))) %>%
                  group_by(m_year, m_month) %>%
                  summarize(m_sld_month = n()) %>%
                  ungroup()

    ### Join Tables

    inv_summary <- left_join(temp_table,  m_inv_summ, by = c("m_year", "m_month"))
    inv_summary <- left_join(inv_summary, m_inv_sold, by = c("m_year", "m_month"))

    inv_summary[is.na(inv_summary)] <- 0

    ### Create Sell Through Stats

    inv_summary <- inv_summary %>%
                   mutate(m_net_inv = m_inv_month - m_sld_month) %>%
                   mutate(m_cum_inv = cumsum(m_net_inv)) %>%
                   mutate(m_bom_inv = lag(m_cum_inv, default = 0)) %>%
                   mutate(sell_thru = round(m_sld_month / m_bom_inv, 4) * 100) %>%
                   select(m_year, m_month, sell_thru)

    inv_summary[is.na(inv_summary)] <- 0

    ###

    return(inv_summary)

}

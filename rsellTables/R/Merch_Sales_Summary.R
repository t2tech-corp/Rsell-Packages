#' Creates the Merchant Sales Summary Table
#'
#' This function creates a Merchant Sales Summary Table from the Inventory Table.
#'
#' Requires: dplyr
#'
#' @param inv_table The Inventory Table for the user.
#' @return A Merchant Sales Summary Table as a data frame.
#' @export
#'

Merch_Sales_Summary <- function(inv_table) {

    m_inv_table <- inv_table %>% filter(item_status == "Sold" | item_status == "Settled")

    m_summary <- m_inv_table %>%
                 group_by(sold_site) %>%
                 summarize(sales_ct = n(),
                           sales_to = sum(sell_price),
                           sales_er = sum(net_earnings),
                           sales_pr = sum(net_profit)) %>%
                 ungroup()

    d_summary <- m_inv_table %>%
                 select(sold_site, sold_date, list_date_poshmark, list_date_mercari, list_date_ebay, list_date_thredup, list_date_tradesy)

    ###

    d_summary$list_date_ebay[d_summary$list_date_ebay == ""]         <- "2021-12-31"
    d_summary$list_date_mercari[d_summary$list_date_mercari == ""]   <- "2021-12-31"
    d_summary$list_date_poshmark[d_summary$list_date_poshmark == ""] <- "2021-12-31"
    d_summary$list_date_thredup[d_summary$list_date_thredup == ""]   <- "2021-12-31"
    d_summary$list_date_tradesy[d_summary$list_date_tradesy == ""]   <- "2021-12-31"

    ###

    d_summary <- d_summary %>%
                 mutate(days_list = case_when(sold_site == "Ebay"     ~ as.integer(difftime(sold_date, list_date_ebay,     units = "days")),
                                              sold_site == "Poshmark" ~ as.integer(difftime(sold_date, list_date_poshmark, units = "days")),
                                              sold_site == "Mercari"  ~ as.integer(difftime(sold_date, list_date_mercari,  units = "days")),
                                              sold_site == "Tradesy"  ~ as.integer(difftime(sold_date, list_date_tradesy,  units = "days")),
                                              sold_site == "Thredup"  ~ as.integer(difftime(sold_date, list_date_thredup,  units = "days"))
                 )) %>%
                 group_by(sold_site) %>%
                 summarize(days_list = round(mean(days_list), 2)) %>%
                 ungroup()

    m_summary <- left_join(m_summary, d_summary, by = "sold_site")

    ###

    return(m_summary)

}

#' Creates a Sold Inventory Summary Table
#'
#' This function creates a Sold Inventory Summary Table from the Inventory Table.
#'
#' Requires: dplyr
#'
#' @param inv_table The Inventory Table for the user.
#' @return A Sold Inventory Summary Table as a data frame.
#' @export
#' @examples
#'

Sold_Inventory_Summary <- function(inv_table) {

    s_inv_table <- inv_table %>% filter(item_status == "Sold" | item_status == "Settled")

    si_summary <- s_inv_table %>%
                  summarize(items_sold  = n(),
                            items_cost  = sum(item_cost),
                            items_list  = sum(listing_price),
                            items_sell  = sum(sell_price),
                            items_earn  = sum(net_earnings),
                            items_prof  = sum(net_profit)) %>%
                  mutate(items_avg  = round(items_cost / items_sold, 2),
                         items_avgl = round(items_list / items_sold, 2),
                         items_avgs = round(items_sell / items_sold, 2),
                         items_avge = round(items_earn / items_sold, 2),
                         items_avgp = round(items_prof / items_sold, 2)) %>%
                  mutate(items_mrg  = round(items_avgp / items_avg * 100, 2))


    si_adi <- s_inv_table %>%
              select(date_added_inv) %>%
              mutate(date_added_inv = as.Date(date_added_inv),
                     cur_date       = Sys.Date()) %>%
              mutate(days_inv = as.numeric(difftime(cur_date, date_added_inv, units = "days"))) %>%
              summarize(items_adi = round(mean(days_inv), 2))

    si_summary <- bind_cols(si_summary, si_adi)

    ###

    return(si_summary)

}

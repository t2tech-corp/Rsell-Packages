#' Creates a Total Inventory Summary Table
#'
#' This function creates a Total Inventory Summary Table from the Inventory Table.
#'
#' Requires: dplyr, lubridate
#'
#' @param inv_table The Inventory Table for the user.
#' @return A Total Inventory Summary Table as a data frame.
#' @export
#' @examples
#'

Total_Inventory_Summary <- function(inv_table) {

    t_inv_table <- inv_table %>% filter(item_status != "Removed")

    ti_summary <- t_inv_table %>%
                  summarize(items_purch = n(),
                            items_cost  = sum(item_cost)) %>%
                  mutate(items_avg = round(items_cost / items_purch, 2))

    ###

    ti_adi <- t_inv_table %>%
              select(date_added_inv) %>%
              mutate(date_added_inv = as.Date(date_added_inv),
                     cur_date       = Sys.Date()) %>%
              mutate(days_inv = as.numeric(difftime(cur_date, date_added_inv, units = "days"))) %>%
              summarize(items_adi = round(mean(days_inv), 2))

    ti_summary <- bind_cols(ti_summary, ti_adi)

    ###

    ati_table <- inv_table %>%
                 group_by(year(as.Date(date_added_inv)), month(as.Date(date_added_inv))) %>%
                 summarize(inv_count = n())

    colnames(ati_table) <- c("year", "month", "inv_count")

    ###

    sfi_table <- inv_table %>%
                 filter(item_status == "Sold" | item_status == "Settled") %>%
                 group_by(year(as.Date(date_added_inv)), lubridate::month(as.Date(date_added_inv))) %>%
                 summarize(sld_count = n())

    colnames(sfi_table) <- c("year", "month", "sld_count")

    ###

    ti_sell_thru <- left_join(ati_table, sfi_table, by = c("year", "month"))

    ti_sell_thru[is.na(ti_sell_thru)] <- 0

    ###

    ti_sell_thru <- ti_sell_thru %>%
                    mutate(net_inv = inv_count - sld_count) %>%
                    mutate(cum_inv = cumsum(net_inv)) %>%
                    mutate(bom_inv = lag(cum_inv, default = 0)) %>%
                    mutate(sell_thru = round(sld_count / bom_inv, 4) * 100) %>%
                    filter(is.finite(sell_thru)) %>%
                    filter(sell_thru > 0) %>%
                    summarize(items_st = round(mean(sell_thru), 2))

    ti_summary <- ti_summary %>% mutate(items_st = ti_sell_thru$items_st)

    ###

    return(ti_summary)

}

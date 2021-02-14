#' Creates a Total Inventory Summary Table
#'
#' This function creates a Total Inventory Summary Table from the Inventory Table.
#'
#' Requires: dplyr, lubridate
#'
#' @param inv_table The Inventory Table for the user.
#' @return A Total Inventory Summary Table as a data frame.
#' @export
#'

Inventory_Summary_Total <- function(inv_table) {

    ####  Filter Inventory Table  ----------------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    t_inv_table <- inv_table %>% filter(item_status != "Removed")

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Create Summary Table and Base Stats  ---------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    ti_summary <- t_inv_table %>%

                  summarize(items_purch = n(),
                            items_cost  = sum(item_cost)) %>%

                  mutate(items_avg = round(items_cost / items_purch, 2))

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Create Avg Days in Inventory Table  ----------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    adi_table <- t_inv_table %>%

                 select(date_added_inv, sold_date) %>%

                 mutate(date_added_inv = lubridate::ymd(date_added_inv),
                        sold_date      = lubridate::ymd(sold_date),
                        cur_date       = Sys.Date()) %>%

                 mutate(days_inv = case_when(is.na(sold_date) ~ as.numeric(difftime(cur_date, date_added_inv, units = "days")),
                                             TRUE ~ as.numeric(difftime(sold_date, date_added_inv, units = "days")))) %>%

                 summarize(items_adi = round(mean(days_inv), 2))

    ti_summary <- bind_cols(ti_summary, adi_table)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Create Monthly Added To Inventory  ------------------------------------------------------------------------------------ ####
    ##                                                                                                                              ##

    ati_table <- t_inv_table %>%

                 group_by(lubridate::year(date_added_inv), lubridate::month(date_added_inv)) %>%

                 summarize(inv_count = n()) %>%

                 ungroup()

    colnames(ati_table) <- c("year", "month", "inv_count")

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Create Monthly Sold From Inventory  ----------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    sfi_table <- t_inv_table %>%

                 filter(item_status == "Sold" | item_status == "Settled") %>%

                 group_by(lubridate::year(sold_date), lubridate::month(sold_date)) %>%

                 summarize(sld_count = n()) %>%

                 ungroup()

    colnames(sfi_table) <- c("year", "month", "sld_count")

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Create Monthly Sell Through Table  ------------------------------------------------------------------------------------ ####
    ##                                                                                                                              ##

    ti_sell_thru <- left_join(ati_table, sfi_table, by = c("year", "month"))

    ti_sell_thru[is.na(ti_sell_thru)] <- 0

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Create Monthly Sell Through Stats  ------------------------------------------------------------------------------------ ####
    ##                                                                                                                              ##

    ti_sell_thru <- ti_sell_thru %>%

                    mutate(net_inv = inv_count - sld_count) %>%
                    mutate(cum_inv = cumsum(net_inv)) %>%
                    mutate(bom_inv = lag(cum_inv, default = 0)) %>%
                    mutate(sell_thru = round(sld_count / bom_inv, 4) * 100) %>%

                    filter(is.finite(sell_thru)) %>%
                    filter(sell_thru > 0) %>%

                    summarize(items_st = round(mean(sell_thru), 2))

    ti_summary <- ti_summary %>% mutate(items_st = ti_sell_thru$items_st)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Format Summary Table  ------------------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    ti_summary <- ti_summary %>%
                  mutate(items_purch = scales::comma(items_purch),
                         items_cost  = scales::dollar(items_cost,    accuracy = 0.01),
                         items_avg   = scales::dollar(items_avg,     accuracy = 0.01),
                         items_adi   = scales::comma(items_adi,      accuracy = 0.01),
                         items_st    = scales::percent(items_st/100, accuracy = 0.01))

    #

    ti_summary <- Clean_Summary_Table(ti_summary)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Return Summary Table  ------------------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    return(ti_summary)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####

}

#' Creates a Sold Inventory Summary Table
#'
#' This function creates a Sold Inventory Summary Table from the Inventory Table.
#'
#' Requires: dplyr
#'
#' @param inv_table The Inventory Table for the user.
#' @return A Sold Inventory Summary Table as a data frame.
#' @export
#'

Inventory_Summary_Sold <- function(inv_table) {

    ####  Filter Inventory Table  ----------------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    s_inv_table <- inv_table %>% filter(item_status == "Sold" | item_status == "Settled")

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Create Summary Table and Base Stats  ---------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

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

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Create Avg Days in Inventory Table  ----------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    adi_table <- s_inv_table %>%

                 select(date_added_inv, sold_date) %>%

                 mutate(date_added_inv = lubridate::ymd(date_added_inv),
                        sold_date      = lubridate::ymd(sold_date)) %>%

                 mutate(days_inv = as.numeric(difftime(sold_date, date_added_inv, units = "days"))) %>%

                 mutate(days_inv = case_when(days_inv < 0 ~ 0,
                                             TRUE ~ days_inv)) %>%

                 summarize(items_adi = round(mean(days_inv), 2))

    si_summary <- bind_cols(si_summary, adi_table)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Format Summary Table  ------------------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    si_summary <- si_summary %>%
                  mutate(items_sold = scales::comma(items_sold),
                         items_cost = scales::dollar(items_cost,     accuracy = 0.01),
                         items_list = scales::dollar(items_list,     accuracy = 0.01),
                         items_sell = scales::dollar(items_sell,     accuracy = 0.01),
                         items_earn = scales::dollar(items_earn,     accuracy = 0.01),
                         items_prof = scales::dollar(items_prof,     accuracy = 0.01),
                         items_avg  = scales::dollar(items_avg,      accuracy = 0.01),
                         items_avgl = scales::dollar(items_avgl,     accuracy = 0.01),
                         items_avgs = scales::dollar(items_avgs,     accuracy = 0.01),
                         items_avge = scales::dollar(items_avge,     accuracy = 0.01),
                         items_avgp = scales::dollar(items_avgp,     accuracy = 0.01),
                         items_mrg  = scales::percent(items_mrg/100, accuracy = 0.01, big.mark = ","),
                         items_adi  = scales::comma(items_adi,       accuracy = 0.01))

    #

    si_summary <- Clean_Summary_Table(si_summary)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Return Summary Table  ------------------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    return(si_summary)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####

}

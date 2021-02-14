#' Creates a Listed Inventory Summary Table
#'
#' This function creates a Listed Inventory Summary Table from the Inventory Table.
#'
#' Requires: dplyr
#'
#' @param inv_table The Inventory Table for the user.
#' @return A Listed Inventory Summary Table as a data frame.
#' @export
#'

Inventory_Summary_Listed <- function(inv_table) {

    ####  Filter Inventory Table  ----------------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    l_inv_table <- inv_table %>% filter(item_status == "Listed")

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Create Summary Table and Base Stats  ---------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    li_summary <- l_inv_table %>%

                  summarize(items_purch = n(),
                            items_cost  = sum(item_cost),
                            items_list  = sum(listing_price)) %>%

                  mutate(items_avg  = round(items_cost / items_purch, 2),
                         items_avgl = round(items_list / items_purch, 2))

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Create Avg Days in Inventory Table  ----------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    adi_table <- l_inv_table %>%

                 select(date_added_inv) %>%

                 mutate(date_added_inv = lubridate::ymd(date_added_inv),
                        cur_date       = Sys.Date()) %>%

                 mutate(days_inv = as.numeric(difftime(cur_date, date_added_inv, units = "days"))) %>%

                 summarize(items_adi = round(mean(days_inv), 2))

    li_summary <- bind_cols(li_summary, adi_table)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Format Summary Table  ------------------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    li_summary <- li_summary %>%
                  mutate(items_purch = scales::comma(items_purch),
                         items_cost  = scales::dollar(items_cost, accuracy = 0.01),
                         items_list  = scales::dollar(items_list, accuracy = 0.01),
                         items_avg   = scales::dollar(items_avg,  accuracy = 0.01),
                         items_avgl  = scales::dollar(items_avgl, accuracy = 0.01),
                         items_adi   = scales::comma(items_adi,   accuracy = 0.01))

    #

    li_summary <- Clean_Summary_Table(li_summary)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Return Summary Table  ------------------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    return(li_summary)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####

}

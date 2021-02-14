#' Creates an Open Inventory Summary Table
#'
#' This function creates a Open Inventory Summary Table from the Inventory Table.
#'
#' Requires: dplyr
#'
#' @param inv_table The Inventory Table for the user.
#' @return An Open Inventory Summary Table as a data frame.
#' @export
#'

Inventory_Summary_Open <- function(inv_table) {

    ####  Filter Inventory Table  ----------------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    o_inv_table <- inv_table %>% filter(item_status == "Inventory" | item_status == "Draft")

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Create Summary Table and Base Stats  ---------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    oi_summary <- o_inv_table %>%

                  summarize(items_open  = n(),
                            items_cost  = sum(item_cost)) %>%

                  mutate(items_avg  = round(items_cost / items_open, 2))

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Create Avg Days in Inventory Table  ----------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    adi_table <- o_inv_table %>%

                 select(date_added_inv) %>%

                 mutate(date_added_inv = lubridate::ymd(date_added_inv),
                        cur_date       = Sys.Date()) %>%

                 mutate(days_inv = as.numeric(difftime(cur_date, date_added_inv, units = "days"))) %>%

                 summarize(items_adi = round(mean(days_inv), 2))

    oi_summary <- bind_cols(oi_summary, adi_table)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Format Summary Table  ------------------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    oi_summary <- oi_summary %>%
                  mutate(items_open = scales::comma(items_open),
                         items_cost = scales::dollar(items_cost, accuracy = 0.01),
                         items_avg  = scales::dollar(items_avg,  accuracy = 0.01),
                         items_adi  = scales::comma(items_adi,   accuracy = 0.01))

    #

    oi_summary <- Clean_Summary_Table(oi_summary)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Return Summary Table  ------------------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    return(oi_summary)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####

}

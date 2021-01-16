#' Creates a Source Inventory Summary Table
#'
#' This function creates a Source Inventory Summary Table from the Inventory Table.
#'
#' Requires: dplyr
#'
#' @param inv_table The Inventory Table for the user.
#' @return A Source Inventory Summary Table as a data frame.
#' @export
#' @examples
#'

Source_Inventory_Summary <- function(inv_table) {

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

    return(ti_summary)

}

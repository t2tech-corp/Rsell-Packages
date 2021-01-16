#' Creates an Open Inventory Summary Table
#'
#' This function creates a Open Inventory Summary Table from the Inventory Table.
#'
#' Requires: dplyr
#'
#' @param inv_table The Inventory Table for the user.
#' @return An Open Inventory Summary Table as a data frame.
#' @export
#' @examples
#'

Open_Inventory_Summary <- function(inv_table) {

    o_inv_table <- inv_table %>% filter(item_status == "Inventory")

    oi_summary <- o_inv_table %>%
                  summarize(items_open  = n(),
                            items_cost  = sum(item_cost)) %>%
                  mutate(items_avg  = round(items_cost / items_open, 2))

    ###

    oi_adi <- o_inv_table %>%
              select(date_added_inv) %>%
              mutate(date_added_inv = as.Date(date_added_inv),
                     cur_date       = Sys.Date()) %>%
              mutate(days_inv = as.numeric(difftime(cur_date, date_added_inv, units = "days"))) %>%
              summarize(items_adi = round(mean(days_inv), 2))

    oi_summary <- bind_cols(oi_summary, oi_adi)

    ###

    return(oi_summary)

}

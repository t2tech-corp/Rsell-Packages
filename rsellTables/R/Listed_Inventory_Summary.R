#' Creates a Listed Inventory Summary Table
#'
#' This function creates a Listed Inventory Summary Table from the Inventory Table.
#'
#' Requires: dplyr
#'
#' @param inv_table The Inventory Table for the user.
#' @return A Listed Inventory Summary Table as a data frame.
#' @export
#' @examples
#'

Listed_Inventory_Summary <- function(inv_table) {

    l_inv_table <- inv_table %>% filter(item_status == "Listed")

    li_summary <- l_inv_table %>%
                  summarize(items_purch = n(),
                            items_cost  = sum(item_cost),
                            items_list  = sum(listing_price)) %>%
                  mutate(items_avg  = round(items_cost / items_purch, 2),
                         items_avgl = round(items_list / items_purch, 2))

    li_adi <- l_inv_table %>%
              select(date_added_inv) %>%
              mutate(date_added_inv = as.Date(date_added_inv),
                     cur_date       = Sys.Date()) %>%
              mutate(days_inv = as.numeric(difftime(cur_date, date_added_inv, units = "days"))) %>%
              summarize(items_adi = round(mean(days_inv), 2))

    li_summary <- bind_cols(li_summary, li_adi)

    ###

    return(li_summary)

}

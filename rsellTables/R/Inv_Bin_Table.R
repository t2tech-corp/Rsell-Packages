#' Creates an Inventory Bin Table
#'
#' This function creates an Inventory Bin Table.
#'
#' Requires: dplyr
#'
#' @param inv_table The Inventory Table for the user.
#' @return A data frame with inventory items by bin location.
#' @export
#' @examples
#'

Inv_Bin_Table <- function(inv_table) {

    val_status <- c("Inventory", "Listed")

    b_inv_table <- inv_table %>%
                   filter(item_status %in% val_status) %>%
                   select(bin_nbr, sku, item_status, date_added_inv, item_title, brand, color, size, new_with_tags, item_source, item_cost) %>%
                   mutate(bin_nbr = case_when(bin_nbr == "" ~ "Unassigned",
                                              TRUE ~ bin_nbr)) %>%
                   arrange(bin_nbr, date_added_inv, sku)

    ###

    return(b_inv_table)

}

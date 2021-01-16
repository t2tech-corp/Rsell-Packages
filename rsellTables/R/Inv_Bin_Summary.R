#' Creates a Summary Inventory Bin Table
#'
#' This function creates a Summary Inventory Bin Table and formats it using the Formattable package. This function calls Inv_Bin_Table.
#'
#' Requires: dplyr, formattable
#'
#' @param inv_table The Inventory Table for the user.
#' @return A formatted table detailing inventory bin usage.
#' @export
#' @examples
#'

Inv_Bin_Summary <- function(b_inv_table) {

    b_inv_summary <- b_inv_table %>%
                     group_by(bin_nbr) %>%
                     summarize(total_inv = n(),
                               total_val = sum(item_cost)) %>%
                     ungroup() %>%
                     mutate(total_val = scales::dollar(total_val))

    tbl_names <- c("Bin Label", "Inventory Items", "Inventory Value")

    colnames(b_inv_summary) <- tbl_names

    ###

    pinkPearl = "#FBE7E9"
    lightGray = "#CCCBCE"

    b_inv_table <- formattable::formattable(b_inv_summary,
                                            align = c("c", "c", "r"),
                                            list(
                                                `Bin Label`       = color_tile(lightGray, lightGray),
                                                `Inventory Items` = color_tile(pinkPearl, pinkPearl),
                                                `Inventory Value` = color_tile(pinkPearl, pinkPearl)
                                            ))

    ###

    return(b_inv_table)

}

#' Creates a Monthly Inventory Sell Through Table
#'
#' This function creates a Monthly Inventory Sell Through Table from the Inventory Table.
#'
#' Requires: dplyr, lubridate
#'
#' @param inv_table The Inventory Table for the user.
#' @return A Monthly Inventory Summary Table as a data frame.
#' @export
#'

Monthly_Inventory_Sell_Through <- function(inv_table) {

    ####  Create Year, Month Inventory Added Summary ---------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    ym_inv_add_summary <- Create_YM_Inventory_Added_Summary(inv_table)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Create Year, Month Inventory Sold Summary ----------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    ym_inv_sold_summary <- Create_YM_Inventory_Sold_Summary(inv_table)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Create Base Year, Month Table ----------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    beg_year <- min(lubridate::year(inv_table$date_added_inv))
    end_year <- lubridate::year(Sys.Date())

    ym_table <- Create_YM_Table(beg_year, end_year)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Join to Complete Inventory Summary ------------------------------------------------------------------------------------ ####
    ##                                                                                                                              ##

    ym_inv_summary <- left_join(ym_table,       ym_inv_add_summary,  by = c("m_year", "m_month"))
    ym_inv_summary <- left_join(ym_inv_summary, ym_inv_sold_summary, by = c("m_year", "m_month"))

    ym_inv_summary[is.na(ym_inv_summary)] <- 0

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Create Sell Through Stats --------------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    ym_inv_sell_through <- Create_YM_Inventory_Sell_Through(ym_inv_summary)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    #### Return Sell Through Stats ---------------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    return(ym_inv_sell_through)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####

}

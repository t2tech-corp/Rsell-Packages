#' Creates a Monthly Sales Summary Table
#'
#' This function creates a Monthly Sales Summary Table from the Inventory Table.
#'
#' Requires: dplyr
#'
#' @param inv_table The Inventory Table for the user.
#' @return ym_sales_summary A Monthly Sales Summary Table as a data frame.
#' @export
#'

Monthly_Sales_Summary <- function(inv_table) {

    ####  Create Year, Month Sales Summary -------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    ym_sales_summary <- Create_YM_Sales_Summary(inv_table)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Create Base Year, Month Table ----------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    beg_year <- min(ym_sales_summary$m_year)
    end_year <- lubridate::year(Sys.Date())

    ym_table <- Create_YM_Table(beg_year, end_year)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Join to Complete Sales Summary ---------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    ym_sales_summary <- left_join(ym_table, ym_sales_summary, by = c("m_year", "m_month"))

    ym_sales_summary[is.na(ym_sales_summary)] <- 0

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Return Sales Summary -------------------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    return(ym_sales_summary)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####

}

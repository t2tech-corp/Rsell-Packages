#' Creates a Poshmark Monthly Activity Table
#'
#' This function creates a Poshmark Monthly Activity Table from the Inventory Table.
#'
#' Requires: dplyr, lubridate
#'
#' @param pm_sales The Poshmark Sales Table for the user.
#' @return A Poshmark Monthly Activity Table as a data frame.
#' @export
#' @examples
#'

PM_Monthly_Activity <- function(pm_sales, flt_year, flt_month) {

    monthly_activity <- pm_sales %>%
                        mutate(year        = as.character(lubridate::year(order_date)),
                               month_nbr   = lubridate::month(order_date)) %>%
                        mutate(month_name  = as.character(lubridate::month(month_nbr, label = TRUE, abbr = FALSE))) %>%
                        filter(year       == flt_year,
                               month_name == flt_month) %>%
                        select(-year, -month_nbr, -month_name)

    ###

    return(monthly_activity)

}

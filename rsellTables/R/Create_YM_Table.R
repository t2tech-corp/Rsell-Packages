#' Creates a Year, Month Table
#'
#' This function creates a Year, Month table based on a beginning and ending year.
#'
#' Requires: dplyr
#'
#' @param beg_year The starting year for the table.
#' @param end_year The ending year for the table.
#' @return ym_table The Year Month Table as Data Frame.
#' @export
#'

Create_YM_Table <- function(beg_year = 2019, end_year = 2021) {

    m_month <- rep(1:12, end_year - beg_year + 1)
    m_year  <- sort(rep(beg_year:end_year, 12))

    #

    ym_table <- data.frame(m_year, m_month)

    #

    return(ym_table)

}

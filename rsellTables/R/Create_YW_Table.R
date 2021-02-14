#' Creates a Year, Week Table
#'
#' This function creates a Year, Week table based on a beginning and ending year.
#'
#' Requires: dplyr
#'
#' @param beg_year The starting year for the table.
#' @param end_year The ending year for the table.
#' @return yw_table The Year Month Table as Data Frame.
#' @export
#'

Create_YW_Table <- function(beg_year = 2019, end_year = 2021) {

    year_list <- seq(beg_year, end_year, by = 1)

    seq_year <- sort(rep(beg_year:end_year, 53))
    seq_week <- rep(1:53, length(year_list))

    for(k in seq_along(year_list)) {

        t_seq_date <- seq.Date(from = as.Date(paste0(year_list[k], "-01-07")), to = as.Date(paste0(year_list[k], "-12-31")), by = "weeks")

        t_seq_date <- c(t_seq_date, as.Date(paste0(year_list[k], "-12-31")))

        #

        if(k == 1) { seq_date <- t_seq_date } else
                   { seq_date <- c(seq_date, t_seq_date) }

    }

    #

    yw_table <- data.frame(seq_year, seq_week, seq_date, stringsAsFactors = FALSE)

    #

    return(yw_table)

}

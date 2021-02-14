#' Clean Summary Tables for Dashboards
#'
#' This function Cleans Summary Tables for Dashboards.
#'
#' @param t_table The table to be cleaned.
#' @return t_table The cleaned table.
#' @export
#'

Clean_Summary_Table <- function(t_table) {

    for(k in 1:length(t_table)) {

        if(is.na(t_table[1,k]))       { t_table[1,k] <- "---" } else
        if(is.nan(t_table[1,k]))      { t_table[1,k] <- "---" } else
        if(is.infinite(t_table[1,k])) { t_table[1,k] <- "---" } else
        if(t_table[1,k] == "Inf")     { t_table[1,k] <- "---" }

    }

    return(t_table)

}

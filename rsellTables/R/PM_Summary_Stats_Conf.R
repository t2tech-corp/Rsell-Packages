#' Creates Poshmark Summary Stats for File Upload Confirmation
#'
#' This function Creates Poshmark Summary Stats for File Upload Confirmation.
#'
#' Requires: dplyr
#'
#' @param sales_activity Poshmark Sales Activity table.
#' @return summary_stats_con Poshmark summary statistics.
#' @export
#' @examples
#'

PM_Summary_Stats_Conf <- function(sales_activity) {

    summary_stats_con <- sales_activity %>%
                         summarize(user_id   = first(user_id),
                                   tot_items = n(),
                                   tot_dup   = sum(duplicate == TRUE),
                                   tot_add   = sum(duplicate == FALSE))

    ###

    return(summary_stats_con)

}

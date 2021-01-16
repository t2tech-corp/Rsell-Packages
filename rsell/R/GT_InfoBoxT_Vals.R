#' Create Goal Tracker InfoBoxT Values
#'
#' This function Creates Goal Tracker InfoBoxT Values.
#'
#' Requires:
#'
#' @param t_actual Actual values.
#' @param t_goal User Goal values.
#' @return Formatted array of values for InfoBoxT
#' @export
#' @examples
#'

GT_InfoBoxT_Vals <- function(t_actual, t_goal, t_type) {

    inv_values_p <- c("NaN%", "NA%", "0.00%")
    inv_values_d <- c("$NaN", "$NA")

    ##

    bp_pct    <- round(t_actual / t_goal * 100, 2)

    if(t_type == "Cur") { bp_actual <- paste0("$", format(t_actual, nsmall = 2, big.mark = ",")) } else
    if(t_type == "Pct") { bp_actual <- paste0(format(t_actual, nsmall = 2), "%")                 }

    bp_pct    <- paste0(bp_pct, "%")

    ##

    if(bp_actual %in% inv_values_d) { bp_actual <- "$0.00" }
    if(bp_actual %in% inv_values_p) { bp_actual <- "0%"    }
    if(bp_pct    %in% inv_values_p) { bp_pct    <- "0%"    }

    ###

    return(c(bp_actual, bp_pct))

}

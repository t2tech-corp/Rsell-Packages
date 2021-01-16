#' Create Goal Tracker BoxProfileT Values
#'
#' This function Creates Goal Tracker BoxProfileT Values.
#'
#' Requires:
#'
#' @param t_actual Actual values.
#' @param t_goal User Goal values.
#' @return Formatted array of values for BoxProfileT
#' @export
#' @examples
#'

GT_boxProfileT_Vals <- function(t_actual, t_goal, t_type) {

    inv_values_p <- c("NaN%", "NA%", "0.00%")
    inv_values_d <- c("$NaN", "$NA")

    ##

    bp_diff   <- t_actual - t_goal
    bp_pct    <- t_actual / t_goal * 100

    if(t_type == "Cur") {

        bp_goal   <- paste0("$", format(t_goal,   nsmall = 2, big.mark = ","))
        bp_actual <- paste0("$", format(t_actual, nsmall = 2, big.mark = ","))
        bp_diff   <- paste0("$", format(bp_diff,  nsmall = 2, big.mark = ","))

    } else

    if(t_type == "Pct") {

        bp_goal   <- paste0(format(t_goal,   nsmall = 2), "%")
        bp_actual <- paste0(format(t_actual, nsmall = 2), "%")
        bp_diff   <- paste0(format(bp_diff,  nsmall = 2), "%")

    }

    bp_pct    <- paste0(round(bp_pct, 2), "%")

    ##

    if(bp_actual %in% inv_values_d) { bp_actual <- "$0.00" }
    if(bp_actual %in% inv_values_p) { bp_actual <- "0%"    }
    if(bp_diff   %in% inv_values_d) { bp_diff   <- "$0.00" }
    if(bp_diff   %in% inv_values_p) { bp_diff   <- "0%"    }
    if(bp_pct    %in% inv_values_p) { bp_pct    <- "0%"    }

    ###

    return(c(bp_goal, bp_actual, bp_diff, bp_pct))

}

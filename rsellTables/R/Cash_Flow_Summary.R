#' Creates a Cash Flow Summary Table
#'
#' This function creates a Cash Flow Summary Table from the Inventory Table.
#'
#' Requires: dplyr, lubridate
#'
#' @param inv_table The Inventory Table for the user.
#' @return A cash flow summary table as a data frame.
#' @export
#' @examples
#'

Cash_Flow_Summary <- function(inv_table) {

    ####  Build Year/Week Table ------------------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    beg_year <- min(lubridate::year(inv_table$date_added_inv))
    end_year <- lubridate::year(Sys.Date())

    yw_table <- Create_YW_Table(beg_year, end_year)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Build Cash Flow Out --------------------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    yw_cash_flow_out <- Create_YW_Cash_Flow_Out(inv_table)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Build Cash Flow In ---------------------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    yw_cash_flow_in <- Create_YW_Cash_Flow_In(inv_table)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Create Cash Flow Joined Table ----------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    cf_table <- left_join(yw_table, yw_cash_flow_out, by = c("seq_year", "seq_week"))
    cf_table <- left_join(cf_table, yw_cash_flow_in,  by = c("seq_year", "seq_week"))

    cf_table[is.na(cf_table)] <- 0.00

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####



    ####  Combine Weeks 52 & 53 ------------------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    cf52_idx <- grep(52, cf_table$seq_week)
    cf53_idx <- grep(53, cf_table$seq_week)

    for (i in seq_along(cf52_idx)) {

        cf_table$week_out[cf52_idx[i]] <- cf_table$week_out[cf52_idx[i]] + cf_table$week_out[cf53_idx[i]]
        cf_table$week_in[cf52_idx[i]]  <- cf_table$week_in[cf52_idx[i]]  + cf_table$week_in[cf53_idx[i]]
        cf_table$week_np[cf52_idx[i]]  <- cf_table$week_np[cf52_idx[i]]  + cf_table$week_np[cf53_idx[i]]

    }

    cf_table <- cf_table[-c(cf53_idx),]

    row.names(cf_table) <- NULL

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ##  Create Cumulative Stats ------------------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    cf_table <- Create_YW_Cash_Flow_Cum(cf_table)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ##  Filter for 2020 and later ----------------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    cf_table <- cf_table %>% filter(seq_year >= 2020) %>% filter(sum_np > 0)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ##  Return Cash Flow Table -------------------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    return(cf_table)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####

}

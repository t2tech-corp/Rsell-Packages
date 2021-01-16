#' Create Clean Inventory Record
#'
#' This function creates a clean inventory record for add.
#'
#' Requires:
#'
#' @return c_inv_record The clean inventory record.
#' @export
#' @examples
#'

Load_Add_Inv_Rec <- function() {

    c_inv_record <<- inv_table[1,]
    c_inv_record[sapply(c_inv_record, is.character)] <- ""
    c_inv_record[sapply(c_inv_record, is.numeric)]   <- 0

    ###

    return(c_inv_record)

}

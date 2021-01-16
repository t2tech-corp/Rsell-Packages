#' Fixes Partial SKU in Poshmark Sales Activity Tables
#'
#' This function Fixes Partial SKUs in Poshmark Sales Activity Tables.
#'
#' Requires: stringr
#'
#' @param sales_activity Poshmark Sales Activity Table
#' @return sales_activity Updated Poshmark Sales Activity Table
#' @export
#' @examples
#'

Fix_Partial_SKU <- function(sales_activity) {

    for (i in 1:nrow(sales_activity)) {

        sku_len <- nchar(sales_activity$sku[i])

        if(sku_len > 0 & sku_len < 9) {

            sku_m <- stringr::str_split(sales_activity$sku[i], "-", simplify = TRUE)

            if(sku_len == 7) { sales_activity$sku[i] <- paste0(sku_m[1,1], "-00", sku_m[1,2]) }
            if(sku_len == 8) { sales_activity$sku[i] <- paste0(sku_m[1,1], "-0",  sku_m[1,2]) }

        }

    }

    ###

    return(sales_activity)

}

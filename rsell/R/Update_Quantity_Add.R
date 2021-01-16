#' Update the Inventory Items to be Added
#'
#' This function creates additional records for inventory add based on the item quantity.
#'
#' Requires:
#'
#' @param inv_record The Inventory Record to be added.
#' @param item_qty The number of items to be added.
#' @return inv_record The updated data frame with all inventory records to be added.
#' @export
#' @examples
#'

Update_Quantity_Add <- function(inv_record, item_qty) {

    if(item_qty == 1) { return(inv_record) }

    ###

    seq_sku <- seq.int(1, item_qty, by = 1)

    seq_sku <- formatC(seq_sku, digits = 3, flag = "0")

    seq_sku <- paste0(inv_record$sku, "-", seq_sku)

    ###

    for (i in 2:item_qty) {

        inv_record <- bind_rows(inv_record, inv_record[1,])

    }

    inv_record$sku <- seq_sku

    ###

    return(inv_record)

}

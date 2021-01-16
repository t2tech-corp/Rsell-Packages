#' Get Changed Data from Two Data Frames
#'
#' This function determines the changed data between two data frames.
#'
#' Requires:
#'
#' @param source_df The Source Data Frame.
#' @param target_df The Target Data Frame.
#' @return man_sku The next SKU for inventory items.
#' @export
#' @examples
#'

Get_Changed_Data <- function(source_df, target_df) {

    changed_idx <- rep(TRUE, length(source_df))

    for (i in 1:length(source_df)) {

        changed_idx[i] <- identical(source_df[1,i], target_df[1,i])

    }

    ###

    return(changed_idx)

}

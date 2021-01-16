#' Get Changed User Goals
#'
#' This function finds changed user goals.
#'
#' Requires:
#'
#' @param source_df Source data drame
#' @param target_df Target data frame
#' @return changed_tbl Changed table as data frame
#' @export
#' @examples
#'

Get_Changed_Goals <- function(source_df, target_df) {

    changed_tbl <- as.data.frame(matrix(TRUE, ncol = length(source_df), nrow = nrow(source_df)))

    ####

    for (i in 1:nrow(source_df)) {

        for (j in 1:length(source_df)) {

            changed_tbl[i,j] <- identical(source_df[i,j], target_df[i,j])

        }

    }

    ###

    return(changed_tbl)

}

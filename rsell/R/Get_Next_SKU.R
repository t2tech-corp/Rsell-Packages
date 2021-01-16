#' Get Next SKU for Inventory
#'
#' This function generates the next SKU for Inventory Table.
#'
#' Requires: dplyr
#'
#' @return man_sku The next SKU for inventory items.
#' @export
#' @examples
#'

Get_Next_SKU <- function() {

    sku_helper <- inv_table %>% select(sku)

    #### Create Current Manual SKU ####

    month_number <- formatC(as.numeric(month(Sys.Date())), width = 2, flag = "0")
    year_number  <- substr(as.character(year(Sys.Date())), 3, 4)

    man_sku <- paste0(month_number, year_number, "-", "0001")

    #### Empty Table Indicates New User with No Inventory ####

    if(nrow(sku_helper) == 0) { return(man_sku) }

    #### Filter SKU for Current Month and Year ####

    sku_helper$sku <- substr(sku_helper$sku, 1, 9)

    sku_helper <- sku_helper %>% filter(substr(sku, 1, 4) == substr(man_sku, 1, 4))

    #### Empty Table Indicates First Inventory Item of the Month ####

    if(nrow(sku_helper) == 0) { return(man_sku) }

    #### Set Next SKU Number for the Month and Year ####

    max_sku <- max(sku_helper$sku)

    sku_idx <- as.numeric(substr(max_sku, 6, 10)) + 1

    man_sku <- paste(substr(max_sku, 1, 5), formatC(sku_idx, width = 4, flag = "0"), sep = "")

    ###

    return(man_sku)

}

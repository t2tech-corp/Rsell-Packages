#' Sorts the Poshmark Sales Table
#'
#' This function Sorts the Poshmark Sales Table.
#'
#' @param inv_table Inventory Table to Sort
#' @return inv_table Sorted Inventory Table
#' @export
#'

PM_Sales_Table_Sort <- function(pm_sales) {

    ####  Adjustments for category and sub-category changes --------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    pm_sales <- pm_sales %>%
                mutate(category = case_when(department == "Women" & category == "Pants" ~ "Pants & Jumpsuits",
                                            TRUE ~ category))

    pm_sales <- pm_sales %>%
                mutate(sub_category = case_when(department == "Women" & category == "Jackets & Coats" & sub_category == "Blazers" ~ "Blazers & Suit Jackets",
                                                TRUE ~ sub_category))

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Arrange Table --------------------------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    pm_sales <- pm_sales %>% arrange(as.Date(order_date), as.Date(listing_date), order_id)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Return Table ---------------------------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    return(pm_sales)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####

}

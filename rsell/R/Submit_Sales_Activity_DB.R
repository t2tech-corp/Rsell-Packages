#' Submit Poshmark Sales Activity to Database
#'
#' This function Submits the Poshmark Sales Activity to Database.
#'
#' Requires: dplyr
#'
#' @param sales_activity Poshmark Sales Activity Report
#' @return sales_activity Poshmark Sales Activity Report
#' @export
#' @examples
#'

Submit_Sales_Activity_DB <- function(sales_activity) {

    #### Fetch Previous Sales ####

    prev_order_list <- pm_sales$order_id

    #### Process First Time Upload and Return ####

    if(length(prev_order_list) == 0) {

        #### Add to DB and pm_sales in Memory ####

        Write_Sales_Activity_User(sales_activity)

        sales_activity <- sales_activity %>% mutate(duplicate = FALSE)

        ###

        return(sales_activity)

    }

    #### Check for Duplicates, Filter, Process, Return ####

    if(length(prev_order_list) > 0) {

        f_sales_activity <- sales_activity %>% filter(!order_id %in% prev_order_list)

        #### Process Non Duplicates ####

        if(nrow(f_sales_activity) > 0) {

            Write_Sales_Activity_User(f_sales_activity)

        }

        #### Return Table with Duplicate Flag ####

        sales_activity <- sales_activity %>%
                          mutate(duplicate = case_when(order_id %in% prev_order_list ~ TRUE,
                                                       TRUE ~ FALSE))

        ###

        return(sales_activity)

    }

}

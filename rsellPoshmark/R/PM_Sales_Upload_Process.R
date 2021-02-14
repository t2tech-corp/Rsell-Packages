#' Process Poshmark Sales Activity File
#'
#' This function Processes the Poshmark Sales Activity File.
#'
#' Requires: utils, dplyr, lubridate
#'
#' @param t_datapath The file to be read
#' @return sales_activity The Poshmark Sales Activity table as data frame.
#' @export
#' @examples
#'

PM_Sales_Upload_Process <- function(t_datapath) {

    ####  Check for Beginning of Header ----------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    for (i in 1:100000) {

        start_line <- read.csv(t_datapath, stringsAsFactors = FALSE, nrows = 1, header = FALSE, skip = i-1, blank.lines.skip = FALSE, na.strings = "NA", encoding = "UTF-8")

        if(is.na(start_line$V1)) { next() } else if(start_line$V1 == "Listing Date") { break }

    }

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Check for End of Item Details ----------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    for (j in i:100000) {

        end_line <- read.csv(t_datapath, stringsAsFactors = FALSE, nrows = 1, header = FALSE, skip = j, blank.lines.skip = FALSE, na.strings = "NA", encoding = "UTF-8")

        if(is.na(end_line$V1)) { next() } else if(end_line$V1 == "Totals") { break }

    }

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Test for for Zero Sales Items ----------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    if(as.numeric(j) - as.numeric(i) == 1) { return(NULL) }

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Read Detail Records --------------------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    sales_activity <- read.csv(t_datapath, stringsAsFactors = FALSE, nrows = j-i-1, header = TRUE, skip = i-1, blank.lines.skip = FALSE, na.strings = "NA", encoding = "UTF-8")

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Create Friendly Variable Names ---------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    table_names <- c("listing_date", "order_date",      "sku",             "order_id",         "listing_title", "department",  "category",         "sub_category", "brand",        "color",
                     "size",         "bundled_order",   "offered_order",   "new_with_tags",    "item_cost",     "order_price", "seller_ship_disc", "upg_ship_fee", "net_earnings", "buyer_state",
                     "buyer_zip",    "buyer_user_name", "sales_tax_buyer", "sales_tax_seller", "notes",         "other_info")

    colnames(sales_activity) <- table_names

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Clean Sales Activity Data --------------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    sales_activity <- sales_activity %>% replace(., is.na(.), "")

    sales_activity <- sales_activity %>%
                      mutate(listing_date     = as.character(lubridate::mdy(listing_date)),
                             order_date       = as.character(lubridate::mdy(order_date)),
                             size             = as.character(size),
                             item_cost        = as.numeric(item_cost),
                             order_price      = as.numeric(gsub("\\$", "", order_price)),
                             seller_ship_disc = as.numeric(gsub("\\$", "", seller_ship_disc)),
                             upg_ship_fee     = as.numeric(gsub("\\$", "", upg_ship_fee)),
                             net_earnings     = as.numeric(gsub("\\$", "", net_earnings)),
                             sales_tax_buyer  = as.numeric(gsub("\\$", "", sales_tax_buyer)),
                             sales_tax_seller = as.numeric(gsub("\\$", "", sales_tax_seller)),
                             buyer_zip        = as.character(buyer_zip))

    sales_activity <- sales_activity %>% replace(., is.na(.), 0)

    sales_activity <- sales_activity %>%
                      mutate(days_listed = as.numeric(as.Date(order_date) - as.Date(listing_date)),
                             net_profit  = net_earnings - item_cost,
                             merch_fee   = order_price - seller_ship_disc - upg_ship_fee - net_earnings,
                             user_id     = session_user_record$user_id)

    sales_activity <- sales_activity %>%
                      mutate(color = gsub(",", " | ", color),
                             bundled_order = case_when(bundled_order == "Y" ~ "Yes",
                                                       bundled_order == "N" ~ "No",
                                                       TRUE ~ ""),
                             offered_order = case_when(offered_order == "Y" ~ "Yes",
                                                       offered_order == "N" ~ "No",
                                                       TRUE ~ ""),
                             new_with_tags = case_when(new_with_tags == "Y" ~ "Yes",
                                                       new_with_tags == "N" ~ "No",
                                                       TRUE ~ ""))

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Arrange Table Columns ------------------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    sales_activity <- sales_activity %>%
                      select(user_id,          listing_date,  order_date,      days_listed,      order_id,      sku,
                             department,       category,      sub_category,    brand,            listing_title, color,
                             size,             bundled_order, offered_order,   new_with_tags,    order_price,   item_cost,
                             seller_ship_disc, upg_ship_fee,  net_earnings,    net_profit,       merch_fee,     buyer_user_name,
                             buyer_state,      buyer_zip,     sales_tax_buyer, sales_tax_seller, notes,         other_info)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Correct SKUs ---------------------------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    sales_activity <- PM_Sales_Upload_Partial_SKU(sales_activity)
    sales_activity <- PM_Sales_Upload_Blank_SKU(sales_activity)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Get Cost Data and Update Profit --------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    sales_activity <- PM_Sales_Upload_Item_Cost(sales_activity)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Return Poshmark Sales Activity ---------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    return(sales_activity)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####

}

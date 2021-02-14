#' Creates a Poshmark Sales Monthly Table
#'
#' This function creates a Poshmark Sales Monthly Table from the Inventory Table.
#'
#' Requires: dplyr, scales, lubridate
#'
#' @param pm_sales The Poshmark Sales Table for the user.
#' @return A Poshmark Sales Monthly Table as a data frame.
#' @export
#' @examples
#'

PM_Table_Sales_Monthly <- function(pm_sales) {

    pm_sales_monthly <- pm_sales %>%

                        mutate(year        = as.character(year(order_date)),
                               month_nbr   = lubridate::month(order_date),
                               net_profit  = net_earnings - item_cost,
                               days_listed = as.Date(order_date) - as.Date(listing_date)) %>%

                        group_by(year, month_nbr) %>%

                        summarize(total_items_sold = n(),
                                  full_price       = sum(offered_order == "No"),
                                  buyer_offer      = sum(offered_order == "Yes" & seller_ship_disc == 0),
                                  seller_offer     = sum(offered_order == "Yes" & seller_ship_disc != 0),

                                  total_revenue    = scales::dollar(sum(order_price),      big.mark = ","),
                                  total_shipping   = scales::dollar(sum(seller_ship_disc), big.mark = ","),
                                  total_earnings   = scales::dollar(sum(net_earnings),     big.mark = ","),
                                  total_profit     = scales::dollar(sum(net_profit),       big.mark = ","),

                                  avg_sales_price  = scales::dollar(round(mean(order_price),  2), big.mark = ","),
                                  avg_net_earnings = scales::dollar(round(mean(net_earnings), 2), big.mark = ","),
                                  avg_profit       = scales::dollar(round(mean(net_profit),   2), big.mark = ","),
                                  total_merch      = scales::dollar(sum(merch_fee),               big.mark = ","),

                                  avg_days_listed  = round(mean(days_listed), 1),
                                  tot_orders       = length(unique(order_id)),
                                  tot_bundled      = length(unique(order_id[bundled_order == "Yes"])),
                                  tot_nwt          = sum(new_with_tags == "Yes"),
                                  unique_buyers    = length(unique(buyer_user_name)),
                                  repeat_customers = n() - length(unique(buyer_user_name))) %>%

                        mutate(full_pct   = paste(round(full_price    / total_items_sold * 100, 1), "%", sep = ""),
                               buyer_pct  = paste(round(buyer_offer   / total_items_sold * 100, 1), "%", sep = ""),
                               seller_pct = paste(round(seller_offer  / total_items_sold * 100, 1), "%", sep = ""),
                               bundle_pct = paste(round(tot_bundled   / tot_orders       * 100, 1), "%", sep = ""),
                               nwt_pct    = paste(round(tot_nwt       / total_items_sold * 100, 1), "%", sep = ""),
                               repeat_pct = paste(round(repeat_customers  / unique_buyers, 2), "%", sep = "")) %>%

                        ungroup() %>%

                        mutate(month_name = as.character(lubridate::month(month_nbr, label = TRUE, abbr = FALSE))) %>%

                        arrange(desc(year), desc(month_nbr))

    ###

    return(pm_sales_monthly)

}

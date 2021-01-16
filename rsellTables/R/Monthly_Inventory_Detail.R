#' Creates a Monthly Inventory Detail Table
#'
#' This function creates a Monthly Inventory Detail Table from the Inventory Table.
#'
#' Requires: dplyr, lubridate
#'
#' @param inv_table The Inventory Table for the user.
#' @return A Monthly Inventory Detail Table as a data frame.
#' @export
#' @examples
#'

Monthly_Inventory_Detail <- function(inv_table) {

    sell_stats <- c("Sold", "Settled")

    inv_table <- inv_table %>% filter(item_status != "Removed")

    min_date  <- min(as.Date(inv_table$date_added_inv))
    min_month <- lubridate::month(min_date)
    min_year  <- lubridate::year(min_date)

    beg_date  <- as.Date(paste0(min_year, "-", min_month, "-01"))
    seq_date  <- as.character(seq.Date(beg_date, Sys.Date(), by = 1))

    net_inv_table <- data.frame(seq_date, stringsAsFactors = FALSE)

    ###

    inv_add_sum <- inv_table %>%
                   group_by(date_added_inv) %>%
                   summarize(inv_add = n()) %>%
                   ungroup()

    inv_sld_sum <- inv_table %>%
                   group_by(sold_date) %>%
                   summarize(inv_sld = n()) %>%
                   ungroup()

    net_inv_table <- left_join(net_inv_table, inv_add_sum, by = c("seq_date" = "date_added_inv"))
    net_inv_table <- left_join(net_inv_table, inv_sld_sum, by = c("seq_date" = "sold_date"))

    net_inv_table[is.na(net_inv_table)] <- 0

    ###

    net_inv_table <- net_inv_table %>%
                     mutate(inv_add_cum = cumsum(inv_add),
                            inv_sld_cum = cumsum(inv_sld))

    net_inv_table <- net_inv_table %>% mutate(inv_eod = inv_add_cum - inv_sld_cum)

    net_inv_table <- net_inv_table %>% mutate(inv_start = lag(inv_eod, n = 1))

    net_inv_table[is.na(net_inv_table)] <- 0

    net_inv_table <- net_inv_table %>% select(seq_date, inv_start, inv_add, inv_sld, inv_eod)

    net_inv_table <- net_inv_table %>% arrange(desc(as.Date(seq_date)))

    ###

    return(net_inv_table)

}

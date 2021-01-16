#' Creates a Cash Flow Summary Table
#'
#' This function creates a Cash Flow Summary Table from the Inventory Table.
#'
#' Requires: dplyr, lubridate
#'
#' @param inv_table The Inventory Table for the user.
#' @return A cash flow summary table as a data frame.
#' @export
#' @examples
#'

Cash_Flow_Summary <- function(inv_table) {

    ### Build Year/Week Table

    beg_year <- min(lubridate::year(inv_table$date_added_inv))
    end_year <- lubridate::year(Sys.Date())

    seq_year <- seq(beg_year, end_year, by = 1)

    for(i in seq_along(seq_year)) {

        t_year <- rep(seq_year[i], 53)
        t_week <- seq(1, 53, by = 1)
        t_date <- c(seq(as.Date(paste0(seq_year[i], "-01-07")), as.Date(paste0(seq_year[i], "-12-31")), by = 7), as.Date(paste0(seq_year[i], "-12-31")))

        if(i == 1) {

            o_year <- t_year
            o_week <- t_week
            o_date <- t_date

        } else {

            o_year <- c(o_year, t_year)
            o_week <- c(o_week, t_week)
            o_date <- c(o_date, t_date)

        }

    }

    cf_table <- data.frame(o_year, o_week, o_date, stringsAsFactors = FALSE)

    ###

    rem_status  <- c("Removed")

    cf_inv_out   <- inv_table %>%
                    filter(!item_status %in% rem_status) %>%
                    select(sku, date_added_inv, item_cost) %>%
                    mutate(date_added_inv = as.Date(date_added_inv)) %>%
                    arrange(date_added_inv) %>%
                    mutate(o_year = lubridate::year(date_added_inv),
                           o_week = lubridate::week(date_added_inv)) %>%
                    group_by(o_year, o_week) %>%
                    summarize(week_out = sum(item_cost)) %>%
                    ungroup()

    cf_table <- left_join(cf_table, cf_inv_out, by = c("o_year", "o_week"))

    cf_table[is.na(cf_table)] <- 0.00

    ###

    sell_status <- c("Sold", "Settled")

    cf_inv_in    <- inv_table %>%
                    filter(item_status %in% sell_status) %>%
                    select(sku, sold_date, net_earnings, net_profit) %>%
                    mutate(sold_date = as.Date(sold_date)) %>%
                    arrange(sold_date) %>%
                    mutate(o_year = lubridate::year(sold_date),
                           o_week = lubridate::week(sold_date)) %>%
                    group_by(o_year, o_week) %>%
                    summarize(week_in = sum(net_earnings),
                              week_np = sum(net_profit)) %>%
                    ungroup()

    cf_table <- left_join(cf_table, cf_inv_in, by = c("o_year", "o_week"))

    cf_table[is.na(cf_table)] <- 0.00

    ###

    cf52_idx <- grep(52, cf_table$o_week)
    cf53_idx <- grep(53, cf_table$o_week)

    for (i in seq_along(cf52_idx)) {

        cf_table$week_out[cf52_idx[i]] <- cf_table$week_out[cf52_idx[i]] + cf_table$week_out[cf53_idx[i]]
        cf_table$week_in[cf52_idx[i]]  <- cf_table$week_in[cf52_idx[i]]  + cf_table$week_in[cf53_idx[i]]
        cf_table$week_np[cf52_idx[i]]  <- cf_table$week_np[cf52_idx[i]]  + cf_table$week_np[cf53_idx[i]]

    }

    cf_table <- cf_table[-c(cf53_idx),]

    row.names(cf_table) <- NULL

    ###

    cf_table <- cf_table %>%
                mutate(week_cf = week_in - week_out) %>%
                mutate(sum_np  = cumsum(week_np),
                       sum_out = cumsum(week_out),
                       sum_in  = cumsum(week_in)) %>%
                mutate(cf_pos  = sum_in - sum_out)

    ###

    cf_table <- cf_table %>%
                mutate(cf_pos  = case_when(o_week > lubridate::week(Sys.Date()) & o_year == lubridate::year(Sys.Date()) ~ 0,
                                           TRUE ~ cf_pos),
                       sum_np  = case_when(o_week > lubridate::week(Sys.Date()) & o_year == lubridate::year(Sys.Date()) ~ 0,
                                           TRUE ~ sum_np),
                       sum_out = case_when(o_week > lubridate::week(Sys.Date()) & o_year == lubridate::year(Sys.Date()) ~ 0,
                                           TRUE ~ sum_out),
                       sum_in  = case_when(o_week > lubridate::week(Sys.Date()) & o_year == lubridate::year(Sys.Date()) ~ 0,
                                           TRUE ~ sum_in))

    ###

    cf_table <- cf_table %>% filter(o_year >= 2020) %>% filter(sum_np > 0)

    ###

    return(cf_table)

}

#' Create Cumulative Stats for Cash Flow Table
#'
#' This function Creates Cumulative Stats for Cash Flow Table.
#' 
#' Requires: dplyr, lubridate
#'
#' @param cf_table The Year, Week Cash Flow table.
#' @return cf_table The Year, Week Cash Flow Table as Data Frame.
#' @export
#'

Create_YW_Cash_Flow_Cum <- function(cf_table) {
    
    cf_table <- cf_table %>%
        
                mutate(week_cf = week_in - week_out) %>%
        
                mutate(sum_np  = cumsum(week_np),
                       sum_out = cumsum(week_out),
                       sum_in  = cumsum(week_in)) %>%
        
                mutate(cf_pos  = sum_in - sum_out)
    
    #
    
    cf_table <- cf_table %>%
                mutate(cf_pos  = case_when(seq_week > lubridate::week(Sys.Date()) & seq_year == lubridate::year(Sys.Date()) ~ 0,
                                           TRUE ~ cf_pos),
                       sum_np  = case_when(seq_week > lubridate::week(Sys.Date()) & seq_year == lubridate::year(Sys.Date()) ~ 0,
                                           TRUE ~ sum_np),
                       sum_out = case_when(seq_week > lubridate::week(Sys.Date()) & seq_year == lubridate::year(Sys.Date()) ~ 0,
                                           TRUE ~ sum_out),
                       sum_in  = case_when(seq_week > lubridate::week(Sys.Date()) & seq_year == lubridate::year(Sys.Date()) ~ 0,
                                           TRUE ~ sum_in))
    
    #
    
    return(cf_table)
    
}
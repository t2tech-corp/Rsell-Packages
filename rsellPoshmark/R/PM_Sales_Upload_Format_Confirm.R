#' Format Poshmark Confirmation Panel
#'
#' This function formats the Poshmark Confirmation Panel.
#'
#' Requires: shiny
#'
#' @param sales_activity Poshmark Sales Activity Table
#' @return alert_text HTML Formatted Panel
#' @export
#' @examples
#'

PM_Sales_Upload_Format_Confirm <- function(sales_activity) {

    summary_stats     <- PM_Summary_Stats(sales_activity)
    summary_stats_con <- PM_Summary_Stats_Conf(sales_activity)

    ###

    alert_text <- wellPanel(style = "background: #fbe7e9; border-color: #565455",
                            tags$div(
                                style = "text-align: center;",
                                tags$img(src = logo_source, width = "200px", align = "center"),
                                tags$span(tags$b(tags$br(),
                                                 tags$h3("Sales Activity Report for: ", summary_stats$user_id),
                                                 tags$h4("For Order Dates: ", summary_stats$min_date, " thru ", summary_stats$max_date),
                                                 tags$hr(),
                                                 tags$h4("Total Order Amount:  ", summary_stats$tot_sale),
                                                 tags$h4("Total Net Earnings:  ", summary_stats$net_earn),
                                                 tags$hr(),
                                                 tags$h4("Total Items Sold:    ", summary_stats$tot_items),
                                                 tags$h5("Total Offered Items: ", summary_stats$tot_off),
                                                 tags$h5("Total Bundles Items: ", summary_stats$tot_bun),
                                                 tags$h5("Total New With Tags: ", summary_stats$tot_nwt),
                                                 tags$hr(),
                                                 tags$h5("Total Items To Be Loaded:  ", summary_stats_con$tot_items),
                                                 tags$h5("Duplicate Items Found:     ", summary_stats_con$tot_dup),
                                                 tags$h4("Total Items Loaded:        ", summary_stats_con$tot_add),
                                                 tags$hr()
                                )),
                                div(
                                    style = "text-align: center;",
                                    tags$b("Click Close and Add Another Report"),
                                    tags$br(),
                                    tags$br(),
                                    actionButton("pmclose", "Close", style = "color: white; background-color: #565455; border-color: #565455", width = '25%')
                                )

                            ))

    ###

    return(alert_text)

}

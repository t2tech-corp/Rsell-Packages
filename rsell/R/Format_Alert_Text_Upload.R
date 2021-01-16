#' Format Poshmark Upload Panel
#'
#' This function formats the Poshmark Upload Panel.
#'
#' Requires: shiny
#'
#' @param sales_activity Poshmark Sales Activity Table
#' @return alert_text HTML Formatted Panel
#' @export
#' @examples
#'

Format_Alert_Text_Upload <- function(sales_activity) {

    summary_stats <- PM_Summary_Stats(sales_activity)

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
                                                 tags$hr()
                                )),
                                tags$div(
                                    style = "text-align: center;",
                                    tags$div(tags$b("Click Submit to Load Data"), style = "display:inline-block; position:relative; right: 30px;"),
                                    tags$div(tags$b("Click Cancel to Skip File"), style = "display:inline-block; position:relative; left:  30px;")
                                ),
                                tags$br(),
                                tags$div(
                                    style = "text-align: center;",
                                    actionButton("pmsubmit", "Submit", style = "color: white; background-color: #565455; border-color: #565455;", width = '25%'),
                                    tags$div(style = "display:inline-block; width: 90px", HTML("<br>")),
                                    actionButton("pmcancel", "Cancel", style = "color: white; background-color: #d91e2f; border-color: #d91e2f;", width = '25%')

                                )

                            ))

    ###

    return(alert_text)

}

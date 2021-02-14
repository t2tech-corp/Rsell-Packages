#' Creates the User Goal Modal
#'
#' This function Creates the User Goal Modal.
#'
#' Requires: shiny, shinyjs, shinyWidgets
#'
#' @return Modal Dialog in App
#' @export
#' @examples
#'

User_Goal_Modal <- function() {

    if(is.null(user_goals)) { user_goals <<- DB_Read_Table_User(user_goals_name) }

    user_goals <- user_goals %>% filter(year == lubridate::year(Sys.Date()))

    ####

    modalDialog(

        title = tags$div(style = "text-align: center;",
                         strong(paste(lubridate::year(Sys.Date()),
                                      "| Monthly Goals For:",
                                      session_user_record$first_name,
                                      session_user_record$last_name,
                                      sep = " "))),

        fluidPage(useShinyjs(), useToastr(),

                  chooseSliderSkin("Square"),

                  fluidRow(

                      box(width = 2, solidHeader = FALSE, status = "primary", title = strong("January"),
                          numericInput("jan_net_earn", "Net Earnings",      value = user_goals$net_earnings[1], min = 0, max = 100000, step = 100),
                          numericInput("jan_net_prof", "Net Profit",        value = user_goals$net_profit[1],   min = 0, max = 100000, step = 100),
                          sliderInput("jan_sell_thru", "Sell Through Rate", value = user_goals$sell_through[1], min = 0, max = 100,    post = "%")
                      ),
                      box(width = 2, solidHeader = FALSE, status = "primary", title = strong("February"),
                          numericInput("feb_net_earn", "Net Earnings",      value = user_goals$net_earnings[2], min = 0, max = 100000, step = 100),
                          numericInput("feb_net_prof", "Net Profit",        value = user_goals$net_profit[2],   min = 0, max = 100000, step = 100),
                          sliderInput("feb_sell_thru", "Sell Through Rate", value = user_goals$sell_through[2], min = 0, max = 100,    post = "%")
                      ),
                      box(width = 2, solidHeader = FALSE, status = "primary", title = strong("March"),
                          numericInput("mar_net_earn", "Net Earnings",      value = user_goals$net_earnings[3], min = 0, max = 100000, step = 100),
                          numericInput("mar_net_prof", "Net Profit",        value = user_goals$net_profit[3],   min = 0, max = 100000, step = 100),
                          sliderInput("mar_sell_thru", "Sell Through Rate", value = user_goals$sell_through[3], min = 0, max = 100,    post = "%")
                      ),
                      box(width = 2, solidHeader = FALSE, status = "primary", title = strong("April"),
                          numericInput("apr_net_earn", "Net Earnings",      value = user_goals$net_earnings[4], min = 0, max = 100000, step = 100),
                          numericInput("apr_net_prof", "Net Profit",        value = user_goals$net_profit[4],   min = 0, max = 100000, step = 100),
                          sliderInput("apr_sell_thru", "Sell Through Rate", value = user_goals$sell_through[4], min = 0, max = 100,    post = "%")
                      ),
                      box(width = 2, solidHeader = FALSE, status = "primary", title = strong("May"),
                          numericInput("may_net_earn", "Net Earnings",      value = user_goals$net_earnings[5], min = 0, max = 100000, step = 100),
                          numericInput("may_net_prof", "Net Profit",        value = user_goals$net_profit[5],   min = 0, max = 100000, step = 100),
                          sliderInput("may_sell_thru", "Sell Through Rate", value = user_goals$sell_through[5], min = 0, max = 100,    post = "%")
                      ),
                      box(width = 2, solidHeader = FALSE, status = "primary", title = strong("June"),
                          numericInput("jun_net_earn", "Net Earnings",      value = user_goals$net_earnings[6], min = 0, max = 100000, step = 100),
                          numericInput("jun_net_prof", "Net Profit",        value = user_goals$net_profit[6],   min = 0, max = 100000, step = 100),
                          sliderInput("jun_sell_thru", "Sell Through Rate", value = user_goals$sell_through[6], min = 0, max = 100,    post = "%")
                      )

                  ),

                  fluidRow(

                      box(width = 2, solidHeader = FALSE, status = "primary", title = strong("July"),
                          numericInput("jul_net_earn", "Net Earnings",      value = user_goals$net_earnings[7], min = 0, max = 100000, step = 100),
                          numericInput("jul_net_prof", "Net Profit",        value = user_goals$net_profit[7],   min = 0, max = 100000, step = 100),
                          sliderInput("jul_sell_thru", "Sell Through Rate", value = user_goals$sell_through[7], min = 0, max = 100,    post = "%")
                      ),
                      box(width = 2, solidHeader = FALSE, status = "primary", title = strong("August"),
                          numericInput("aug_net_earn", "Net Earnings",      value = user_goals$net_earnings[8], min = 0, max = 100000, step = 100),
                          numericInput("aug_net_prof", "Net Profit",        value = user_goals$net_profit[8],   min = 0, max = 100000, step = 100),
                          sliderInput("aug_sell_thru", "Sell Through Rate", value = user_goals$sell_through[8], min = 0, max = 100,    post = "%")
                      ),
                      box(width = 2, solidHeader = FALSE, status = "primary", title = strong("September"),
                          numericInput("sep_net_earn", "Net Earnings",      value = user_goals$net_earnings[9], min = 0, max = 100000, step = 100),
                          numericInput("sep_net_prof", "Net Profit",        value = user_goals$net_profit[9],   min = 0, max = 100000, step = 100),
                          sliderInput("sep_sell_thru", "Sell Through Rate", value = user_goals$sell_through[9], min = 0, max = 100,    post = "%")
                      ),
                      box(width = 2, solidHeader = FALSE, status = "primary", title = strong("October"),
                          numericInput("oct_net_earn", "Net Earnings",      value = user_goals$net_earnings[10], min = 0, max = 100000, step = 100),
                          numericInput("oct_net_prof", "Net Profit",        value = user_goals$net_profit[10],   min = 0, max = 100000, step = 100),
                          sliderInput("oct_sell_thru", "Sell Through Rate", value = user_goals$sell_through[10], min = 0, max = 100,    post = "%")
                      ),
                      box(width = 2, solidHeader = FALSE, status = "primary", title = strong("November"),
                          numericInput("nov_net_earn", "Net Earnings",      value = user_goals$net_earnings[11], min = 0, max = 100000, step = 100),
                          numericInput("nov_net_prof", "Net Profit",        value = user_goals$net_profit[11],   min = 0, max = 100000, step = 100),
                          sliderInput("nov_sell_thru", "Sell Through Rate", value = user_goals$sell_through[11], min = 0, max = 100,    post = "%")
                      ),
                      box(width = 2, solidHeader = FALSE, status = "primary", title = strong("December"),
                          numericInput("dec_net_earn", "Net Earnings",      value = user_goals$net_earnings[12], min = 0, max = 100000, step = 100),
                          numericInput("dec_net_prof", "Net Profit",        value = user_goals$net_profit[12],   min = 0, max = 100000, step = 100),
                          sliderInput("dec_sell_thru", "Sell Through Rate", value = user_goals$sell_through[12], min = 0, max = 100,    post = "%")
                      )

                  ),

                  tags$hr(),

                  tags$div(
                      style = "text-align: center;",
                      tags$br(),
                      actionButton("glsubmit", "Update Goals", style = "color: white; background-color: #565455; border-color: #565455;", width = '15%'),
                      tags$div(style = "display:inline-block; width: 40px", HTML("<br>")),
                      actionButton("glcancel", "Exit",         style = "color: white; background-color: #d91e2f; border-color: #d91e2f;", width = '15%'),
                      tags$br()

                  )

        ),

        footer = NULL,
        size = "s"

    )

}

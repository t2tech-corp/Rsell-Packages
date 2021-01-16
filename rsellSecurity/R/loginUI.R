#' Login UI to Render Sign In Page
#'
#' This function is the Login UI to Render Sign In Page.
#'
#' Requires: shinyjs
#'
#' @param id Namespace ID
#' @return Sign in Page
#' @export
#' @examples
#'

loginUI <- function(id) {

    ns <- NS(id)

    div(id = ns("panel"), style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",

        tags$br(), tags$br(), tags$br(), tags$br(), tags$br(),
        tags$br(), tags$br(), tags$br(), tags$br(), tags$br(),

        wellPanel(style = "background: #fbe7e9",

                  div(
                      style = "text-align: center;",
                      tags$img(src = logo_source, width = "200px", align = "center")
                  ),

                  tags$hr(),

                  tags$h4(strong("Application Sign In"), class = "text-center", style = "padding-top: 0; color: #565455"),

                  textInput(ns("user_name"), tagList(icon("user"), "User Name")),

                  passwordInput(ns("password"), tagList(icon("unlock-alt"), "Password")),

                  div(
                      style = "text-align: center;",
                      tags$br(),
                      actionButton(ns("button"), "Sign In", style = "color: white; background-color: #4c4c4f; border-color: #4c4c4f", width = '100%')
                  ),

                  hidden(
                      div(id = ns("db_error"),
                          tags$br(),
                          tags$p("Security Database Unavailable | Contact Admin",
                                 style = "color: red; font-weight: bold; padding-top: 5px;", class = "text-center"))
                  ),

                  hidden(
                      div(id = ns("id_error"),
                          tags$br(),
                          tags$p("Invalid Username | Contact Admin",
                                 style = "color: red; font-weight: bold; padding-top: 5px;", class = "text-center"))
                  ),

                  hidden(
                      div(id = ns("pw_error"),
                          tags$br(),
                          tags$p("Invalid Password | Try Again",
                                 style = "color: red; font-weight: bold; padding-top: 5px;", class = "text-center"))
                  ),

                  hidden(
                      div(id = ns("au_error"),
                          tags$br(),
                          tags$p("User Not Authorized | Contact Admin",
                                 style = "color: red; font-weight: bold; padding-top: 5px;", class = "text-center"))
                  ),

                  hidden(
                      div(id = ns("ia_error"),
                          tags$br(),
                          tags$p("User Inactive | Contact Admin",
                                 style = "color: red; font-weight: bold; padding-top: 5px;", class = "text-center"))
                  )

        )

    )

}

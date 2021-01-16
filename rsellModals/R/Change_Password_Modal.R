#' Creates the Change Password Modal
#'
#' This function Creates the Change Password Modal.
#'
#' Requires: shiny
#'
#' @return Modal Dialog in App
#' @export
#' @examples
#'

Change_Password_Modal <- function(status = NULL) {

    modalDialog(

        title = tags$div(style = "text-align: center;",
                         strong(paste("Change Password For:",
                                      session_user_record$first_name,
                                      session_user_record$last_name,
                                      sep = " "))),

        tags$div(

            style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",

            tags$br(),

            wellPanel(style = "background: #fbe7e9",

                      tags$div(
                          style = "text-align: center;",
                          tags$img(src = logo_source, width = "200px", align = "center"),
                          tags$hr()
                      ),

                      tags$h4(strong("Change Password"), class = "text-center", style = "padding-top: 0;"),

                      textInput("c_user_id",     tagList(icon("user"),       "User ID"             ), session_user_record$user_id),
                      passwordInput("c_user_pw", tagList(icon("unlock-alt"), "New Password"        ), ""),
                      passwordInput("c_user_p2", tagList(icon("unlock-alt"), "Confirm New Password"), ""),

                      if(status != "conf")

                          tags$div(
                              style = "text-align: center;",
                              tags$br(),
                              actionButton("subchgpwd", "Change Password", style = "color: white; background-color: #4c4c4f; border-color: #4c4c4f", width = '100%'),
                              tags$br(),tags$br(),
                              actionButton("subchgcan", "Cancel",          style = "color: white; background-color: #d91e2f; border-color: #d91e2f", width = '100%')
                          ),

                      if(status == "conf")

                          tags$div(
                              style = "text-align: center;",
                              actionButton("subchgcl", "Exit", style = "color: white; background-color: #565455; border-color: #565455;", width = '100%')
                          ),

                      if(status == "miss")

                          tags$div(
                              tags$br(),
                              tags$p("New Password Not Entered | Password Not Changed",
                                     style = "color: red; font-weight: bold; padding-top: 5px;", class = "text-center")
                          ),

                      if(status == "invl")

                          tags$div(
                              tags$br(),
                              tags$p("Passwords Don't Match | Password Not Changed",
                                     style = "color: red; font-weight: bold; padding-top: 5px;", class = "text-center")
                          ),

                      if(status == "conf")

                          tags$div(
                              style = "text-align: center;",
                              tags$br(),
                              tags$p(tags$b("Password Updated | New Password Will Take Effect On Next Logon")),
                              tags$p(tags$b("Click EXIT to Close"))
                          )

            )

        ),

        footer = NULL,
        size = "l"

    )

}

#' Creates the User Profile Modal
#'
#' This function Creates the User Profile Modal.
#'
#' Requires: shiny
#'
#' @return Modal Dialog in App
#' @export
#'

User_Profile_Modal <- function() {

    fluidPage(useShinyjs(), useToastr(),

              modalDialog(

                  title = tags$div(style = "text-align: center;",
                                   strong(paste("Update User Profile | ",
                                                session_user_record$first_name,
                                                session_user_record$last_name,
                                                sep = " "))),

                  tags$div(
                      style = "text-align: center;",

                      tags$div(textInput("m_first_name", label = "First Name", value = session_user_record$first_name),
                               style = "display:inline-block; position:relative; right: 30px; text-align: left;"),
                      tags$div(textInput("m_last_name",  label = "Last Name",  value = session_user_record$last_name),
                               style = "display:inline-block; position:relative; left:  30px; text-align: left;"),

                      tags$br(),

                      tags$div(textInput("m_email", label = "Email",   value = session_user_record$e_mail),
                               style = "display:inline-block; position:relative; right: 30px; text-align: left;"),
                      tags$div(textInput("m_phone", label = "Phone #", value = session_user_record$phone),
                               style = "display:inline-block; position:relative; left:  30px; text-align: left;"),

                      tags$br(), tags$br(),

                      tags$div(textInput("m_street_1", label = "Street 1", value = session_user_record$street_1),
                               style = "display:inline-block; position:relative; right: 30px; text-align: left;"),
                      tags$div(textInput("m_street_2", label = "Street 2", value = session_user_record$street_2),
                               style = "display:inline-block; position:relative; left:  30px; text-align: left;"),

                      tags$br(),

                      tags$div(textInput("m_city",  label = "City",  value = session_user_record$city),
                               style = "display:inline-block; position:relative; right: 30px; text-align: left;"),
                      tags$div(textInput("m_state", label = "State", value = session_user_record$state),
                               style = "display:inline-block; position:relative; left:  30px; text-align: left;"),

                      tags$br(),

                      tags$div(textInput("m_zip",   label = "Zip",   value = session_user_record$zip),
                               style = "display:inline-block; text-align: left;"),

                  ),

                  tags$hr(),

                  tags$div(
                      style = "text-align: center;",
                      actionButton("prsubmit", "Update", style = "color: white; background-color: #565455; border-color: #565455;", width = '25%'),
                      tags$div(style = "display:inline-block; width: 40px", HTML("<br>")),
                      actionButton("prcancel", "Cancel", style = "color: white; background-color: #d91e2f; border-color: #d91e2f;", width = '25%')

                  ),

                  footer = NULL,
                  size = "l"

              )

    )

}

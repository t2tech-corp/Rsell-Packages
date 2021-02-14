#' Creates the User Table
#'
#' This function creates a User Table Data Table.
#'
#' Requires: DT, data.table
#'
#' @param user_table The inventory table to generate the report.
#' @return usr_DT A User Table Data Table.
#' @export
#'

DT_Admin_User_Table <- function(user_table) {

    dt_col_names <- c("User ID",  "First Name", "Last Name",     "Email",    "Street 1",    "Street 2",
                      "City",     "State",      "Zip",           "Phone",    "Status",      "Role",
                      "Proxy_ID", "Date Added", "Date Inactive", "PWD Hash", "Last Logon",  "Last Update", "Update Type")

    #

    usr_DT <- DT::datatable(user_table,
                            escape = FALSE,
                            rownames = FALSE,
                            editable = FALSE,
                            colnames = dt_col_names,
                            options = list(dom = 'Blfrtip',

                                           initComplete = DT::JS("function(settings, json)
                                                             {","$(this.api().table().header()).css({'background-color': '#fbe7e9', 'color': '#404041'});","}"),

                                           scroller = TRUE,
                                           scrollX  = TRUE,
                                           scrollY  = 700,

                                           lengthMenu = list(c(20, 40, 60, -1), c(20, 40, 60, "All"))),

                            class = "display nowrap")

    ###

    return(usr_DT)

}

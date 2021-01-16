#' Custom Dropdown Menu
#'
#' This function generates a custom Boxprofile Item. Built from shinydashboard dropdownMenu.
#'
#' Requires:
#'
#' @param type Type
#' @param profileName Profile Name
#' @return
#' @export
#' @examples
#'

dropdownMenuT <- function(..., type = c("notifications"), profileName = NULL, .list = NULL) {

    type <- match.arg(type)

    items <- c(list(...), .list)

    lapply(items, shinydashboard:::tagAssert, type = "li")

    dropdownClass <- paste0("dropdown ", type, "-menu")

    icon_1 <- icon("user",       "fa-fw")
    icon_2 <- icon("angle-down", "fa-fw")

    tags$li(class = dropdownClass,

            a(href = "#", class = "dropdown-toggle", `data-toggle` = "dropdown", style = "color: #4c4c4f;", icon_1, profileName, icon_2),
            tags$ul(class = "dropdown-menu",
                    tags$li(tags$ul(class = "menu", items))))

}

#' Custom Infobox
#'
#' This function generates a custom Infobox. Built from shinydashboard infoBox.
#'
#' Requires:
#'
#' @param title Title
#' @param value Value
#' @param subtitle Subtitle
#' @param caption Caption
#' @return
#' @export
#' @examples
#'

infoBoxT <- function (title, value = NULL, subtitle = NULL, caption = NULL, icon = shiny::icon("bar-chart"), color = "#ffffff", width = 4, href = NULL, fill = FALSE) {

    colorClass <- paste0("bg-", color)

    boxContent <- div(class = "info-box",
                      class = if (fill) colorClass,
                      span(class = "info-box-icon",
                           class = if (!fill) colorClass, icon),
                      div(class = "info-box-content",
                          span(class = "info-box-text", title),
                          if (!is.null(value)) span(class = "info-box-number", value),
                          if (!is.null(subtitle)) p(subtitle))
    )

    if (!is.null(href))
        boxContent <- a(href = href, boxContent)

    div(class = if (!is.null(width))
        paste0("col-sm-", width), boxContent)

}

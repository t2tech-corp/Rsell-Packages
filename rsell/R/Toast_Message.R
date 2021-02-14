#' Generate Toast Message
#'
#' This function generates a toast message.
#'
#' Requires: shinytoastr
#'
#' @param tr_message The message for the Toast output.
#' @param tr_type The type of Toast message to generate.
#' @return A Toast message at the bottom right of the page.
#' @export
#'

Toast_Message <- function(tr_message, tr_type) {

    if(tr_type == "Error")   { toastr_error(message = tr_message, position = "bottom-right", closeButton = TRUE, showMethod = "fadeIn", hideMethod = "fadeOut")   } else
    if(tr_type == "Info")    { toastr_info(message = tr_message, position = "bottom-right", closeButton = TRUE, showMethod = "fadeIn", hideMethod = "fadeOut")    } else
    if(tr_type == "Success") { toastr_success(message = tr_message, position = "bottom-right", closeButton = TRUE, showMethod = "fadeIn", hideMethod = "fadeOut") } else
    if(tr_type == "Warning") { toastr_warning(message = tr_message, position = "bottom-right", closeButton = TRUE, showMethod = "fadeIn", hideMethod = "fadeOut") }

}

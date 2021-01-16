#' Check the Validity of the Input Poshmark Sales Activity File
#'
#' This function Checks the Validity of the Input Poshmark Sales Activity File.
#'
#' @param t_datapath The file to be read.
#' @return valid_report, valid_user Logical values in array.
#' @export
#' @examples
#'

Check_Valid_Report <- function(t_datapath) {

    line_1 <- read.csv(t_datapath, stringsAsFactors = FALSE, nrows = 1, header = FALSE, skip = 0, encoding = "UTF-8")

    valid_report <- grepl("Poshmark Sales Report", line_1$V1)

    ##

    pm_user_id <- gsub("Poshmark Sales Report for ", "", line_1$V1)

    valid_user <- grepl(pm_user_id, session_user_record$user_id)

    ###

    return(c(valid_report, valid_user))

}

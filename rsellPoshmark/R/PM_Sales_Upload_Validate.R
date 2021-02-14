#' Check the Validity of the Input Poshmark Sales Activity File
#'
#' This function Checks the Validity of the Input Poshmark Sales Activity File.
#'
#' @param t_datapath The file to be read.
#' @return valid_report, valid_user Logical values in array.
#' @export
#' @examples
#'

PM_Sales_Upload_Validate <- function(t_datapath) {

    ####  Check for Valid Report ------------------------------------------------------------------------------------------------ ####
    ##                                                                                                                              ##

    line_1 <- read.csv(t_datapath, stringsAsFactors = FALSE, nrows = 1, header = FALSE, skip = 0, encoding = "UTF-8")

    valid_report <- grepl("Poshmark Sales Report", line_1$V1)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Check for Primary or Proxy User --------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    db_user_id <- User_Search_ID()

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Check for Valid User -------------------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    pm_user_id <- gsub("Poshmark Sales Report for ", "", line_1$V1)

    valid_user <- grepl(pm_user_id, db_user_id)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Return Validation Results --------------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    return(c(valid_report, valid_user))

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####

}

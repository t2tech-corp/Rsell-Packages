#' Queries Database for User Goals
#'
#' This function Queries Database for User Goals.
#'
#' Requires: RPostgres, dplyr
#'
#' @return User Goals Table as data frame.
#' @export
#' @examples
#'

Get_User_Goals <- function() {

    #### Refresh DB Connector if Needed ####

    if(!RPostgres::dbIsValid(db_con)) { db_con <- DB_Connect(db_name, db_host, db_user, db_pass) }

    #### Build Query ####

    my_query <- "SELECT * FROM user_goals WHERE user_id = 'USERID'"

    my_query <- gsub("USERID", session_user_record$user_id, my_query)

    my_query <- RPostgres::dbSendQuery(db_con, my_query)

    user_goals <- RPostgres::dbFetch(my_query)

    RPostgres::dbClearResult(my_query)

    #### Arrange by Year and Month ####

    user_goals <- user_goals %>% arrange(year, month)

    ###

    return(user_goals)

}

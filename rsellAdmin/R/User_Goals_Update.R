#' Updates User Goals in Database
#'
#' This function Updates User Goals in Database.
#'
#' Requires: dplyr
#'
#' @param u_user_goals The User Goal table with updates.
#' @param changed_tbl The changed fields tables for User Goals.
#' @export
#'

User_Goals_Update <- function(u_user_goals, changed_tbl) {

    ####  Set Table Names ------------------------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    table_names <- names(u_user_goals)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####

    #

    for (i in 1:nrow(u_user_goals)) {

        ####  Change Index by Row --------------------------------------------------------------------------------------------------- ####
        ##                                                                                                                              ##

        change_idx <- which(changed_tbl[i,] == FALSE)

        if(length(change_idx) == 0) { next() }

        ##                                                                                                                              ##
        #### ------------------------------------------------------------------------------------------------------------------------ ####


        ####  Format Query ---------------------------------------------------------------------------------------------------------- ####
        ##                                                                                                                              ##

        query_1 <- "UPDATE TABLENAME SET"
        query_2 <- "VARNAME = 'VARVALUE'"
        query_3 <- "WHERE user_id = 'USERID' AND year = 'YEAR' AND month = 'MONTH'"

        query_1 <- gsub("TABLENAME", user_goals_name,         query_1)
        query_3 <- gsub("USERID",    u_user_goals$user_id[i], query_3)
        query_3 <- gsub("YEAR",      u_user_goals$year[i],    query_3)
        query_3 <- gsub("MONTH",     u_user_goals$month[i],   query_3)

        for (j in 1:length(change_idx)) {

            t_query <- gsub("VARNAME",  table_names[change_idx[j]],     query_2)
            t_query <- gsub("VARVALUE", u_user_goals[i, change_idx[j]], t_query)

            if(j == 1) { m_query_2 <- t_query } else
                       { m_query_2 <- paste(m_query_2, t_query, sep = ", ") }

        }

        my_query <- paste(query_1, m_query_2, query_3, sep = " ")

        ##                                                                                                                              ##
        #### ------------------------------------------------------------------------------------------------------------------------ ####


        ####  Execute DB Update ----------------------------------------------------------------------------------------------------- ####
        ##                                                                                                                              ##

        DB_Execute_Command(my_query)

        ##                                                                                                                              ##
        #### ------------------------------------------------------------------------------------------------------------------------ ####

    }


}

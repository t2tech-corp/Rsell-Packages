#' Updates User Goals in Database
#'
#' This function Updates User Goals in Database.
#'
#' Requires: RPostgres, dplyr
#'
#' @param  u_user_goals
#' @return User Goals Table as data frame.
#' @export
#' @examples
#'

Update_User_Goals <- function(u_user_goals, changed_tbl, table_names) {

    table_names <- names(u_user_goals)

    #

    for (i in 1:nrow(u_user_goals)) {

        change_idx <- which(changed_tbl[i,] == FALSE)

        if(length(change_idx) == 0) { next() }

        query_1 <- "UPDATE user_goals SET"
        query_2 <- "VARNAME = 'VARVALUE'"
        query_3 <- "WHERE user_id = 'USERID' AND year = 'YEAR' AND month = 'MONTH'"

        query_3 <- gsub("USERID", u_user_goals$user_id[i], query_3)
        query_3 <- gsub("YEAR",   u_user_goals$year[i],    query_3)
        query_3 <- gsub("MONTH",  u_user_goals$month[i],   query_3)

        for (j in 1:length(change_idx)) {

            t_query <- gsub("VARNAME",  table_names[change_idx[j]],     query_2)
            t_query <- gsub("VARVALUE", u_user_goals[i, change_idx[j]], t_query)

            if(j == 1) { m_query_2 <- t_query } else
                       { m_query_2 <- paste(m_query_2, t_query, sep = ", ") }

        }

        my_query <- paste(query_1, m_query_2, query_3, sep = " ")

        #### Refresh DB Connector if Needed ####

        if(!RPostgres::dbIsValid(db_con)) { db_con <- DB_Connect(db_name, db_host, db_user, db_pass) }

        ##### Process Update #####

        RPostgres::dbBegin(db_con)

        db_return <- RPostgres::dbExecute(db_con, my_query)

        RPostgres::dbCommit(db_con)

    }

    #### Update User Goals in Memory ####

    for(k in 1:nrow(u_user_goals)) {

        k_idx <- which(user_goals$year == u_user_goals$year[k] & user_goals$month == u_user_goals$month[k])

        user_goals[k_idx,] <- u_user_goals[k,]

    }

    user_goals <<- user_goals

}

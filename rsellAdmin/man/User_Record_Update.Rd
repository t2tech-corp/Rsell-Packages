% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/User_Record_Update.R
\name{User_Record_Update}
\alias{User_Record_Update}
\title{Updates User Record in Database}
\usage{
User_Record_Update(e_user_record, changed_idx)
}
\arguments{
\item{e_user_record}{The user record to update in the database.}

\item{changed_idx}{The list of fields to update.}
}
\description{
This function Updates User Record in Database.
}
\details{
Requires: RPostgres, dplyr, shinytoastr
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PM_Sales_Upload_Submit.R
\name{PM_Sales_Upload_Submit}
\alias{PM_Sales_Upload_Submit}
\title{Submit Poshmark Sales Activity to Database}
\usage{
PM_Sales_Upload_Submit(sales_activity)
}
\arguments{
\item{sales_activity}{Poshmark Sales Activity Report}
}
\value{
sales_activity Poshmark Sales Activity Report with added Duplicate column
}
\description{
This function Submits the Poshmark Sales Activity to Database.
}
\details{
Requires: dplyr
}

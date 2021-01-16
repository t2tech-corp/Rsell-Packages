#' Read Category Table from AWS S3
#'
#' This function reads the Category Table from AWS S3.
#'
#' Requires: aws.s3
#'
#' @param s3Bucket The name of the S3 Bucket in AWS.
#' @return A character vector of Brands.
#' @export
#' @examples
#'

Read_AWS_Category <- function(s3Bucket) {

    temp_data <- aws.s3::s3readRDS("categories_table_n.rds", bucket = s3Bucket)

    return(temp_data)

}

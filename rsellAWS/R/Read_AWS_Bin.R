#' Read Bin Table from AWS S3
#'
#' This function reads the Bin Table from AWS S3.
#'
#' Requires: aws.s3
#'
#' @param s3Bucket The name of the S3 Bucket in AWS.
#' @return A character vector of Bin IDs.
#' @export
#' @examples
#'

Read_AWS_Bin <- function(s3Bucket) {

    temp_data <- aws.s3::s3readRDS("bin_table.rds", bucket = s3Bucket)

    return(temp_data)

}

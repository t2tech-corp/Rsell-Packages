#' Fixes Inventory Bin Numbers
#'
#' This function fixes inventory bin numbers from old format to new. May be removed when no longer needed.
#'
#' @param inv_table The Inventory Table for the user.
#' @return Updated inv_table as data frame.
#' @export
#' @examples
#'

Fix_Bin_Number <- function(inv_table) {

    inv_table <- inv_table %>%
                 mutate(bin_nbr = case_when(bin_nbr == "A-1" ~ "A-01",
                                            bin_nbr == "A-2" ~ "A-02",
                                            bin_nbr == "A-3" ~ "A-03",
                                            bin_nbr == "A-4" ~ "A-04",
                                            bin_nbr == "A-5" ~ "A-05",
                                            bin_nbr == "A-6" ~ "A-06",
                                            bin_nbr == "A-7" ~ "A-07",
                                            bin_nbr == "A-8" ~ "A-08",
                                            bin_nbr == "A-9" ~ "A-09",
                                            bin_nbr == "B-1" ~ "B-01",
                                            bin_nbr == "B-2" ~ "B-02",
                                            bin_nbr == "B-3" ~ "B-03",
                                            bin_nbr == "B-4" ~ "B-04",
                                            bin_nbr == "B-5" ~ "B-05",
                                            bin_nbr == "B-6" ~ "B-06",
                                            bin_nbr == "B-7" ~ "B-07",
                                            bin_nbr == "B-8" ~ "B-08",
                                            bin_nbr == "B-9" ~ "B-09",
                                            bin_nbr == "C-1" ~ "C-01",
                                            bin_nbr == "C-2" ~ "C-02",
                                            bin_nbr == "C-3" ~ "C-03",
                                            bin_nbr == "C-4" ~ "C-04",
                                            bin_nbr == "C-5" ~ "C-05",
                                            bin_nbr == "C-6" ~ "C-06",
                                            bin_nbr == "C-7" ~ "C-07",
                                            bin_nbr == "C-8" ~ "C-08",
                                            bin_nbr == "C-9" ~ "C-09",
                                            bin_nbr == "D-1" ~ "D-01",
                                            bin_nbr == "D-2" ~ "D-02",
                                            bin_nbr == "D-3" ~ "D-03",
                                            bin_nbr == "D-4" ~ "D-04",
                                            bin_nbr == "D-5" ~ "D-05",
                                            bin_nbr == "D-6" ~ "D-06",
                                            bin_nbr == "D-7" ~ "D-07",
                                            bin_nbr == "D-8" ~ "D-08",
                                            bin_nbr == "D-9" ~ "D-09",
                                            bin_nbr == "E-1" ~ "E-01",
                                            bin_nbr == "E-2" ~ "E-02",
                                            bin_nbr == "E-3" ~ "E-03",
                                            bin_nbr == "E-4" ~ "E-04",
                                            bin_nbr == "E-5" ~ "E-05",
                                            bin_nbr == "E-6" ~ "E-06",
                                            bin_nbr == "E-7" ~ "E-07",
                                            bin_nbr == "E-8" ~ "E-08",
                                            bin_nbr == "E-9" ~ "E-09",
                                            bin_nbr == ""    ~ "Unassigned",
                                            TRUE ~ bin_nbr))

    ###

    return(inv_table)

}

#' @title Function to implement START-A1 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining START-A1.
#'
#' START-A1 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item
#' }
#'
#' @param df Dataframe of patient information.
#'
#' \itemize{
#' \item Name of patient age column / variable must be "Age".
#' \item Names of comorbidities columns / variables must contain "Comorbidity_".
#' \item Other column / variable names must not contain "Comorbidity_".
#' \item Names of drugs columns / variables must contain "Drug_".
#' \item Other column / variable names must not contain "Drug_".
#' }
#'
#' @return List with two elements:
#'
#' \itemize{
#' \item `all_checks`: logical vector,
#'
#' `TRUE` if this STOPP/START criterion is satisfied, `FALSE` otherwise.
#' \item `instruction`: character vector,
#'
#' the STOPP/START criterion's name if `all_checks` is `TRUE`, blank otherwise.
#' }
#'
#' @export
start_a1 <- function(df) {

}


#' @title Function to implement START-A2 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining START-A2.
#'
#' START-A2 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item
#' }
#'
#' @inheritParams start_a1
#'
#' @inherit start_a1 return
#'
#' @export
start_a2 <- function(df) {

}


#' @title Function to implement START-A3 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining START-A3.
#'
#' START-A3 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item
#' }
#'
#' @inheritParams start_a1
#'
#' @inherit start_a1 return
#'
#' @export
start_a3 <- function(df) {

}


#' @title Function to implement START-A4 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining START-A4.
#'
#' START-A4 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item
#' }
#'
#' @inheritParams start_a1
#'
#' @inherit start_a1 return
#'
#' @export
start_a4 <- function(df) {

}


#' @title Function to implement START-A5 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining START-A5.
#'
#' START-A5 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item Age less than 85 years.
#' \item Any of the following comorbidities:
#'
#' I20, I21, I22, I24, I25,
#' I63, I64, I65, I66, I73.9,
#' I74, G45, Z95.1, Z95.5, or Z95.8.
#' \item None of the following drugs:
#'
#' C10AA.
#' }
#'
#' @inheritParams start_a1
#'
#' @inherit start_a1 return
#'
#' @export
start_a5 <- function(df) {

  # 'checks_list' is a list of logical vectors, each has one entry per patient.
  checks_list <- list()
  # 'codes_list' is a list of character vectors, each containing codes to check.
  codes_list  <- list()

  # 'check_list$extras1' is TRUE if the patient's age is less than 85 years.
  checks_list$extras1 <- df$Age < 85

  # 'codes_list$comorbs1' is a character vector of comorbidity codes to check.
  codes_list$comorbs1 <- c("I20", "I21",   "I22",   "I24",   "I25",
                           "I63", "I64",   "I65",   "I66", "I73.9",
                           "I74", "G45", "Z95.1", "Z95.5", "Z95.8")
  # 'checks_list$comorbs1' is TRUE if the patient has any listed comorbidities.
  checks_list$comorbs1 <- check_any_match(df,
                                          column_string = "Comorbidity_",
                                          codes = codes_list$comorbs1)

  # 'codes_list$drugs1' is a character vector of drug codes to check.
  codes_list$drugs1 <- c("C10AA")
  # 'checks_list$drugs1' is TRUE if the patient is not on any listed drugs.
  checks_list$drugs1 <- check_any_match(df,
                                        column_string = "Drug_",
                                        codes = codes_list$drugs1,
                                        no_matches = TRUE)

  output <- list()
  # 'output$all_checks' is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'checks_list'.
  output$all_checks <- Reduce(x = checks_list, f = "&")
  # 'output$instruction' is a character vector with one entry per patient.
  # "START A5" if the patient is TRUE for 'output$all_checks', "" otherwise.
  output$instruction <- ifelse(output$all_checks, yes = "START A5", no = "")

  return(output)
}


#' @title Function to implement START-A6 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining START-A6.
#'
#' START-A6 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item
#' }
#'
#' @inheritParams start_a1
#'
#' @inherit start_a1 return
#'
#' @export
start_a6 <- function(df) {

}


#' @title Function to implement START-A7 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining START-A7.
#'
#' START-A7 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item
#' }
#'
#' @inheritParams start_a1
#'
#' @inherit start_a1 return
#'
#' @export
start_a7 <- function(df) {

}


#' @title Function to implement START-A8 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining START-A8.
#'
#' START-A8 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item
#' }
#'
#' @inheritParams start_a1
#'
#' @inherit start_a1 return
#'
#' @export
start_a8 <- function(df) {

}

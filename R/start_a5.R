#' @title Function to implement START-A5 rule.
#'
#' @description
#' Determine which patients triggered the conditions defining START-A5.
#'
#' \itemize{
#' \item Age less than 85 years.
#' \item Any of the following comorbidities: I20, I21, I22, I24, or I25.
#' \item None of the following drugs: C10AA.}
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
#' `all_checks`: logical vector,
#'               TRUE if START-A5 is triggered, FALSE otherwise.
#'
#' `instruction`: character vector,
#'                "START-A5" if START-A5 is triggered, "" otherwise.
#'
#' @export
#'
#' @examples start_a5(mock_patients)
start_a5 <- function(df) {

  # 'checks_list' is a list of logical vectors, each has one entry per patient.
  checks_list <- list()
  # 'codes_list' is a list of character vectors, each containing codes to check.
  codes_list  <- list()

  # 'extras1' is TRUE if the patient's age is less than 85 years.
  checks_list$extras1 <- df$Age < 85

  # 'codes$comorbs1' is a character vector of comorbidity codes to check.
  codes$comorbs1 <- c("I20", "I21",   "I22",   "I24",   "I25",
                      "I63", "I64",   "I65",   "I66", "I73.9",
                      "I74", "G45", "Z95.1", "Z95.5", "Z95.8")
  # 'checks_list$comorbs1' is TRUE if the patient has any of these comorbidities.
  checks_list$comorbs1 <- check_any_match(df,
                                          column_string = "Comorbidity_",
                                          codes = codes$comorbs1)

  # 'codes$drugs1' is a character vector of drug codes to check.
  codes$drugs1 <- c("C10AA")
  # 'checks_list$drugs1' is TRUE if the patient is not on the listed drug.
  checks_list$drugs1 <- !check_any_match(df,
                                         column_string = "Drug_",
                                         codes = codes$drugs1
                                         )

  output <- list()
  # 'all_checks' is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'checks_list'.
  output$all_checks <- Reduce(x = checks_list, f = "&")
  # 'instruction' is a character vector with one entry per patient.
  # "START A5" if the patient is TRUE for 'bool', "" (blank) otherwise.
  output$instruction <- ifelse(output$all_checks, yes = "START A5", no = "")

  return(output)
}

#' Function to implement START-A5 rule.
#'
#' @param df Dataframe of patient information.
#'
#' @return List with two elements:
#'
#' `all_checks`: logical vector,
#'               TRUE if a patient has triggered START-A5, FALSE otherwise.
#'
#' `instruction`: character vector,
#'                "START-A5" if a patient has triggered START-A5, "" otherwise.
#'
#' @export
#'
#' @examples start_a5(mock_patients)
start_a5 <- function(df) {

  # 'checks_list' is a list of logical vectors, each has one entry per patient.
  checks_list <- list()

  # 'extras1' is TRUE if the patient's age is less than 85 years.
  checks_list$extras1 <- df$Age < 85

  # 'comorbs1' is TRUE if the patient has any of the listed comorbidities.
  checks_list$comorbs1 <- check_any_match(df,
                                          column_string = "Comorbidity_",
                                          code_set = "I20|I21|I22|I24|I25")

  # 'drugs1' is TRUE if the patient is not on the listed drug.
  checks_list$drugs1 <- !check_any_match(df,
                                         column_string = "Drug_",
                                         code_set = "C10AA")

  output <- list()
  # 'all_checks' is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'checks_list'.
  output$all_checks <- Reduce(x = checks_list, f = "&")
  # 'instruction' is a character vector with one entry per patient.
  # "START A5" if the patient is TRUE for 'bool', "" (blank) otherwise.
  output$instruction <- ifelse(output$all_checks, yes = "START A5", no = "")

  return(output)
}

#' @title Function to implement START-A1 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining START-A1.
#'
#' START-A1 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item Any of the following comorbidities:
#'
#' I48.2.
#' \item None of the following drugs:
#'
#' B01AA, B01AE, or B01AF.
#' }
#'
#' @param df Dataframe of patient information.
#'
#' \itemize{
#' \item Patient age column / variable must be named "Age".
#' \item Systolic BP column / variable must be named "Systolic_BP".
#' \item Diastolic BP column / variable must be named "Diastolic_BP".
#' \item Names of comorbidities columns / variables must contain "Comorbidity_".
#' \item Other column / variable names must not contain "Comorbidity_".
#' \item Names of drugs columns / variables must contain "Drug_".
#' \item Other column / variable names must not contain "Drug_".
#' }
#'
#' @return `output`: character vector,
#' \itemize{
#' \item "Not Relevant" if the conditions are not satisfied.
#' \item "Appropriate" if the conditions are satisfied but the correct drug has
#' already been prescribed.
#' \item The name of the criterion if the conditions are satisfied and the correct
#' drug has not been prescribed.
#' }
#'
#' @export
start_a1 <- function(df) {

  # 'checks_list' is a list of logical vectors, each has one entry per patient.
  checks_list <- list()
  # 'codes_list' is a list of character vectors, each containing codes to check.
  codes_list <- list()

  # 'codes_list$comorbs1' is a character vector of comorbidity codes to check.
  codes_list$comorbs1 <- c("I48.2")
  # 'checks_list$comorbs1' is TRUE if the patient has any listed comorbidities.
  checks_list$comorbs1 <- check_matches(df,
                                        column_string = "Comorbidity_",
                                        codes = codes_list$comorbs1,
                                        match = "any")

  # 'codes_list$drugs1' is a character vector of drug codes to check.
  codes_list$drugs1 <- c("B01AA", "B01AE", "B01AF")
  # 'checks_list$drugs1' is TRUE if the patient is not on any listed drugs.
  checks_list$drugs1 <- check_matches(df,
                                      column_string = "Drug_",
                                      codes = codes_list$drugs1,
                                      match = "none")

  # 'all_checks' is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'checks_list'.
  all_checks <- Reduce(x = checks_list, f = "&")

  return(all_checks)
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
#' \item None of the following comorbidities:
#'
#' I48.
#' \item Any of the following comorbidities:
#'
#' I20, I21, I22, I24, I25,
#' I63, I64, I65, I66, I73.9,
#' I74, G45, Z95.1, Z95.5, or Z95.8.
#' \item None of the following drugs:
#'
#' B01AC.
#' }
#'
#' @inheritParams start_a1
#'
#' @inherit start_a1 return
#'
#' @export
start_a3 <- function(df) {

  # 'checks_list' is a list of logical vectors, each has one entry per patient.
  checks_list <- list()
  # 'codes_list' is a list of character vectors, each containing codes to check.
  codes_list <- list()

  # 'codes_list$comorbs1' is a character vector of comorbidity codes to check.
  codes_list$comorbs1 <- c("I48")
  # 'checks_list$comorbs1' is TRUE if the patient has any listed comorbidities.
  checks_list$comorbs1 <- check_matches(df,
                                        column_string = "Comorbidity_",
                                        codes = codes_list$comorbs1,
                                        match = "none")

  # 'codes_list$comorbs2' is a character vector of comorbidity codes to check.
  codes_list$comorbs2 <- c("I20", "I21",   "I22",   "I24",   "I25",
                           "I63", "I64",   "I65",   "I66", "I73.9",
                           "I74", "G45", "Z95.1", "Z95.5", "Z95.8")
  # 'checks_list$comorbs2' is TRUE if the patient has any listed comorbidities.
  checks_list$comorbs2 <- check_matches(df,
                                        column_string = "Comorbidity_",
                                        codes = codes_list$comorbs2,
                                        match = "any")

  # 'codes_list$drugs1' is a character vector of drug codes to check.
  codes_list$drugs1 <- c("B01AC")
  # 'checks_list$drugs1' is TRUE if the patient is not on any listed drugs.
  checks_list$drugs1 <- check_matches(df,
                                      column_string = "Drug_",
                                      codes = codes_list$drugs1,
                                      match = "none")

  # 'all_checks' is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'checks_list'.
  all_checks <- Reduce(x = checks_list, f = "&")

  return(all_checks)
}


#' @title Function to implement START-A4 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining START-A4.
#'
#' START-A4 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item None of the following drugs:
#'
#' C07, C08, C09, C03A, C03EA.
#' \item Any of the following subconditions:
#' \itemize{
#' \item Systolic_BP > 160
#' \item Diastolic_BP >90
#' \item Systolic_BP > 140 and any of the following comorbidities:
#'
#' E10, E11, E12, E13, E14
#' }
#' }
#'
#' @inheritParams start_a1
#'
#' @inherit start_a1 return
#'
#' @export
start_a4 <- function(df) {

  # 'checks_list' is a list of logical vectors, each has one entry per patient.
  checks_list <- list()
  # 'codes_list' is a list of character vectors, each containing codes to check.
  codes_list <- list()

  # 'codes_list$comorbs1' is a character vector of comorbidity codes to check.
  codes_list$comorbs1 <- c("E10", "E11", "E12", "E13", "E14")
  # 'checks_list$multis1' is TRUE if the patient has diastolic BP over 90, or
  # systolic BP over 160, or both systolic BP over 140 and the listed
  # comorbidities.
  checks_list$multis1 <- (df$Diastolic > 90 |
                            df$Systolic > 160 |
                            (df$Systolic > 140 &
                               check_matches(df,
                                             column_string = "Comorbidity_",
                                             codes = codes_list$comorbs1,
                                             match = "any"))
                          )

  # 'codes_list$drugs1' is a character vector of drug codes to check.
  codes_list$drugs1 <- c("C07", "C08", "C09", "C03A", "C03EA")
  # 'checks_list$drugs1' is TRUE if the patient is not on any listed drugs.
  checks_list$drugs1 <- check_matches(df,
                                      column_string = "Drug_",
                                      codes = codes_list$drugs1,
                                      match = "none")

  # 'all_checks' is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'checks_list'.
  all_checks <- Reduce(x = checks_list, f = "&")

  return(all_checks)
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

  # prelim_checks is a list of logical vectors, each has one entry per patient.
  prelim_checks <- list()
  # prelim_codes is a list of character vectors, each containing codes to check.
  prelim_codes <- list()

  # prelim_checks$extras1 is TRUE if the patient's age is less than 85 years.
  prelim_checks$extras1 <- df$Age < 85

  # prelim_codes$comorbs1 is a character vector of comorbidity codes to check.
  prelim_codes$comorbs1 <- c("I20", "I21",   "I22",   "I24",   "I25",
                             "I63", "I64",   "I65",   "I66", "I73.9",
                             "I74", "G45", "Z95.1", "Z95.5", "Z95.8")
  # prelim_checks$comorbs1 is TRUE if the patient has any listed comorbidities.
  prelim_checks$comorbs1 <- check_matches(df,
                                          column_string = "Comorbidity_",
                                          codes = prelim_codes$comorbs1,
                                          match = "any")

  # all_prelims is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'prelim_checks'.
  all_prelims <- Reduce(x = prelim_checks, f = "&")


   # action_checks is a list of logical vectors, each has one entry per patient.
  action_checks <- list()
  # action_codes is a list of character vectors, each containing codes to check.
  action_codes <- list()

  # prelim_codes$drugs1 is a character vector of drug codes to check.
  action_codes$drugs1 <- c("C10AA")
  # prelim_checks$drugs1 is TRUE if the patient is on any listed drugs.
  action_checks$drugs1 <- check_matches(df,
                                        column_string = "Drug_",
                                        codes = action_codes$drugs1,
                                        match = "any")

  # all_actions is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'action_checks'.
  all_actions <- Reduce(x = action_checks, f = "&")


  output <- ifelse(all_prelims,
                   ifelse(all_actions, "Appropriate", "START-A5"),
                   "Not Relevant")

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

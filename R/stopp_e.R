#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Function to implement STOPP-E1 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining STOPP-E1.
#'
#' STOPP-E1 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item GFR less than:
#'
#' 30
#' \item Any of the following drugs:
#'
#' C01AA05
#' \item Digoxin Dose >125mcg:
#'
#' Yes
#' }
#'
#' @param df Dataframe of patient information.
#' @param gfr_column The name of the patient Na column as a character string.
#' @param digoxin_column The name of the patient Digoxin column as a
#' character string.
#' @param drug_string Character string contained in the name of each drug
#'                    column which uniquely identifies them.
#'
#' @return `output`: character vector,
#' \itemize{
#' \item "Not Relevant" if the conditions are not satisfied.
#' \item "Appropriate" if the conditions are satisfied but the correct drug has
#' already been prescribed.
#' \item The name of the criterion if the conditions are satisfied and the
#' correct drug has not been prescribed.
#' }
#'
#' @export
stopp_e1 <- function(df, gfr_column = "Lab Values: eGFR", digoxin_column =
                    "Digoxin Dose >125mcg",
                     drug_string = "Drug_") {

  if (!any(grepl(colnames(df), pattern = gfr_column))) {
    stop(paste0("No column names include ", gfr_column,
                ". Change GFR_string argument."))
  } else if (!any(grepl(colnames(df), pattern = drug_string))) {
    stop(paste0("No column names include ", drug_string,
                ". Change drug_string argument."))
  } else if (!(digoxin_column %in% colnames(df))) {
    stop(paste0("No columns are named ", digoxin_column,
                ". Change digoxin_column argument."))
  }


  # prelim_checks is a list of logical vectors, each has one entry per patient.
  prelim_checks <- list()
  # prelim_codes is a list of character vectors, each containing codes to check.
  prelim_codes <- list()

  # prelim_checks$extras1 is TRUE if the patient's GFR is less than 30ml/min.
  prelim_checks$extras1 <- df[, gfr_column, drop = TRUE] < 30

  # prelim_codes$drugs1 is a character vector of drug codes to check.
  prelim_codes$drugs1 <- c("C01AA05")
  # prelim_checks$drugs1 is TRUE if the patient has any listed drugs.
  prelim_checks$drugs1 <- check_matches(df,
                                        column_string = drug_string,
                                        codes = prelim_codes$drugs1,
                                        match = "any")

  # all_prelims is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'prelim_checks'.
  all_prelims <- Reduce(x = prelim_checks, f = "&")


  # action_checks is a list of logical vectors, each has one entry per patient.
  action_checks <- list()
  # action_codes is a list of character vectors, each containing codes to check.
  action_codes <- list()

  # action_checks$extras1 is TRUE if the patient's digoxin dose is greater than
  # 125mcg.
  action_checks$extras1 <- df[, digoxin_column, drop = TRUE] == "Yes"

  # all_actions is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'action_checks'.
  all_actions <- Reduce(x = action_checks, f = "&")


  output <- ifelse(all_prelims,
                   ifelse(all_actions, "Appropriate", "STOPP-E1"),
                   "Not Relevant")

  return(output)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title Function to implement STOPP-E2 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining STOPP-E2.
#'
#' STOPP-E2 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item Any of the following drugs:
#'
#' B01AE
#' \item GFR less than:
#'
#' 30
#' }
#'
#' @param df Dataframe of patient information.
#' @param gfr_column The name of the patient Na column as a character string.
#' @param drug_string Character string contained in the name of each drug
#'                    column which uniquely identifies them.
#'
#' @return `output`: character vector,
#' \itemize{
#' \item "Not Relevant" if the conditions are not satisfied.
#' \item "Appropriate" if the conditions are satisfied but the correct drug has
#' already been prescribed.
#' \item The name of the criterion if the conditions are satisfied and the
#' correct drug has not been prescribed.
#' }
#'
#' @export
stopp_e2 <- function(df, gfr_column = "Lab Values: eGFR",
                     drug_string = "Drug_") {

  if (!any(grepl(colnames(df), pattern = gfr_column))) {
    stop(paste0("No column names include ", gfr_column,
                ". Change GFR_string argument."))
  } else if (!any(grepl(colnames(df), pattern = drug_string))) {
    stop(paste0("No column names include ", drug_string,
                ". Change drug_string argument."))
  }


  # prelim_checks is a list of logical vectors, each has one entry per patient.
  prelim_checks <- list()
  # prelim_codes is a list of character vectors, each containing codes to check.
  prelim_codes <- list()

  # prelim_codes$drugs1 is a character vector of drug codes to check.
  prelim_codes$drugs1 <- c("B01AE")
  # prelim_checks$drugs1 is TRUE if the patient has any listed drugs.
  prelim_checks$drugs1 <- check_matches(df,
                                        column_string = drug_string,
                                        codes = prelim_codes$drugs1,
                                        match = "any")

  # all_prelims is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'prelim_checks'.
  all_prelims <- Reduce(x = prelim_checks, f = "&")


  # action_checks is a list of logical vectors, each has one entry per patient.
  action_checks <- list()
  # action_codes is a list of character vectors, each containing codes to check.
  action_codes <- list()

  # action_checks$extras1 is TRUE if the patient's GFR is greater than 30ml/min.
  action_checks$extras1 <- df[, gfr_column, drop = TRUE] > 30

  # all_actions is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'action_checks'.
  all_actions <- Reduce(x = action_checks, f = "&")


  output <- ifelse(all_prelims,
                   ifelse(all_actions, "Appropriate", "STOPP-E2"),
                   "Not Relevant")

  return(output)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Function to implement STOPP-E3 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining STOPP-E3.
#'
#' STOPP-E3 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item Any of the following drugs:
#'
#' B01AF
#' \item GFR less than:
#'
#' 15
#' }
#'
#' @param df Dataframe of patient information.
#' @param gfr_column The name of the patient Na column as a character string.
#' @param drug_string Character string contained in the name of each drug
#'                    column which uniquely identifies them.
#'
#' @return `output`: character vector,
#' \itemize{
#' \item "Not Relevant" if the conditions are not satisfied.
#' \item "Appropriate" if the conditions are satisfied but the correct drug has
#' already been prescribed.
#' \item The name of the criterion if the conditions are satisfied and the
#' correct drug has not been prescribed.
#' }
#'
#' @export
stopp_e3 <- function(df, gfr_column = "Lab Values: eGFR",
                     drug_string = "Drug_") {

  if (!any(grepl(colnames(df), pattern = gfr_column))) {
    stop(paste0("No column names include ", gfr_column,
                ". Change GFR_string argument."))
  } else if (!any(grepl(colnames(df), pattern = drug_string))) {
    stop(paste0("No column names include ", drug_string,
                ". Change drug_string argument."))
  }


  # prelim_checks is a list of logical vectors, each has one entry per patient.
  prelim_checks <- list()
  # prelim_codes is a list of character vectors, each containing codes to check.
  prelim_codes <- list()

  # prelim_codes$drugs1 is a character vector of drug codes to check.
  prelim_codes$drugs1 <- c("B01AF")
  # prelim_checks$drugs1 is TRUE if the patient has any listed drugs.
  prelim_checks$drugs1 <- check_matches(df,
                                        column_string = drug_string,
                                        codes = prelim_codes$drugs1,
                                        match = "any")

  # all_prelims is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'prelim_checks'.
  all_prelims <- Reduce(x = prelim_checks, f = "&")


  # action_checks is a list of logical vectors, each has one entry per patient.
  action_checks <- list()
  # action_codes is a list of character vectors, each containing codes to check.
  action_codes <- list()

  # action_checks$extras1 is TRUE if the patient's GFR is greater than 15ml/min.
  action_checks$extras1 <- df[, gfr_column, drop = TRUE] > 15

  # all_actions is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'action_checks'.
  all_actions <- Reduce(x = action_checks, f = "&")


  output <- ifelse(all_prelims,
                   ifelse(all_actions, "Appropriate", "STOPP-E3"),
                   "Not Relevant")

  return(output)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Function to implement STOPP-E4 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining STOPP-E4.
#'
#' STOPP-E4 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item Any of the following drugs:
#'
#' M01A, M01BA01
#' \item GFR less than:
#'
#' 50
#' }
#'
#' @param df Dataframe of patient information.
#' @param gfr_column The name of the patient Na column as a character string.
#' @param drug_string Character string contained in the name of each drug
#'                    column which uniquely identifies them.
#'
#' @return `output`: character vector,
#' \itemize{
#' \item "Not Relevant" if the conditions are not satisfied.
#' \item "Appropriate" if the conditions are satisfied but the correct drug has
#' already been prescribed.
#' \item The name of the criterion if the conditions are satisfied and the
#' correct drug has not been prescribed.
#' }
#'
#' @export
stopp_e4 <- function(df, gfr_column = "Lab Values: eGFR",
                     drug_string = "Drug_") {

  if (!any(grepl(colnames(df), pattern = gfr_column))) {
    stop(paste0("No column names include ", gfr_column,
                ". Change GFR_string argument."))
  } else if (!any(grepl(colnames(df), pattern = drug_string))) {
    stop(paste0("No column names include ", drug_string,
                ". Change drug_string argument."))
  }


  # prelim_checks is a list of logical vectors, each has one entry per patient.
  prelim_checks <- list()
  # prelim_codes is a list of character vectors, each containing codes to check.
  prelim_codes <- list()

  # prelim_codes$drugs1 is a character vector of drug codes to check.
  prelim_codes$drugs1 <- c("M01A", "M01BA01")
  # prelim_checks$drugs1 is TRUE if the patient has any listed drugs.
  prelim_checks$drugs1 <- check_matches(df,
                                        column_string = drug_string,
                                        codes = prelim_codes$drugs1,
                                        match = "any")

  # all_prelims is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'prelim_checks'.
  all_prelims <- Reduce(x = prelim_checks, f = "&")


  # action_checks is a list of logical vectors, each has one entry per patient.
  action_checks <- list()
  # action_codes is a list of character vectors, each containing codes to check.
  action_codes <- list()

  # action_checks$extras1 is TRUE if the patient's GFR is greater than 50ml/min.
  action_checks$extras1 <- df[, gfr_column, drop = TRUE] > 50

  # all_actions is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'action_checks'.
  all_actions <- Reduce(x = action_checks, f = "&")


  output <- ifelse(all_prelims,
                   ifelse(all_actions, "Appropriate", "STOPP-E4"),
                   "Not Relevant")

  return(output)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Function to implement STOPP-E5 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining STOPP-E5.
#'
#' STOPP-E5 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item Any of the following drugs:
#'
#' M04AC01
#' \item GFR less than:
#'
#' 10
#' }
#'
#' @param df Dataframe of patient information.
#' @param gfr_column The name of the patient Na column as a character string.
#' @param drug_string Character string contained in the name of each drug
#'                    column which uniquely identifies them.
#'
#' @return `output`: character vector,
#' \itemize{
#' \item "Not Relevant" if the conditions are not satisfied.
#' \item "Appropriate" if the conditions are satisfied but the correct drug has
#' already been prescribed.
#' \item The name of the criterion if the conditions are satisfied and the
#' correct drug has not been prescribed.
#' }
#'
#' @export
stopp_e5 <- function(df, gfr_column = "Lab Values: eGFR",
                     drug_string = "Drug_") {

  if (!any(grepl(colnames(df), pattern = gfr_column))) {
    stop(paste0("No column names include ", gfr_column,
                ". Change GFR_string argument."))
  } else if (!any(grepl(colnames(df), pattern = drug_string))) {
    stop(paste0("No column names include ", drug_string,
                ". Change drug_string argument."))
  }


  # prelim_checks is a list of logical vectors, each has one entry per patient.
  prelim_checks <- list()
  # prelim_codes is a list of character vectors, each containing codes to check.
  prelim_codes <- list()

  # prelim_codes$drugs1 is a character vector of drug codes to check.
  prelim_codes$drugs1 <- c("M04AC01")
  # prelim_checks$drugs1 is TRUE if the patient has any listed drugs.
  prelim_checks$drugs1 <- check_matches(df,
                                        column_string = drug_string,
                                        codes = prelim_codes$drugs1,
                                        match = "any")

  # all_prelims is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'prelim_checks'.
  all_prelims <- Reduce(x = prelim_checks, f = "&")


  # action_checks is a list of logical vectors, each has one entry per patient.
  action_checks <- list()
  # action_codes is a list of character vectors, each containing codes to check.
  action_codes <- list()

  # action_checks$extras1 is TRUE if the patient's GFR is greater than 50ml/min.
  action_checks$extras1 <- df[, gfr_column, drop = TRUE] > 10

  # all_actions is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'action_checks'.
  all_actions <- Reduce(x = action_checks, f = "&")


  output <- ifelse(all_prelims,
                   ifelse(all_actions, "Appropriate", "STOPP-E5"),
                   "Not Relevant")

  return(output)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Function to implement STOPP-E6 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining STOPP-E6.
#'
#' STOPP-E6 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item Any of the following drugs:
#'
#' A10BA02, A10BD02, A10BD03, A10BD05, A10BD07, A10BD08, A10BD10, A10BD11,
#' A10BD13, A10BD14, A10BD15, A10BD16, A10BD17, A10BD18, A10BD22
#'
#' \item GFR less than:
#'
#' 30
#' }
#'
#' @param df Dataframe of patient information.
#' @param gfr_column The name of the patient Na column as a character string.
#' @param drug_string Character string contained in the name of each drug
#'                    column which uniquely identifies them.
#'
#' @return `output`: character vector,
#' \itemize{
#' \item "Not Relevant" if the conditions are not satisfied.
#' \item "Appropriate" if the conditions are satisfied but the correct drug has
#' already been prescribed.
#' \item The name of the criterion if the conditions are satisfied and the
#' correct drug has not been prescribed.
#' }
#'
#' @export
stopp_e6 <- function(df, gfr_column = "Lab Values: eGFR",
                     drug_string = "Drug_") {

  if (!any(grepl(colnames(df), pattern = gfr_column))) {
    stop(paste0("No column names include ", gfr_column,
                ". Change GFR_string argument."))
  } else if (!any(grepl(colnames(df), pattern = drug_string))) {
    stop(paste0("No column names include ", drug_string,
                ". Change drug_string argument."))
  }


  # prelim_checks is a list of logical vectors, each has one entry per patient.
  prelim_checks <- list()
  # prelim_codes is a list of character vectors, each containing codes to check.
  prelim_codes <- list()

  # prelim_codes$drugs1 is a character vector of drug codes to check.
  prelim_codes$drugs1 <- c("A10BA02", "A10BD02", "A10BD03", "A10BD05",
                           "A10BD07", "A10BD08", "A10BD10", "A10BD11",
                           "A10BD13", "A10BD14", "A10BD15", "A10BD16",
                           "A10BD17", "A10BD18", "A10BD22")
  # prelim_checks$drugs1 is TRUE if the patient has any listed drugs.
  prelim_checks$drugs1 <- check_matches(df,
                                        column_string = drug_string,
                                        codes = prelim_codes$drugs1,
                                        match = "any")

  # all_prelims is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'prelim_checks'.
  all_prelims <- Reduce(x = prelim_checks, f = "&")


  # action_checks is a list of logical vectors, each has one entry per patient.
  action_checks <- list()
  # action_codes is a list of character vectors, each containing codes to check.
  action_codes <- list()

  # action_checks$extras1 is TRUE if the patient's GFR is greater than 30ml/min.
  action_checks$extras1 <- df[, gfr_column, drop = TRUE] > 30

  # all_actions is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'action_checks'.
  all_actions <- Reduce(x = action_checks, f = "&")


  output <- ifelse(all_prelims,
                   ifelse(all_actions, "Appropriate", "STOPP-E6"),
                   "Not Relevant")

  return(output)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

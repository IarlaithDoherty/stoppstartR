#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title Implement STOPP-B1 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining STOPP-B1.
#'
#' @details
#'
#' STOPP-B1 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item Any of the following comorbidities:
#'
#' I50, I11.0, I13.0, I13.2
#' \item None of the following comorbidities:
#'
#' I48
#' \item None of the following drugs:
#'
#' C01AA05
#' }
#'
#' @inheritParams all_stoppstart
#'
#' @return A character vector with one entry per patient / row in `df`. Each
#'         entry takes one of the following three values:
#' \itemize{
#' \item "Not Relevant" if the conditions are not satisfied.
#' \item "Appropriate" if the conditions are satisfied but the incorrect drugs
#'       have not been prescribed.
#' \item The name of the criterion if the conditions are satisfied and the
#'       incorrect drugs have been prescribed. That is, if this STOPP
#'       criterion has been triggered.
#' }
#'
#' @export
stopp_b1 <- function(df, comorb_string = "Comorbidity_",
                     drug_string = "Drug_") {
  if (!any(grepl(colnames(df), pattern = comorb_string))) {
    stop(paste0("No column names include ", comorb_string,
                ". Change comorb_string argument."))
  } else if (!any(grepl(colnames(df), pattern = drug_string))) {
    stop(paste0("No column names include ", drug_string,
                ". Change drug_string argument."))
  }


  # prelim_checks is a list of logical vectors, each has one entry per patient.
  prelim_checks <- list()
  # prelim_codes is a list of character vectors, each containing codes to check.
  prelim_codes <- list()

  # prelim_codes$comorbs1 is a character vector of comorbidity codes to check.
  prelim_codes$comorbs1 <- c("I50", "I11.0", "I13.0", "I13.2")
  # prelim_checks$comorbs1 is TRUE if the patient has any listed comorbidities.
  prelim_checks$comorbs1 <- check_matches(df,
                                          column_string = comorb_string,
                                          codes = prelim_codes$comorbs1,
                                          match = "any")

  # prelim_codes$comorbs2 is a character vector of comorbidity codes to check.
  prelim_codes$comorbs2 <- c("I48")
  # prelim_checks$comorbs2 is TRUE if the patient has none listed comorbidities.
  prelim_checks$comorbs2 <- check_matches(df,
                                          column_string = comorb_string,
                                          codes = prelim_codes$comorbs2,
                                          match = "none")

  # all_prelims is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'prelim_checks'.
  all_prelims <- Reduce(x = prelim_checks, f = "&")


  # action_checks is a list of logical vectors, each has one entry per patient.
  action_checks <- list()
  # action_codes is a list of character vectors, each containing codes to check.
  action_codes <- list()

  # prelim_codes$drugs1 is a character vector of drug codes to check.
  action_codes$drugs1 <- c("C01AA05")
  # prelim_checks$drugs1 is TRUE if the patient is on none listed drugs.
  action_checks$drugs1 <- check_matches(df,
                                        column_string = drug_string,
                                        codes = action_codes$drugs1,
                                        match = "none")

  # all_actions is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'action_checks'.
  all_actions <- Reduce(x = action_checks, f = "&")


  output <- ifelse(all_prelims,
                   ifelse(all_actions, "Appropriate", "STOPP-B1"),
                   "Not Relevant")

  return(output)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title Implement STOPP-B2 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining STOPP-B2.
#'
#' @details
#'
#' STOPP-B2 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item Any of the following comorbidities:
#'
#' I50, I11.0, I13.0, I13.2
#' \item Any of the following drugs:
#'
#' C08DB01, C08DA01, C09BB10, C08DA51
#' }
#'
#' @inheritParams all_stoppstart
#'
#' @inherit stopp_b1 return
#'
#' @export
stopp_b2 <- function(df, comorb_string = "Comorbidity_",
                     drug_string = "Drug_") {
  if (!any(grepl(colnames(df), pattern = comorb_string))) {
    stop(paste0("No column names include ", comorb_string,
                ". Change comorb_string argument."))
  } else if (!any(grepl(colnames(df), pattern = drug_string))) {
    stop(paste0("No column names include ", drug_string,
                ". Change drug_string argument."))
  }


  # prelim_checks is a list of logical vectors, each has one entry per patient.
  prelim_checks <- list()
  # prelim_codes is a list of character vectors, each containing codes to check.
  prelim_codes <- list()

  # prelim_codes$comorbs1 is a character vector of comorbidity codes to check.
  prelim_codes$comorbs1 <- c("I50", "I11.0", "I13.0", "I13.2")
  # prelim_checks$comorbs1 is TRUE if the patient has any listed comorbidities.
  prelim_checks$comorbs1 <- check_matches(df,
                                          column_string = comorb_string,
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
  action_codes$drugs1 <- c("C08DB01", "C08DA01", "C09BB10", "C08DA51")
  # prelim_checks$drugs1 is TRUE if the patient is on none listed drugs.
  action_checks$drugs1 <- check_matches(df,
                                        column_string = drug_string,
                                        codes = action_codes$drugs1,
                                        match = "none")

  # all_actions is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'action_checks'.
  all_actions <- Reduce(x = action_checks, f = "&")


  output <- ifelse(all_prelims,
                   ifelse(all_actions, "Appropriate", "STOPP-B2"),
                   "Not Relevant")

  return(output)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title Implement STOPP-B3 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining STOPP-B3.
#'
#' @details
#'
#' STOPP-B3 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item Any of the following drugs:
#'
#' C08DA01, C09BB10, C08DA51, C08DB01
#'
#' \item Any of the following drugs:
#' C07
#'
#' }
#'
#' @inheritParams all_stoppstart
#'
#' @inherit stopp_b1 return
#'
#' @export
stopp_b3 <- function(df, drug_string = "Drug_") {
  if (!any(grepl(colnames(df), pattern = drug_string))) {
    stop(paste0("No column names include ", drug_string,
                ". Change drug_string argument."))
  }


  # prelim_checks is a list of logical vectors, each has one entry per patient.
  prelim_checks <- list()
  # prelim_codes is a list of character vectors, each containing codes to check.
  prelim_codes <- list()

  # prelim_codes$drugs1 is a character vector of drug codes to check.
  prelim_codes$drugs1 <- c("C08DA01", "C09BB10", "C08DA51",	"C08DB01")
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

  # action_codes$drugs1 is a character vector of drug codes to check.
  action_codes$drugs1 <- c("C07")
  # action_checks$drugs1 is TRUE if the patient is on any listed drugs.
  action_checks$drugs1 <- check_matches(df,
                                        column_string = drug_string,
                                        codes = action_codes$drugs1,
                                        match = "none")

  # all_actions is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'action_checks'.
  all_actions <- Reduce(x = action_checks, f = "&")


  output <- ifelse(all_prelims,
                   ifelse(all_actions, "Appropriate", "STOPP-B3"),
                   "Not Relevant")

  return(output)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'
#' @title Implement STOPP-B4 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining STOPP-B4.
#'
#' @details
#'
#' STOPP-B4 requires the following conditions to be satisfied:
#' \itemize{
#' \item Heart Rate less than 50 beats per minute.
#' OR
#' \item Any of the following comorbidities:
#'
#'I49.5, R00.1, I44.1, I44.2, I44.3, I45.5, I45.9, Q24.6
#' \item Any of the following drugs:
#'
#' C07.
#' }
#'
#' @inheritParams all_stoppstart
#'
#' @inherit stopp_b1 return
#'
#' @export
stopp_b4 <- function(df, hr_column = "Lab Values: Heart Rate",
                     comorb_string = "Comorbidity_",
                     drug_string = "Drug_") {

  if (!any(grepl(colnames(df), pattern = comorb_string))) {
    stop(paste0("No column names include ", comorb_string,
                ". Change comorb_string argument."))
  } else if (!any(grepl(colnames(df), pattern = drug_string))) {
    stop(paste0("No column names include ", drug_string,
                ". Change drug_string argument."))
  } else if (!(hr_column %in% colnames(df))) {
    stop(paste0("No columns are named ", hr_column,
                ". Change hr_column argument."))
  }

  # prelim_checks is a list of logical vectors, each has one entry per patient.
  prelim_checks <- list()
  # prelim_codes is a list of character vectors, each containing codes to check.
  prelim_codes <- list()

  # prelim_checks$extras1 is TRUE if the patient's HR is less than 50 beats per
  # minute.
  prelim_checks$extras1 <- df[, hr_column, drop = TRUE] < 50

  # prelim_codes$comorbs1 is a character vector of comorbidity codes to check.
  prelim_codes$comorbs1 <- c("I49.5", "R00.1", "I44.1", "I44.2", "I44.3",
                             "I45.5", "I45.9", "Q24.6")
  # prelim_checks$comorbs1 is TRUE if the patient has any listed comorbidities.
  prelim_checks$comorbs1 <- check_matches(df,
                                          column_string = comorb_string,
                                          codes = prelim_codes$comorbs1,
                                          match = "any")

  # all_prelims is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for either element of 'prelim_checks'.
  all_prelims <- Reduce(x = prelim_checks, f = "|")

  # action_checks is a list of logical vectors, each has one entry per patient.
  action_checks <- list()
  # action_codes is a list of character vectors, each containing codes to check.
  action_codes <- list()

  # prelim_codes$drugs1 is a character vector of drug codes to check.
  action_codes$drugs1 <- c("C07")
  # prelim_checks$drugs1 is TRUE if the patient is on none of listed drugs.
  action_checks$drugs1 <- check_matches(df,
                                        column_string = drug_string,
                                        codes = action_codes$drugs1,
                                        match = "none")

  # all_actions is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'action_checks'.
  all_actions <- Reduce(x = action_checks, f = "&")


  output <- ifelse(all_prelims,
                   ifelse(all_actions, "Appropriate", "STOPP-B4"),
                   "Not Relevant")

  return(output)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title Implement STOPP-B5 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining STOPP-B5.
#'
#' @details
#'
#' STOPP-B5 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item Any of the following comorbidities:
#'
#' I47.1, I48
#' \item None of the following drugs:
#'
#' C07, C01AA05, C08DA01, C08DB01
#' \item Any of the following drugs:
#'
#' C01BD01
#' }
#'
#' @inheritParams all_stoppstart
#'
#' @inherit stopp_b1 return
#'
#' @export
stopp_b5 <- function(df, comorb_string = "Comorbidity_",
                     drug_string = "Drug_") {
  if (!any(grepl(colnames(df), pattern = comorb_string))) {
    stop(paste0("No column names include ", comorb_string,
                ". Change comorb_string argument."))
  } else if (!any(grepl(colnames(df), pattern = drug_string))) {
    stop(paste0("No column names include ", drug_string,
                ". Change drug_string argument."))
  }


  # prelim_checks is a list of logical vectors, each has one entry per patient.
  prelim_checks <- list()
  # prelim_codes is a list of character vectors, each containing codes to check.
  prelim_codes <- list()

  # prelim_codes$comorbs1 is a character vector of comorbidity codes to check.
  prelim_codes$comorbs1 <- c("I47.1", "I48")
  # prelim_checks$comorbs1 is TRUE if the patient has any listed comorbidities.
  prelim_checks$comorbs1 <- check_matches(df,
                                          column_string = comorb_string,
                                          codes = prelim_codes$comorbs1,
                                          match = "any")

  # prelim_codes$drugs1 is a character vector of drug codes to check.
  prelim_codes$drugs1 <- c("C07", "C01AA05", "C08DA01", "C08DB01")
  # prelim_checks$drugs1 is TRUE if the patient has any listed drugs.
  prelim_checks$drugs1 <- check_matches(df,
                                        column_string = drug_string,
                                        codes = prelim_codes$drugs1,
                                        match = "none")

  # all_prelims is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'prelim_checks'.
  all_prelims <- Reduce(x = prelim_checks, f = "&")


  # action_checks is a list of logical vectors, each has one entry per patient.
  action_checks <- list()
  # action_codes is a list of character vectors, each containing codes to check.
  action_codes <- list()

  # prelim_codes$drugs1 is a character vector of drug codes to check.
  action_codes$drugs1 <- c("C01BD01")
  # prelim_checks$drugs1 is TRUE if the patient is on none listed drugs.
  action_checks$drugs1 <- check_matches(df,
                                        column_string = drug_string,
                                        codes = action_codes$drugs1,
                                        match = "none")

  # all_actions is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'action_checks'.
  all_actions <- Reduce(x = action_checks, f = "&")


  output <- ifelse(all_prelims,
                   ifelse(all_actions, "Appropriate", "STOPP-B5"),
                   "Not Relevant")

  return(output)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#' @title Implement STOPP-B6 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining STOPP-B6.
#'
#' @details
#'
#' STOPP-B6 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item Any of the following comorbidities:
#'
#' I10, I15
#' \item None of the following comorbidities:
#'
#' I50, I11.0, I13.0, I13.2
#' \item None of the following drugs:
#'
#' C07, C08, C09, C03A, C03EA
#' \item Any of the following drugs:
#'
#' C03C, C03EB
#' }
#'
#' @inheritParams all_stoppstart
#'
#' @inherit stopp_b1 return
#'
#' @export
stopp_b6 <- function(df, comorb_string = "Comorbidity_",
                     drug_string = "Drug_") {
  if (!any(grepl(colnames(df), pattern = comorb_string))) {
    stop(paste0("No column names include ", comorb_string,
                ". Change comorb_string argument."))
  } else if (!any(grepl(colnames(df), pattern = drug_string))) {
    stop(paste0("No column names include ", drug_string,
                ". Change drug_string argument."))
  }


  # prelim_checks is a list of logical vectors, each has one entry per patient.
  prelim_checks <- list()
  # prelim_codes is a list of character vectors, each containing codes to check.
  prelim_codes <- list()

  # prelim_codes$comorbs1 is a character vector of comorbidity codes to check.
  prelim_codes$comorbs1 <- c("I10", "I15")
  # prelim_checks$comorbs1 is TRUE if the patient has any listed comorbidities.
  prelim_checks$comorbs1 <- check_matches(df,
                                          column_string = comorb_string,
                                          codes = prelim_codes$comorbs1,
                                          match = "any")

  # prelim_codes$comorbs2 is a character vector of comorbidity codes to check.
  prelim_codes$comorbs2 <- c("I50", "I11.0", "I13.0", "I13.2")
  # prelim_checks$comorbs2 is TRUE if the patient has none listed comorbidities.
  prelim_checks$comorbs2 <- check_matches(df,
                                          column_string = comorb_string,
                                          codes = prelim_codes$comorbs2,
                                          match = "none")

  # prelim_codes$drugs1 is a character vector of drug codes to check.
  prelim_codes$drugs1 <- c("C07", "C08", "C09", "C03A", "C03EA")
  # prelim_checks$drugs1 is TRUE if the patient has any listed drugs.
  prelim_checks$drugs1 <- check_matches(df,
                                        column_string = drug_string,
                                        codes = prelim_codes$drugs1,
                                        match = "none")


  # all_prelims is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'prelim_checks'.
  all_prelims <- Reduce(x = prelim_checks, f = "&")


  # action_checks is a list of logical vectors, each has one entry per patient.
  action_checks <- list()
  # action_codes is a list of character vectors, each containing codes to check.
  action_codes <- list()

  # prelim_codes$drugs1 is a character vector of drug codes to check.
  action_codes$drugs1 <- c("C03C", "C03EB")
  # prelim_checks$drugs1 is TRUE if the patient is on none listed drugs.
  action_checks$drugs1 <- check_matches(df,
                                        column_string = drug_string,
                                        codes = action_codes$drugs1,
                                        match = "none")

  # all_actions is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'action_checks'.
  all_actions <- Reduce(x = action_checks, f = "&")


  output <- ifelse(all_prelims,
                   ifelse(all_actions, "Appropriate", "STOPP-B6"),
                   "Not Relevant")

  return(output)
}

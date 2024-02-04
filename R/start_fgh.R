#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title Implement START-G1 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining START-G1.
#'
#' @details
#'
#' START-G1 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item Gender equals Male.
#' \item Any of the following comorbidities:
#'
#' N40 or R33.
#' \item None of the following drugs:
#'
#' G04CA.
#' }
#'
#' @inheritParams all_stoppstart
#'
#' @inherit start_a1 return
#'
#' @export
start_g1 <- function(df, gender_column = "Gender",
                     comorb_string = "Comorbidity_",
                     drug_string = "Drug_") {

  if (!any(grepl(colnames(df), pattern = comorb_string))) {
    stop(paste0("No column names include ", comorb_string,
                ". Change comorb_string argument."))
  } else if (!any(grepl(colnames(df), pattern = drug_string))) {
    stop(paste0("No column names include ", drug_string,
                ". Change drug_string argument."))
  } else if (!(gender_column %in% colnames(df))) {
    stop(paste0("No columns are named ", gender_column,
                ". Change gender_column argument."))
  }

  # prelim_checks is a list of logical vectors, each has one entry per patient.
  prelim_checks <- list()
  # prelim_codes is a list of character vectors, each containing codes to check.
  prelim_codes <- list()

  # prelim_checks$extras1 is TRUE if the patient's gender is male.
  prelim_checks$extras1 <- df[, gender_column, drop = TRUE] == "M"

  # prelim_codes$comorbs1 is a character vector of comorbidity codes to check.
  prelim_codes$comorbs1 <- c("N40", "R33")
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
  action_codes$drugs1 <- c("G04CA")
  # prelim_checks$drugs1 is TRUE if the patient is on any listed drugs.
  action_checks$drugs1 <- check_matches(df,
                                        column_string = drug_string,
                                        codes = action_codes$drugs1,
                                        match = "any")

  # all_actions is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'action_checks'.
  all_actions <- Reduce(x = action_checks, f = "&")


  output <- ifelse(all_prelims,
                   ifelse(all_actions, "Appropriate", "START-G1"),
                   "Not Relevant")

  return(output)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title Implement START-G2 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining START-G2.
#'
#' @details
#'
#' START-G2 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item Gender equals Male.
#' \item Any of the following comorbidities:
#'
#' N40 or R33.
#' \item None of the following drugs:
#'
#' G04CB.
#' }
#'
#' @inheritParams all_stoppstart
#'
#' @inherit start_a1 return
#'
#' @export
start_g2 <- function(df, gender_column = "Gender",
                     comorb_string = "Comorbidity_",
                     drug_string = "Drug_") {

  if (!any(grepl(colnames(df), pattern = comorb_string))) {
    stop(paste0("No column names include ", comorb_string,
                ". Change comorb_string argument."))
  } else if (!any(grepl(colnames(df), pattern = drug_string))) {
    stop(paste0("No column names include ", drug_string,
                ". Change drug_string argument."))
  } else if (!(gender_column %in% colnames(df))) {
    stop(paste0("No columns are named ", gender_column,
                ". Change gender_column argument."))
  }

  # prelim_checks is a list of logical vectors, each has one entry per patient.
  prelim_checks <- list()
  # prelim_codes is a list of character vectors, each containing codes to check.
  prelim_codes <- list()

  # prelim_checks$extras1 is TRUE if the patient's gender is male.
  prelim_checks$extras1 <- df[, gender_column, drop = TRUE] == "M"

  # prelim_codes$comorbs1 is a character vector of comorbidity codes to check.
  prelim_codes$comorbs1 <- c("N40", "R33")
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
  action_codes$drugs1 <- c("G04CB")
  # prelim_checks$drugs1 is TRUE if the patient is on any listed drugs.
  action_checks$drugs1 <- check_matches(df,
                                        column_string = drug_string,
                                        codes = action_codes$drugs1,
                                        match = "any")

  # all_actions is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'action_checks'.
  all_actions <- Reduce(x = action_checks, f = "&")


  output <- ifelse(all_prelims,
                   ifelse(all_actions, "Appropriate", "START-G2"),
                   "Not Relevant")

  return(output)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title Implement START-G3 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining START-G3.
#'
#' @details
#'
#' START-G3 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item Gender equals Female.
#' \item Any of the following comorbidities:
#'
#' N95.2 .
#' \item None of the following drugs:
#'
#' G03CA03 or G03CA04.
#' }
#'
#' @inheritParams all_stoppstart
#'
#' @inherit start_a1 return
#'
#' @export
start_g3 <- function(df, gender_column = "Gender",
                     comorb_string = "Comorbidity_",
                     drug_string = "Drug_") {

  if (!any(grepl(colnames(df), pattern = comorb_string))) {
    stop(paste0("No column names include ", comorb_string,
                ". Change comorb_string argument."))
  } else if (!any(grepl(colnames(df), pattern = drug_string))) {
    stop(paste0("No column names include ", drug_string,
                ". Change drug_string argument."))
  } else if (!(gender_column %in% colnames(df))) {
    stop(paste0("No columns are named ", gender_column,
                ". Change gender_column argument."))
  }

  # prelim_checks is a list of logical vectors, each has one entry per patient.
  prelim_checks <- list()
  # prelim_codes is a list of character vectors, each containing codes to check.
  prelim_codes <- list()

  # prelim_checks$extras1 is TRUE if the patient's gender is male.
  prelim_checks$extras1 <- df[, gender_column, drop = TRUE] == "F"

  # prelim_codes$comorbs1 is a character vector of comorbidity codes to check.
  prelim_codes$comorbs1 <- c("N95.2")
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
  action_codes$drugs1 <- c("G03CA03", "G03CA04")
  # prelim_checks$drugs1 is TRUE if the patient is on any listed drugs.
  action_checks$drugs1 <- check_matches(df,
                                        column_string = drug_string,
                                        codes = action_codes$drugs1,
                                        match = "any")

  # all_actions is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'action_checks'.
  all_actions <- Reduce(x = action_checks, f = "&")


  output <- ifelse(all_prelims,
                   ifelse(all_actions, "Appropriate", "START-G3"),
                   "Not Relevant")

  return(output)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title Implement START-H2 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining START-H2.
#'
#' @details
#'
#' START-H2 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item Any of the following drugs:
#'
#' N02A, N07BC05, N07BC06, N07BC02, N07BC01, or R05DA04.
#'
#' \item None of the following drugs:
#' A06A or A02AA04.
#'
#' }
#'
#' @inheritParams all_stoppstart
#'
#' @inherit start_a1 return
#'
#' @export
start_h2 <- function(df, drug_string = "Drug_") {
  if (!any(grepl(colnames(df), pattern = drug_string))) {
    stop(paste0("No column names include ", drug_string,
                ". Change drug_string argument."))
  }


  # prelim_checks is a list of logical vectors, each has one entry per patient.
  prelim_checks <- list()
  # prelim_codes is a list of character vectors, each containing codes to check.
  prelim_codes <- list()

  # prelim_codes$drugs1 is a character vector of drug codes to check.
  prelim_codes$drugs1 <- c("N02A", "N07BC05", "N07BC06", "N07BC02", "N07BC01",
                           "R05DA04")
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
  action_codes$drugs1 <- c("A06A", "A02AA04")
  # action_checks$drugs1 is TRUE if the patient is on any listed drugs.
  action_checks$drugs1 <- check_matches(df,
                                        column_string = drug_string,
                                        codes = action_codes$drugs1,
                                        match = "any")

  # all_actions is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'action_checks'.
  all_actions <- Reduce(x = action_checks, f = "&")


  output <- ifelse(all_prelims,
                   ifelse(all_actions, "Appropriate", "START-H2"),
                   "Not Relevant")

  return(output)
}

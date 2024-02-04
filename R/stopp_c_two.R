
#' @title Implement STOPP-C7 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining STOPP-C7.
#'
#' @details
#'
#' STOPP-C requires all of the following conditions to be satisfied:
#' \itemize{
#' \item Any of the following drugs:
#'
#' B01AC05
#' }
#'
#' @inheritParams all_stoppstart
#'
#' @inherit stopp_b1 return
#'
#' @export
stopp_c7 <- function(df, comorb_string = "Comorbidity_",
                     drug_string = "Drug_") {
  if (!any(grepl(colnames(df), pattern = comorb_string))) {
    stop(paste0("No column names include ", comorb_string,
                ". Change comorb_string argument."))
  } else if (!any(grepl(colnames(df), pattern = drug_string))) {
    stop(paste0("No column names include ", drug_string,
                ". Change drug_string argument."))
  }


  # action_checks is a list of logical vectors, each has one entry per patient.
  action_checks <- list()
  # action_codes is a list of character vectors, each containing codes to check.
  action_codes <- list()

  # prelim_codes$drugs1 is a character vector of drug codes to check.
  action_codes$drugs1 <- c("B01AC05")
  # prelim_checks$drugs1 is TRUE if the patient is on none listed drugs.
  action_checks$drugs1 <- check_matches(df,
                                        column_string = drug_string,
                                        codes = action_codes$drugs1,
                                        match = "none")

  # all_actions is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'action_checks'.
  all_actions <- Reduce(x = action_checks, f = "&")


  output <- ifelse(all_actions, "Appropriate", "STOPP-C7")

  return(output)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#' @title Implement STOPP-C8 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining STOPP-C8.
#'
#' @details
#'
#' STOPP-C8 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item Any of the following comorbidities:
#'
#'  I80.2, I26
#' \item None of the following comorbidities:
#'
#' I48
#' \item None of the following drugs:
#'
#' B01AA, B01AE, B01AF
#' }
#'
#' @inheritParams all_stoppstart
#'
#' @inherit stopp_b1 return
#'
#' @export
stopp_c8 <- function(df, comorb_string = "Comorbidity_",
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
  prelim_codes$comorbs1 <- c("I80.2", "I26")
  # prelim_checks$comorbs1 is TRUE if the patient has any listed comorbidities.
  prelim_checks$comorbs1 <- check_matches(df,
                                          column_string = comorb_string,
                                          codes = prelim_codes$comorbs1,
                                          match = "any")

  # prelim_codes$comorbs3 is a character vector of comorbidity codes to check.
  prelim_codes$comorbs3 <- c("I48")
  # prelim_checks$comorbs3 is TRUE if the patient has none listed comorbidities.
  prelim_checks$comorbs3 <- check_matches(df,
                                          column_string = comorb_string,
                                          codes = prelim_codes$comorbs3,
                                          match = "none")

  # all_prelims is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'prelim_checks'.
  all_prelims <- Reduce(x = prelim_checks, f = "&")


  # action_checks is a list of logical vectors, each has one entry per patient.
  action_checks <- list()
  # action_codes is a list of character vectors, each containing codes to check.
  action_codes <- list()

  # prelim_codes$drugs1 is a character vector of drug codes to check.
  action_codes$drugs1 <- c("B01AA", "B01AE", "B01AF")
  # prelim_checks$drugs1 is TRUE if the patient is on none listed drugs.
  action_checks$drugs1 <- check_matches(df,
                                        column_string = drug_string,
                                        codes = action_codes$drugs1,
                                        match = "none")

  # all_actions is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'action_checks'.
  all_actions <- Reduce(x = action_checks, f = "&")


  output <- ifelse(all_prelims,
                   ifelse(all_actions, "Appropriate", "STOPP-c8"),
                   "Not Relevant")

  return(output)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#' @title Implement STOPP-C9 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining STOPP-C9.
#'
#' @details
#'
#' STOPP-C9 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item Any of the following comorbidities:
#'
#' I80.2, I26
#' \item None of the following comorbidities:
#'
#' I48
#' \item None of the following drugs:
#'
#' B01AA, B01AE, B01AF
#' }
#'
#' @inheritParams all_stoppstart
#'
#' @inherit stopp_b1 return
#'
#' @export
stopp_c9 <- function(df, comorb_string = "Comorbidity_",
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
  prelim_codes$comorbs1 <- c("I80.2", "I26")
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
  action_codes$drugs1 <- c("B01AA", "B01AE", "B01AF")
  # prelim_checks$drugs1 is TRUE if the patient is on none listed drugs.
  action_checks$drugs1 <- check_matches(df,
                                        column_string = drug_string,
                                        codes = action_codes$drugs1,
                                        match = "none")

  # all_actions is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'action_checks'.
  all_actions <- Reduce(x = action_checks, f = "&")


  output <- ifelse(all_prelims,
                   ifelse(all_actions, "Appropriate", "STOPP-C9"),
                   "Not Relevant")

  return(output)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title Implement STOPP-C10 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining STOPP-C10.
#'
#' @details
#'
#' STOPP-C10 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item Any of the following drugs:
#'
#' B01AA, B01AE, B01AF
#'
#' \item Any of the following drugs:
#' M01A, N02BA, M01BA
#'
#' }
#'
#' @inheritParams all_stoppstart
#'
#' @inherit stopp_b1 return
#'
#' @export
stopp_c10 <- function(df, drug_string = "Drug_") {
  if (!any(grepl(colnames(df), pattern = drug_string))) {
    stop(paste0("No column names include ", drug_string,
                ". Change drug_string argument."))
  }


  # prelim_checks is a list of logical vectors, each has one entry per patient.
  prelim_checks <- list()
  # prelim_codes is a list of character vectors, each containing codes to check.
  prelim_codes <- list()

  # prelim_codes$drugs1 is a character vector of drug codes to check.
  prelim_codes$drugs1 <- c("B01AA", "B01AE", "B01AF")
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
  action_codes$drugs1 <- c("M01A", "N02BA", "M01BA")
  # action_checks$drugs1 is TRUE if the patient is on any listed drugs.
  action_checks$drugs1 <- check_matches(df,
                                        column_string = drug_string,
                                        codes = action_codes$drugs1,
                                        match = "none")

  # all_actions is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'action_checks'.
  all_actions <- Reduce(x = action_checks, f = "&")


  output <- ifelse(all_prelims,
                   ifelse(all_actions, "Appropriate", "STOPP-C10"),
                   "Not Relevant")

  return(output)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#' @title Implement STOPP-C11 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining STOPP-C11.
#'
#' @details
#'
#' STOPP-C11 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item Any of the following drugs:
#'
#' B01AC
#' \item None of the following drugs:
#'
#' A02BC
#' \item None of the following drugs:
#'
#' M01A, N02BA, M01BA
#' }
#'
#' @inheritParams all_stoppstart
#'
#' @inherit stopp_b1 return
#'
#' @export
stopp_c11 <- function(df,
                      drug_string = "Drug_") {
  if (!any(grepl(colnames(df), pattern = drug_string))) {
    stop(paste0("No column names include ", drug_string,
                ". Change drug_string argument."))
  }


  # prelim_checks is a list of logical vectors, each has one entry per patient.
  prelim_checks <- list()
  # prelim_codes is a list of character vectors, each containing codes to check.
  prelim_codes <- list()

  # prelim_codes$drugs1 is a character vector of drug codes to check.
  prelim_codes$drugs1 <- c("B01AC")
  # prelim_checks$drugs1 is TRUE if the patient has any listed drugs.
  prelim_checks$drugs1 <- check_matches(df,
                                        column_string = drug_string,
                                        codes = prelim_codes$drugs1,
                                        match = "any")

  # prelim_codes$drugs2 is a character vector of drug codes to check.
  prelim_codes$drugs2 <- c("A02BC")
  # prelim_checks$drugs2 is TRUE if the patient has none listed comorbidities.
  prelim_checks$drugs2 <- check_matches(df,
                                        column_string = drug_string,
                                        codes = prelim_codes$drugs2,
                                        match = "none")

  # all_prelims is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'prelim_checks'.
  all_prelims <- Reduce(x = prelim_checks, f = "&")


  # action_checks is a list of logical vectors, each has one entry per patient.
  action_checks <- list()
  # action_codes is a list of character vectors, each containing codes to check.
  action_codes <- list()

  # prelim_codes$drugs1 is a character vector of drug codes to check.
  action_codes$drugs1 <- c("M01A", "N02BA", "M01BA")
  # prelim_checks$drugs1 is TRUE if the patient is on none listed drugs.
  action_checks$drugs1 <- check_matches(df,
                                        column_string = drug_string,
                                        codes = action_codes$drugs1,
                                        match = "none")

  # all_actions is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'action_checks'.
  all_actions <- Reduce(x = action_checks, f = "&")


  output <- ifelse(all_prelims,
                   ifelse(all_actions, "Appropriate", "STOPP-C11"),
                   "Not Relevant")

  return(output)
}

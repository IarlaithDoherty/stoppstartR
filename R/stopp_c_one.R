#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title Function to implement STOPP-C1 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining STOPP-C1.
#'
#' STOPP-C1 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item Any of the following drugs:
#'
#' B01AC06, N02BA01
#'
#' \item Dose of aspirin (acetylsalicylic acid) greater than 150mg =
#'
#' Yes.
#'}
#'
#' @param df Dataframe of patient information.
#' @param drug_string Character string contained in the name of each drug
#'                    column which uniquely identifies them.
#' @param asa_column The name of the patient aspirin column as a
#' character string.
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
stopp_c1 <- function(df, asa_column = "Aspirin dose >150mg",
                     drug_string = "Drug_") {
  if (!any(grepl(colnames(df), pattern = asa_column))) {
    stop(paste0("No column names include ", asa_column,
                ". Change asa_column argument."))
  } else if (!any(grepl(colnames(df), pattern = drug_string))) {
    stop(paste0("No column names include ", drug_string,
                ". Change drug_string argument."))
  }


  # prelim_checks is a list of logical vectors, each has one entry per patient.
  prelim_checks <- list()
  # prelim_codes is a list of character vectors, each containing codes to check.
  prelim_codes <- list()

  # prelim_codes$drugs1 is a character vector of drug codes to check.
  prelim_codes$drugs1 <- c("B01AC06", "N02BA01")
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

  # action_checks$extras1 is True if the patient's aspirin dose column is
  # not equal to "Yes"
  action_checks$extras1 <- df[, asa_column, drop = TRUE] != "Yes"

  # all_actions is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'action_checks'.
  all_actions <- Reduce(x = action_checks, f = "&")


  output <- ifelse(all_prelims,
                   ifelse(all_actions, "Appropriate", "STOPP_C1"),
                   "Not Relevant")

  return(output)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#' @title Function to implement STOPP-C2 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining STOPP-C2.
#'
#' STOPP-C2 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item Any of the following comorbidities:
#'
#' K22.1, K25, K26, K27, K28
#' \item None of the following drugs:
#'
#' A02BC
#' \item Any of the following drugs:
#'
#' B01AC06, B01AC08, N02BA01, M01BA03, C10BX01, C10BX02, C10BX04, C10BX05,
#' C10BX06, C10BX08, N02BA15, N02BA51, N02BA65, N02BA71
#' }
#'
#' @param df Dataframe of patient information.
#' @param comorb_string Character string contained in the name of each
#'                      comorbidity column which uniquely identifies them.
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
stopp_c2 <- function(df, comorb_string = "Comorbidity_",
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
  prelim_codes$comorbs1 <- c("K22.1", "K25", "K26", "K27", "K28")
  # prelim_checks$comorbs1 is TRUE if the patient has any listed comorbidities.
  prelim_checks$comorbs1 <- check_matches(df,
                                          column_string = comorb_string,
                                          codes = prelim_codes$comorbs1,
                                          match = "any")

  # prelim_codes$drugs1 is a character vector of drug codes to check.
  prelim_codes$drugs1 <- c("A02BC")
  # prelim_checks$drugs1 is TRUE if the patient has none listed comorbidities.
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
  action_codes$drugs1 <- c("B01AC06", "B01AC08", "N02BA01", "M01BA03",
  "C10BX01", "C10BX02", "C10BX04", "C10BX05", "C10BX06", "C10BX08", "N02BA15",
  "N02BA51", "N02BA65", "N02BA71"
)
  # prelim_checks$drugs1 is TRUE if the patient is on none listed drugs.
  action_checks$drugs1 <- check_matches(df,
                                        column_string = drug_string,
                                        codes = action_codes$drugs1,
                                        match = "none")

  # all_actions is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'action_checks'.
  all_actions <- Reduce(x = action_checks, f = "&")


  output <- ifelse(all_prelims,
                   ifelse(all_actions, "Appropriate", "STOPP-C2"),
                   "Not Relevant")

  return(output)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title Function to implement STOPP-C5 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining STOPP-C5.
#'
#' STOPP-C5 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item Any of the following comorbidities:
#'
#' I48.2
#' \item Any of the following drugs:
#'
#' B01AA, B01AE, B01AF
#' \item None of the following drugs:
#'
#' B01AC06, B01AC08, B01AC56
#' }
#'
#' @param df Dataframe of patient information.
#' @param comorb_string Character string contained in the name of each
#'                      comorbidity column which uniquely identifies them.
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
stopp_c5 <- function(df, comorb_string = "Comorbidity_",
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
  prelim_codes$comorbs1 <- c("I48.2")
  # prelim_checks$comorbs1 is TRUE if the patient has any listed comorbidities.
  prelim_checks$comorbs1 <- check_matches(df,
                                          column_string = comorb_string,
                                          codes = prelim_codes$comorbs1,
                                          match = "any")

  # prelim_codes$drugs1 is a character vector of drug codes to check.
  prelim_codes$drugs1 <- c("B01AA", "B01AE", "B01AF")
  # prelim_checks$drugs1 is TRUE if the patient has none listed drugs.
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

  # prelim_codes$drugs1 is a character vector of drug codes to check.
  action_codes$drugs1 <- c("B01AC06", "B01AC08", "B01AC56"
  )
  # prelim_checks$drugs1 is TRUE if the patient is on none listed drugs.
  action_checks$drugs1 <- check_matches(df,
                                        column_string = drug_string,
                                        codes = action_codes$drugs1,
                                        match = "none")

  # all_actions is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'action_checks'.
  all_actions <- Reduce(x = action_checks, f = "&")


  output <- ifelse(all_prelims,
                   ifelse(all_actions, "Appropriate", "STOPP-C5"),
                   "Not Relevant")

  return(output)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


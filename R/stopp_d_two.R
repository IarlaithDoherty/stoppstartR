#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Function to implement STOPP-D10 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining STOPP-D10.
#'
#' STOPP-D10 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item Any of the following comorbidities:
#'
#' F51.0, G47.0
#' \item None of the following comorbidities:
#'
#' F20, F25, F29, F00, F01, F02, F03, F07, G30, G31.0, G31.1, G31.8
#' \item Any of the following drugs:
#'
#' N05A
#' \item None of the following drugs:
#'
#' N05AN
#' }
#'
#' @inheritParams all_stoppstart
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
stopp_d10 <- function(df, comorb_string = "Comorbidity_",
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
  prelim_codes$comorbs1 <- c("F51.0", "G47.0")
  # prelim_checks$comorbs1 is TRUE if the patient has any listed comorbidities.
  prelim_checks$comorbs1 <- check_matches(df,
                                          column_string = comorb_string,
                                          codes = prelim_codes$comorbs1,
                                          match = "any")

  # prelim_codes$comorbs2 is a character vector of comorbidity codes to check.
  prelim_codes$comorbs2 <- c("F20", "F25", "F29", "F00", "F01", "F02", "F03",
                             "F07", "G30", "G31.0", "G31.1", "G31.8")
  # prelim_checks$comorbs2 is TRUE if the patient has any listed comorbidities.
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
  action_codes$drugs1 <- c("N05A")
  # prelim_checks$drugs1 is TRUE if the patient is on none listed drugs.
  action_checks$drugs1 <- check_matches(df,
                                        column_string = drug_string,
                                        codes = action_codes$drugs1,
                                        match = "none",
                                        exceptions = c("N05AN"))


  # all_actions is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'action_checks'.
  all_actions <- Reduce(x = action_checks, f = "&")


  output <- ifelse(all_prelims,
                   ifelse(all_actions, "Appropriate", "STOPP-D10"),
                   "Not Relevant")
  return(output)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title Function to implement STOPP-D11 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining STOPP-D11.
#'
#' STOPP-D11 requires the following conditions to be satisfied:
#' \itemize{
#' \item Heart Rate less than 60 beats per minute.
#' OR
#' \item Any of the following comorbidities:
#'
#' I49.5, R00.1,  I44.1, I44.2, I44.3, I45.5, I45.9 , Q24.6, R55
#' OR
#' \item Any of the following drugs:
#'
#' C07, C01AA05, C08DB01, C08DA01, C09BB10, C08DA51.
#' AND
#' \item Any of the following drugs:
#'
#' N06DA
#' }
#'
#' @inheritParams all_stoppstart
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
stopp_d11 <- function(df, hr_column = "Lab Values: Heart Rate",
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
  prelim_checks$extras1 <- df[, hr_column, drop = TRUE] < 60

  # prelim_codes$comorbs1 is a character vector of comorbidity codes to check.
  prelim_codes$comorbs1 <- c("I49.5", "R00.1",  "I44.1", "I44.2", "I44.3",
                             "I45.5", "I45.9", "Q24.6", "R55")
  # prelim_checks$comorbs1 is TRUE if the patient has any listed comorbidities.
  prelim_checks$comorbs1 <- check_matches(df,
                                          column_string = comorb_string,
                                          codes = prelim_codes$comorbs1,
                                          match = "any")
  # prelim_codes$drugs1 is a character vector of drug codes to check.
  prelim_codes$drugs1 <- c("C07", "C01AA05", "C08DB01", "C08DA01", "C09BB10",
                           "C08DA51")
  # prelim_checks$drugs1 is TRUE if the patient is on none listed drugs.
  prelim_checks$drugs1 <- check_matches(df,
                                        column_string = drug_string,
                                        codes = prelim_codes$drugs1,
                                        match = "any")


  # all_prelims is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for any element of 'prelim_checks'.
  all_prelims <- Reduce(x = prelim_checks, f = "|")

  # action_checks is a list of logical vectors, each has one entry per patient.
  action_checks <- list()
  # action_codes is a list of character vectors, each containing codes to check.
  action_codes <- list()

  # prelim_codes$drugs1 is a character vector of drug codes to check.
  action_codes$drugs1 <- c("N06DA")
  # prelim_checks$drugs1 is TRUE if the patient is on none of listed drugs.
  action_checks$drugs1 <- check_matches(df,
                                        column_string = drug_string,
                                        codes = action_codes$drugs1,
                                        match = "none")

  # all_actions is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'action_checks'.
  all_actions <- Reduce(x = action_checks, f = "&")


  output <- ifelse(all_prelims,
                   ifelse(all_actions, "Appropriate", "STOPP-D11"),
                   "Not Relevant")

  return(output)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title Function to implement STOPP-D12 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining STOPP-D12.
#'
#' STOPP-D12 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item Any of the following drugs:
#'
#' N05AA, N05AB, N05AC, N05AX07
#' }
#'
#' @inheritParams all_stoppstart
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
stopp_d12 <- function(df,
                      drug_string = "Drug_") {
  if (!any(grepl(colnames(df), pattern = drug_string))) {
    stop(paste0("No column names include ", drug_string,
                ". Change drug_string argument."))
  }


  # action_checks is a list of logical vectors, each has one entry per patient.
  action_checks <- list()
  # action_codes is a list of character vectors, each containing codes to check.
  action_codes <- list()

  # prelim_codes$drugs1 is a character vector of drug codes to check.
  action_codes$drugs1 <- c("N05AA", "N05AB", "N05AC", "N05AX07")
  # prelim_checks$drugs1 is TRUE if the patient is on none listed drugs.
  action_checks$drugs1 <- check_matches(df,
                                        column_string = drug_string,
                                        codes = action_codes$drugs1,
                                        match = "none")

  # all_actions is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'action_checks'.
  all_actions <- Reduce(x = action_checks, f = "&")


  output <- ifelse(all_actions, "Appropriate", "STOPP-D12")

  return(output)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#' @title Function to implement STOPP-D13 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining STOPP-D13.
#'
#' STOPP-D13 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item Any of the following comorbidities:
#'
#' G25.0
#' \item None of the following comorbidities:
#'
#' G20, G21, G23.1, G23.2, G31.8, G90.3
#' \item None of the following drugs:
#'
#' N04B
#' }
#'
#' @inheritParams all_stoppstart
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
stopp_d13 <- function(df, comorb_string = "Comorbidity_",
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
  prelim_codes$comorbs1 <- c("G25.0")
  # prelim_checks$comorbs1 is TRUE if the patient has any listed comorbidities.
  prelim_checks$comorbs1 <- check_matches(df,
                                          column_string = comorb_string,
                                          codes = prelim_codes$comorbs1,
                                          match = "any")

  # prelim_codes$comorbs2 is a character vector of comorbidity codes to check.
  prelim_codes$comorbs2 <- c("G20", "G21", "G23.1", "G23.2", "G31.8", "G90.3")
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
  action_codes$drugs1 <- c("N04B")
  # prelim_checks$drugs1 is TRUE if the patient is on none listed drugs.
  action_checks$drugs1 <- check_matches(df,
                                        column_string = drug_string,
                                        codes = action_codes$drugs1,
                                        match = "none")

  # all_actions is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'action_checks'.
  all_actions <- Reduce(x = action_checks, f = "&")


  output <- ifelse(all_prelims,
                   ifelse(all_actions, "Appropriate", "STOPP-D13"),
                   "Not Relevant")

  return(output)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Function to implement STOPP-D14 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining STOPP-D14.
#'
#' STOPP-D14 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item Any of the following drugs:
#'
#' R06AA, R06AB, R06AC, R06AD, R06AE01, R06AE03, R06AE04, R06AE05, R06AE06,
#' R06AE51, R06AE53, R06AE55, R06AX01, R06AX02, R06AX03, R06AX04, R06AX05,
#' R06AX08, R06AX09, R06AX15, R06AX16, R06AX17, R06AX23, R06AX53, R06AX58,
#' N05BB01, N05BB51, N07CA02, N07CA52
#' }
#'
#' @inheritParams all_stoppstart
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
stopp_d14 <- function(df,
                      drug_string = "Drug_") {
  if (!any(grepl(colnames(df), pattern = drug_string))) {
    stop(paste0("No column names include ", drug_string,
                ". Change drug_string argument."))
  }


  # action_checks is a list of logical vectors, each has one entry per patient.
  action_checks <- list()
  # action_codes is a list of character vectors, each containing codes to check.
  action_codes <- list()

  # prelim_codes$drugs1 is a character vector of drug codes to check.
  action_codes$drugs1 <- c("R06AA", "R06AB", "R06AC", "R06AD", "R06AE01",
                           "R06AE03", "R06AE04", "R06AE05", "R06AE06",
                           "R06AE51", "R06AE53", "R06AE55", "R06AX01",
                           "R06AX02", "R06AX03", "R06AX04", "R06AX05",
                           "R06AX08", "R06AX09", "R06AX15", "R06AX16",
                           "R06AX17", "R06AX23", "R06AX53", "R06AX58",
                           "N05BB01", "N05BB51", "N07CA02", "N07CA52")
  # prelim_checks$drugs1 is TRUE if the patient is on none listed drugs.
  action_checks$drugs1 <- check_matches(df,
                                        column_string = drug_string,
                                        codes = action_codes$drugs1,
                                        match = "none")

  # all_actions is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'action_checks'.
  all_actions <- Reduce(x = action_checks, f = "&")


  output <- ifelse(all_actions, "Appropriate", "STOPP-D14")

  return(output)
}

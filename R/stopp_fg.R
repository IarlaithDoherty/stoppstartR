#' @title Function to implement STOPP-F1 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining STOPP-F1.
#'
#' STOPP-F1 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item Any of the following comorbidities:
#'
#' G20, G21, G23.1, G23.2, G31.8
#' \item Any of the following drugs:
#'
#' N05AB04, A03FA01
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
stopp_f1 <- function(df, comorb_string = "Comorbidity_",
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
  prelim_codes$comorbs1 <- c("G20", "G21", "G23.1", "G23.2", "G31.8"
)
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
  action_codes$drugs1 <- c("N05AB04", "A03FA01"
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
                   ifelse(all_actions, "Appropriate", "STOPP-F1"),
                   "Not Relevant")

  return(output)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Function to implement STOPP-F3 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining STOPP-F3.
#'
#' STOPP-F3 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item Any of the following comorbidities:
#'
#' K59.0
#' \item Any of the following drugs:
#'
#' R06AA, R06AB, R06AC, R06AD , R06AE01, R06AE03, R06AE04, R06AE05, R06AE06,
#' R06AE51, R06AE53, R06AE55, R06AX01, R06AX02, R06AX03, R06AX04, R06AX05,
#' R06AX08, R06AX09, R06AX15, R06AX16, R06AX17, R06AX23, R06AX53, R06AX58,
#' N05BB01, N05BB51, N07CA02, N07CA52, G04BD01, G04BD02, G04BD03, G04BD04,
#' G04BD05, G04BD06, G04BD07, G04BD08, G04BD09, G04BD10, G04BD11, N06AA, A03B,
#' A03AA, A03AB, A04AD01, M03BX02, N04A, N05AA01, N05AH02, N05AB02, N05AA02,
#' N05AC02, N05AA04, N05AF04, B03AA, B03AB, B03AD, B03AE
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
stopp_f3 <- function(df, comorb_string = "Comorbidity_",
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
  prelim_codes$comorbs1 <- c("K59.0"
  )
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
  action_codes$drugs1 <- c("R06AA", "R06AB", "R06AC", "R06AD" , "R06AE01",
                           "R06AE03", "R06AE04", "R06AE05", "R06AE06",
                           "R06AE51", "R06AE53", "R06AE55", "R06AX01",
                           "R06AX02", "R06AX03", "R06AX04", "R06AX05",
                           "R06AX08", "R06AX09", "R06AX15", "R06AX16",
                           "R06AX17", "R06AX23", "R06AX53", "R06AX58",
                           "N05BB01", "N05BB51", "N07CA02", "N07CA52",
                           "G04BD01", "G04BD02", "G04BD03", "G04BD04",
                           "G04BD05", "G04BD06", "G04BD07", "G04BD08",
                           "G04BD09", "G04BD10", "G04BD11", "N06AA", "A03B",
                           "A03AA", "A03AB", "A04AD01", "M03BX02", "N04A",
                           "N05AA01", "N05AH02", "N05AB02", "N05AA02",
                           "N05AC02", "N05AA04", "N05AF04", "B03AA", "B03AB",
                           "B03AD", "B03AE"
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
                   ifelse(all_actions, "Appropriate", "STOPP-F3"),
                   "Not Relevant")

  return(output)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
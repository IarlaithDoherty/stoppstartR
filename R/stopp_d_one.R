#' @title Implement STOPP-D1 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining STOPP-D1.
#'
#' @details
#'
#' STOPP-D1 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item Any of the following comorbidities:
#'
#' F00, F01, F02, F03, G30, G31.0, G31.1, G31.8, H40.2, I44, I45, N40, R33
#' \item Any of the following drugs:
#'
#' N06AA, N06CA01, N06CA02
#' }
#'
#' @inheritParams all_stoppstart
#'
#' @inherit stopp_b1 return
#'
#' @export
stopp_d1 <- function(df, comorb_string = "Comorbidity_",
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
  prelim_codes$comorbs1 <- c("F00", "F01", "F02", "F03",
                             "G30", "G31.0", "G31.1", "G31.8",
                             "H40.2", "I44", "I45", "N40", "R33")
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
  action_codes$drugs1 <- c("N06AA", "N06CA01", "N06CA02")
  # prelim_checks$drugs1 is TRUE if the patient is on none listed drugs.
  action_checks$drugs1 <- check_matches(df,
                                        column_string = drug_string,
                                        codes = action_codes$drugs1,
                                        match = "none")

  # all_actions is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'action_checks'.
  all_actions <- Reduce(x = action_checks, f = "&")


  output <- ifelse(all_prelims,
                   ifelse(all_actions, "Appropriate", "STOPP-D1"),
                   "Not Relevant")

  return(output)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Implement STOPP-D3 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining STOPP-D3.
#'
#' @details
#'
#' STOPP-D3 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item Any of the following comorbidities:
#'
#' N40, R33
#' \item Any of the following drugs:
#'
#' N05AA01, N05AH02, N05AF01, N05AB02, N05AC04, N05AA03, N05AF05
#' }
#'
#' @inheritParams all_stoppstart
#'
#' @inherit stopp_b1 return
#'
#' @export
stopp_d3 <- function(df, comorb_string = "Comorbidity_",
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
  action_codes$drugs1 <- c("N05AA01", "N05AH02", "N05AF01", "N05AB02",
                           "N05AC04", "N05AA03", "N05AF05")
  # prelim_checks$drugs1 is TRUE if the patient is on none listed drugs.
  action_checks$drugs1 <- check_matches(df,
                                        column_string = drug_string,
                                        codes = action_codes$drugs1,
                                        match = "none")

  # all_actions is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'action_checks'.
  all_actions <- Reduce(x = action_checks, f = "&")


  output <- ifelse(all_prelims,
                   ifelse(all_actions, "Appropriate", "STOPP-D3"),
                   "Not Relevant")

  return(output)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Implement STOPP-D4 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining STOPP-D4.
#'
#' @details
#'
#' STOPP-D4 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item Serum sodium less than:
#'
#' 130
#' \item Any of the following drugs:
#'
#' N06AB, N06CA03
#' }
#'
#' @inheritParams all_stoppstart
#'
#' @inherit stopp_b1 return
#'
#' @export
stopp_d4 <- function(df, sodium_column = "Lab Values: Na",
                     drug_string = "Drug_") {

  if (!any(grepl(colnames(df), pattern = "Lab Values: Na"))) {
    stop(paste0("No column names include ", sodium_column,
                ". Change sodium_column argument."))
  } else if (!any(grepl(colnames(df), pattern = drug_string))) {
    stop(paste0("No column names include ", drug_string,
                ". Change drug_string argument."))
  }


  # prelim_checks is a list of logical vectors, each has one entry per patient.
  prelim_checks <- list()

  # prelim_checks$extras1 is TRUE if the patient's Na is less than 130 mmol/L.
  prelim_checks$extras1 <- df[, sodium_column, drop = TRUE] < 130


  # all_prelims is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'prelim_checks'.
  all_prelims <- Reduce(x = prelim_checks, f = "&")


  # action_checks is a list of logical vectors, each has one entry per patient.
  action_checks <- list()
  # action_codes is a list of character vectors, each containing codes to check.
  action_codes <- list()

  # prelim_codes$drugs1 is a character vector of drug codes to check.
  action_codes$drugs1 <- c("N06AB", "N06CA03")
  # prelim_checks$drugs1 is TRUE if the patient is on none listed drugs.
  action_checks$drugs1 <- check_matches(df,
                                        column_string = drug_string,
                                        codes = action_codes$drugs1,
                                        match = "none")

  # all_actions is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'action_checks'.
  all_actions <- Reduce(x = action_checks, f = "&")


  output <- ifelse(all_prelims,
                   ifelse(all_actions, "Appropriate", "STOPP-D4"),
                   "Not Relevant")

  return(output)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title Implement STOPP-D5 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining STOPP-D5.
#'
#' @details
#'
#' STOPP-D5 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item Any of the following drugs:
#'
#' N05BA, N05CD, N05CF, N03AE
#' }
#'
#' @inheritParams all_stoppstart
#'
#' @inherit stopp_b1 return
#'
#' @export
stopp_d5 <- function(df,
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
  action_codes$drugs1 <- c("N05BA", "N05CD", "N05CF", "N03AE")
  # prelim_checks$drugs1 is TRUE if the patient is on none listed drugs.
  action_checks$drugs1 <- check_matches(df,
                                        column_string = drug_string,
                                        codes = action_codes$drugs1,
                                        match = "none")

  # all_actions is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'action_checks'.
  all_actions <- Reduce(x = action_checks, f = "&")


  output <- ifelse(all_actions, "Appropriate", "STOPP-D5")

  return(output)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Implement STOPP-D6 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining STOPP-D6.
#'
#' @details
#'
#' STOPP-D6 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item Any of the following comorbidities:
#'
#' G20, G21, G23.1, G23.2, G31.8, G90.3
#' \item Any of the following drugs:
#'
#' N05A
#' \item None of the following drugs:
#'
#' N05AH04, N05AH02, N05AN
#' }
#'
#' @inheritParams all_stoppstart
#'
#' @inherit stopp_b1 return
#'
#' @export
stopp_d6 <- function(df, comorb_string = "Comorbidity_",
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
  prelim_codes$comorbs1 <- c("G20", "G21", "G23.1", "G23.2", "G31.8", "G90.3")
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
  action_codes$drugs1 <- c("N05A")
  # prelim_checks$drugs1 is TRUE if the patient is on none listed drugs.
  action_checks$drugs1 <- check_matches(df,
                                        column_string = drug_string,
                                        codes = action_codes$drugs1,
                                        match = "none",
                                        exceptions = c("N05AH04", "N05AH02",
                                                       "N05AN"))


  # all_actions is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'action_checks'.
  all_actions <- Reduce(x = action_checks, f = "&")


  output <- ifelse(all_prelims,
                   ifelse(all_actions, "Appropriate", "STOPP-D6"),
                   "Not Relevant")
  return(output)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title Implement STOPP-D7 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining STOPP-D7.
#'
#' @details
#'
#' STOPP-D7 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item Any of the following drugs:
#'
#' N05A, except N05AN

#' \item Any of the following drugs:
#'
#' N04A
#' }
#'
#' @inheritParams all_stoppstart
#'
#' @inherit stopp_b1 return
#'
#' @export
stopp_d7 <- function(df, drug_string = "Drug_") {
  if (!any(grepl(colnames(df), pattern = drug_string))) {
    stop(paste0("No column names include ", drug_string,
                ". Change drug_string argument."))
  }


  # prelim_checks is a list of logical vectors, each has one entry per patient.
  prelim_checks <- list()
  # prelim_codes is a list of character vectors, each containing codes to check.
  prelim_codes <- list()

  # prelim_codes$drugs1 is a character vector of comorbidity codes to check.
  prelim_codes$drugs1 <- c("N05A")
  # prelim_checks$drugs1 is TRUE if the patient has any listed comorbidities.
  prelim_checks$drugs1 <- check_matches(df,
                                        column_string = drug_string,
                                        codes = prelim_codes$drugs1,
                                        match = "any",
                                        exceptions = c("N05AN"))

  # all_prelims is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'prelim_checks'.
  all_prelims <- Reduce(x = prelim_checks, f = "&")


  # action_checks is a list of logical vectors, each has one entry per patient.
  action_checks <- list()
  # action_codes is a list of character vectors, each containing codes to check.
  action_codes <- list()

  # action_codes$drugs1 is a character vector of drug codes to check.
  action_codes$drugs1 <- c("N04A"
  )
  # action_checks$drugs1 is TRUE if the patient is on any listed drugs.
  action_checks$drugs1 <- check_matches(df,
                                        column_string = drug_string,
                                        codes = action_codes$drugs1,
                                        match = "none")

  # all_actions is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'action_checks'.
  all_actions <- Reduce(x = action_checks, f = "&")


  output <- ifelse(all_prelims,
                   ifelse(all_actions, "Appropriate", "STOPP-D7"),
                   "Not Relevant")

  return(output)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title Implement STOPP-D8 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining STOPP-D8.
#'
#' @details
#'
#' STOPP-D8 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item Any of the following comorbidities:
#'
#' F05, F00, F01, F02, F03, G30, G31.0, G31.1, G31.8

#' \item Any of the following drugs:
#'
#' R06AA, R06AB, R06AC, R06AD, R06AE01, R06AE03, R06AE04, R06AE05, R06AE06,
#' R06AE51, R06AE53, R06AE55, R06AX01, R06AX02, R06AX03, R06AX04, R06AX05,
#'  R06AX08, R06AX09, R06AX15, R06AX16, R06AX17, R06AX23, R06AX53, R06AX58,
#'   N05BB01, N05BB51, N07CA02, N07CA52, G04BD01, G04BD02, G04BD03, G04BD04,
#'    G04BD05, G04BD06, G04BD07, G04BD08, G04BD09, G04BD10, G04BD11, N06AA,
#'     A03B, A03AA, A03AB, A04AD01, M03BX02, N04A, N05AA01, N05AH02, N05AB02,
#'      N05AA02, N05AC02, N05AA04, N05AF04
#' }
#'
#' @inheritParams all_stoppstart
#'
#' @inherit stopp_b1 return
#'
#' @export
stopp_d8 <- function(df, comorb_string = "Comorbidity_",
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
  prelim_codes$comorbs1 <- c("F00", "F01", "F02", "F03", "G30", "G31.0",
                             "G31.1", "F05")
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
  action_codes$drugs1 <- c("R06AA", "R06AB", "R06AC", "R06AD", "R06AE01",
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
                           "N05AC02", "N05AA04", "N05AF04")
  # prelim_checks$drugs1 is TRUE if the patient is on none listed drugs.
  action_checks$drugs1 <- check_matches(df,
                                        column_string = drug_string,
                                        codes = action_codes$drugs1,
                                        match = "none")

  # all_actions is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'action_checks'.
  all_actions <- Reduce(x = action_checks, f = "&")


  output <- ifelse(all_prelims,
                   ifelse(all_actions, "Appropriate", "STOPP-D8"),
                   "Not Relevant")

  return(output)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Implement STOPP-D9 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining STOPP-D9.
#'
#' @details
#'
#' STOPP-D9 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item Any of the following comorbidities:
#'
#' F00, F01, F02, F03, F07, G30, G31.0, G31.1, G31.8
#' \item None of the following comorbidities:
#'
#' F20, F25, F29, F51.0, G47.0
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
#' @inherit stopp_b1 return
#'
#' @export
stopp_d9 <- function(df, comorb_string = "Comorbidity_",
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
  prelim_codes$comorbs1 <- c("F00", "F01", "F02", "F03", "F07", "G30", "G31.0",
                             "G31.1", "G31.8")
  # prelim_checks$comorbs1 is TRUE if the patient has any listed comorbidities.
  prelim_checks$comorbs1 <- check_matches(df,
                                          column_string = comorb_string,
                                          codes = prelim_codes$comorbs1,
                                          match = "any")

  # prelim_codes$comorbs2 is a character vector of comorbidity codes to check.
  prelim_codes$comorbs2 <- c("F20", "F25", "F29", "F51.0", "G47.0")
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
                   ifelse(all_actions, "Appropriate", "STOPP-D9"),
                   "Not Relevant")
  return(output)
}

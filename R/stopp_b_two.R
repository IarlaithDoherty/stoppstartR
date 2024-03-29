#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title Implement STOPP-B7 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining STOPP-B7.
#'
#' @details
#'
#' STOPP-B7 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item Any of the following comorbidities:
#'
#' I87.2, R60
#' \item None of the following comorbidities:
#'
#' I50, I11.0, I13.0, I13.2
#' \item None of the following drugs:
#'
#' C03C, C03EB
#' }
#'
#' @inheritParams all_stoppstart
#'
#' @inherit stopp_b1 return
#'
#' @export
stopp_b7 <- function(df, comorb_string = "Comorbidity_",
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
  prelim_codes$comorbs1 <- c("I87.2", "R60")
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
                   ifelse(all_actions, "Appropriate", "STOPP-B7"),
                   "Not Relevant")

  return(output)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title Implement STOPP-B8 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining STOPP-B8.
#'
#' @details
#'
#' STOPP-B8 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item Any of the following drugs:
#'
#' C03A, C03BA, C03EA01, C03EA02, C03EA13, C03EA07, C09XA52, C09XA54, C07B,
#' C07D, C09DX01, C09DX03
#' \item Any of the following comorbidities:
#'
#' M10, E87.6, E87.1, E83.5
#' \item Serum Corrected Calcium > 2.65
#' \item Serum Sodium < 130
#' \item Serum Potassium < 3.5
#'
#' }
#'
#' @inheritParams all_stoppstart
#'
#' @inherit stopp_b1 return
#'
#' @export
stopp_b8 <- function(df, potassium_column = "Lab Values: K",
                     sodium_column = "Lab Values: Na",
                     calcium_column = "Lab Values: Corrected Ca",
                     drug_string = "Drug_",
                     comorb_string = "Comorbidity_") {

  if (!any(grepl(colnames(df), pattern = potassium_column))) {
    stop(paste0("No column names include ", potassium_column,
                ". Change K_string argument."))
  } else if (!any(grepl(colnames(df), pattern = drug_string))) {
    stop(paste0("No column names include ", drug_string,
                ". Change drug_string argument."))
  } else if (!(sodium_column %in% colnames(df))) {
    stop(paste0("No columns are named ", sodium_column,
                ". Change sodium_column argument."))
  } else if (!(calcium_column %in% colnames(df))) {
    stop(paste0("No columns are named ", calcium_column,
                ". Change calcium_column argument."))
  } else if (!any(grepl(colnames(df), pattern = comorb_string))) {
    stop(paste0("No column names include ", comorb_string,
                ". Change comorb_string argument."))
  }


  # prelim_checks is a list of logical vectors, each has one entry per patient.
  prelim_checks <- list()
  # prelim_codes is a list of character vectors, each containing codes to check.
  prelim_codes <- list()

  # prelim_codes$drugs1 is a character vector of drug codes to check.
  prelim_codes$drugs1 <- c("C03A", "C03BA", "C03EA01", "C03EA02", "C03EA13",
                           "C03EA07", "C09XA52", "C09XA54", "C07B", "C07D",
                           "C09DX01", "C09DX03")

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

  # action_checks$extras1 is TRUE if the patient's serum sodium is greater than
  # 130.
  action_checks$extras1 <- df[, sodium_column, drop = TRUE] > 130

  # action_checks$extras2 is TRUE if the patient's serum potassium is greater
  # than 3.5.
  action_checks$extras2 <- df[, potassium_column, drop = TRUE] > 3.5

  # action_checks$extras3 is TRUE if the patient's corrected calcium is less
  # than 2.65.
  action_checks$extras3 <- df[, calcium_column, drop = TRUE] < 2.65

  # action_codes$comorb4 is a character vector of drug codes to check.
  action_codes$comorb4 <- c("M10", "E87.6", "E87.1", "E83.5")
  # action_checks$drugs4 is TRUE if the patient has none listed comorbidities.
  action_checks$comorb4 <- check_matches(df,
                                         column_string = comorb_string,
                                         codes = action_codes$comorb4,
                                         match = "none")


  # all_actions is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'action_checks'.
  all_actions <- Reduce(x = action_checks, f = "&")


  output <- ifelse(all_prelims,
                   ifelse(all_actions, "Appropriate", "STOPP-B8"),
                   "Not Relevant")

  return(output)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title Implement STOPP-B9 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining STOPP-B9.
#'
#' @details
#'
#' STOPP-B9 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item Any of the following comorbidities:
#'
#' I10, I15
#' \item Any of the following comorbidities:
#'
#' N39.3, N39.4, R32
#' \item None of the following comorbidities:
#'
#' I50, I11.0, I13.0, I13.2
#' \item None of the following drugs:
#'
#' C03C, C03EB
#' }
#'
#' @inheritParams all_stoppstart
#'
#' @inherit stopp_b1 return
#'
#' @export
stopp_b9 <- function(df, comorb_string = "Comorbidity_",
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
  prelim_codes$comorbs2 <- c("N39.3", "N39.4", "R32")
  # prelim_checks$comorbs2 is TRUE if the patient has any listed comorbidities.
  prelim_checks$comorbs2 <- check_matches(df,
                                          column_string = comorb_string,
                                          codes = prelim_codes$comorbs2,
                                          match = "any")

  # prelim_codes$comorbs3 is a character vector of comorbidity codes to check.
  prelim_codes$comorbs3 <- c("I50", "I11.0", "I13.0", "I13.2")
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
                   ifelse(all_actions, "Appropriate", "STOPP-B9"),
                   "Not Relevant")

  return(output)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title Implement STOPP-B10 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining STOPP-B10.
#'
#' @details
#'
#' STOPP-B10 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item Any of the following drugs:
#'
#' C02AB, C02AC, C02LC, C02LB
#' }
#'
#' @inheritParams all_stoppstart
#'
#' @inherit stopp_b1 return
#'
#' @export
stopp_b10 <- function(df, comorb_string = "Comorbidity_",
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
  action_codes$drugs1 <- c("C02AB", "C02AC", "C02LC", "C02LB")
  # prelim_checks$drugs1 is TRUE if the patient is on none listed drugs.
  action_checks$drugs1 <- check_matches(df,
                                        column_string = drug_string,
                                        codes = action_codes$drugs1,
                                        match = "none")

  # all_actions is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'action_checks'.
  all_actions <- Reduce(x = action_checks, f = "&")


  output <- ifelse(all_actions, "Appropriate", "STOPP-B10")

  return(output)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'
#' @title Implement STOPP-B11 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining STOPP-B11.
#'
#' @details
#'
#' STOPP-B11 requires the following conditions to be satisfied:
#' \itemize{
#' \item Serum potassium >5 .
#' OR
#' \item Any of the following comorbidities:
#'
#' E87.5
#' \item Any of the following drugs:
#'
#' C09.
#' }
#'
#' @inheritParams all_stoppstart
#'
#' @inherit stopp_b1 return
#'
#' @export
stopp_b11 <- function(df, potassium_column = "Lab Values: K",
                      comorb_string = "Comorbidity_",
                      drug_string = "Drug_") {

  if (!any(grepl(colnames(df), pattern = comorb_string))) {
    stop(paste0("No column names include ", comorb_string,
                ". Change comorb_string argument."))
  } else if (!any(grepl(colnames(df), pattern = drug_string))) {
    stop(paste0("No column names include ", drug_string,
                ". Change drug_string argument."))
  } else if (!(potassium_column %in% colnames(df))) {
    stop(paste0("No columns are named ", potassium_column,
                ". Change potassium_column argument."))
  }

  # prelim_checks is a list of logical vectors, each has one entry per patient.
  prelim_checks <- list()
  # prelim_codes is a list of character vectors, each containing codes to check.
  prelim_codes <- list()

  # prelim_checks$extras1 is TRUE if the patient's serum potassium is greater
  # than 5.
  prelim_checks$extras1 <- df[, potassium_column, drop = TRUE] > 5

  # prelim_codes$comorbs1 is a character vector of comorbidity codes to check.
  prelim_codes$comorbs1 <- c("E87.5")
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
  action_codes$drugs1 <- c("C09")
  # prelim_checks$drugs1 is TRUE if the patient is on none of listed drugs.
  action_checks$drugs1 <- check_matches(df,
                                        column_string = drug_string,
                                        codes = action_codes$drugs1,
                                        match = "none")

  # all_actions is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'action_checks'.
  all_actions <- Reduce(x = action_checks, f = "&")


  output <- ifelse(all_prelims,
                   ifelse(all_actions, "Appropriate", "STOPP-B11"),
                   "Not Relevant")

  return(output)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title Implement STOPP-B13a criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining STOPP-B13a.
#'
#' @details
#'
#' STOPP-B13a requires the following conditions to be satisfied:
#' \itemize{
#' \item Systolic_BP < 90
#'
#' \item Any of the following comorbidities:
#'
#' I50, I11.0, I13.0, I13.2
#' \item Any of the following drugs:
#'
#' G04BE10, G04BE03, G04BE08, G04BE09
#' }
#'
#' @inheritParams all_stoppstart
#'
#' @inherit stopp_b1 return
#'
#' @export
stopp_b13a <- function(df, systolic_column = "Systolic_BP",
                       comorb_string = "Comorbidity_",
                       drug_string = "Drug_") {

  if (!any(grepl(colnames(df), pattern = comorb_string))) {
    stop(paste0("No column names include ", comorb_string,
                ". Change comorb_string argument."))
  } else if (!any(grepl(colnames(df), pattern = drug_string))) {
    stop(paste0("No column names include ", drug_string,
                ". Change drug_string argument."))
  } else if (!(systolic_column %in% colnames(df))) {
    stop(paste0("No columns are named ", systolic_column,
                ". Change systolic_column argument."))
  }

  # prelim_checks is a list of logical vectors, each has one entry per patient.
  prelim_checks <- list()
  # prelim_codes is a list of character vectors, each containing codes to check.
  prelim_codes <- list()

  # prelim_checks$extras1 is TRUE if the patient's systolic BP is less
  # than 90.
  prelim_checks$extras1 <- df[, systolic_column, drop = TRUE] < 90

  # prelim_codes$comorbs1 is a character vector of comorbidity codes to check.
  prelim_codes$comorbs1 <- c("I50", "I11.0", "I13.0", "I13.2")
  # prelim_checks$comorbs1 is TRUE if the patient has any listed comorbidities.
  prelim_checks$comorbs1 <- check_matches(df,
                                          column_string = comorb_string,
                                          codes = prelim_codes$comorbs1,
                                          match = "any")

  # all_prelims is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for either element of 'prelim_checks'.
  all_prelims <- Reduce(x = prelim_checks, f = "&")

  # action_checks is a list of logical vectors, each has one entry per patient.
  action_checks <- list()
  # action_codes is a list of character vectors, each containing codes to check.
  action_codes <- list()

  # prelim_codes$drugs1 is a character vector of drug codes to check.
  action_codes$drugs1 <- c("G04BE10", "G04BE03", "G04BE08", "G04BE09")
  # prelim_checks$drugs1 is TRUE if the patient is on none of listed drugs.
  action_checks$drugs1 <- check_matches(df,
                                        column_string = drug_string,
                                        codes = action_codes$drugs1,
                                        match = "none")

  # all_actions is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'action_checks'.
  all_actions <- Reduce(x = action_checks, f = "&")


  output <- ifelse(all_prelims,
                   ifelse(all_actions, "Appropriate", "STOPP-B13a"),
                   "Not Relevant")

  return(output)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title Implement STOPP-B13b criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining STOPP-B13b.
#'
#' @details
#'
#' STOPP-B13b requires all of the following conditions to be satisfied:
#' \itemize{
#' \item Any of the following drugs:
#'
#' C01DA
#'
#' \item Any of the following drugs:
#' G04BE10, G04BE03, G04BE08, G04BE09
#'
#' }
#'
#' @inheritParams all_stoppstart
#'
#' @inherit stopp_b1 return
#'
#' @export
stopp_b13b <- function(df, drug_string = "Drug_") {
  if (!any(grepl(colnames(df), pattern = drug_string))) {
    stop(paste0("No column names include ", drug_string,
                ". Change drug_string argument."))
  }


  # prelim_checks is a list of logical vectors, each has one entry per patient.
  prelim_checks <- list()
  # prelim_codes is a list of character vectors, each containing codes to check.
  prelim_codes <- list()

  # prelim_codes$drugs1 is a character vector of drug codes to check.
  prelim_codes$drugs1 <- c("C01DA")
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
  action_codes$drugs1 <- c("G04BE10", "G04BE03", "G04BE08", "G04BE09")
  # action_checks$drugs1 is TRUE if the patient is on any listed drugs.
  action_checks$drugs1 <- check_matches(df,
                                        column_string = drug_string,
                                        codes = action_codes$drugs1,
                                        match = "none")

  # all_actions is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'action_checks'.
  all_actions <- Reduce(x = action_checks, f = "&")


  output <- ifelse(all_prelims,
                   ifelse(all_actions, "Appropriate", "STOPP-B13b"),
                   "Not Relevant")

  return(output)
}

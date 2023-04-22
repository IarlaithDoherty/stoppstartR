#' @title Function to implement START-B1 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining START-B1.
#'
#' START-B1 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item Any of the following comorbidities:
#'
#' J40, J41, J42, J43, J44, J45, J46
#' \item None of the following drugs:
#'
#' R03AK04, R03AK05, R03AK06, R03AK07, R03AK08, R03AK09, R03AK10, R03AK11,
#' R03AL, R03AK12, R03AK13, R03AC, or R03BB.
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
start_b1 <- function(df, comorb_string = "Comorbidity_",
                     drug_string = "Drug_") {

  # prelim_checks is a list of logical vectors, each has one entry per patient.
  prelim_checks <- list()
  # prelim_codes is a list of character vectors, each containing codes to check.
  prelim_codes <- list()

  # prelim_codes$comorbs1 is a character vector of comorbidity codes to check.
  prelim_codes$comorbs1 <- c("J40", "J41", "J42", "J43", "J44", "J45", "J46")
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
  action_codes$drugs1 <- c("R03AK04", "R03AK05", "R03AK06", "R03AK07",
                           "R03AK08", "R03AK09", "R03AK10", "R03AK11",
                           "R03AL", "R03AK12", "R03AK13", "R03AC", "R03BB")
  # prelim_checks$drugs1 is TRUE if the patient is on any listed drugs.
  action_checks$drugs1 <- check_matches(df,
                                        column_string = drug_string,
                                        codes = action_codes$drugs1,
                                        match = "any")

  # all_actions is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'action_checks'.
  all_actions <- Reduce(x = action_checks, f = "&")


  output <- ifelse(all_prelims,
                   ifelse(all_actions, "Appropriate", "START-B1"),
                   "Not Relevant")

  return(output)
}



#' @title Function to implement START-B2 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining START-B2.
#'
#' START-B2 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item
#' }
#'
#' @param df Dataframe of patient information.
#' @param comorb_string Character string contained in the name of each
#'                      comorbidity column which uniquely identifies them.
#' @param drug_string Character string contained in the name of each drug
#'                    column which uniquely identifies them.
#'
#' @inherit start_b1 return
#'
#' @export
start_b2 <- function(df, comorb_string = "Comorbidity_",
                     drug_string = "Drug_") {

}


#' @title Function to implement START-B3 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining START-B3.
#'
#' START-B3 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item
#' }
#'
#' @param df Dataframe of patient information.
#' @param comorb_string Character string contained in the name of each
#'                      comorbidity column which uniquely identifies them.
#' @param drug_string Character string contained in the name of each drug
#'                    column which uniquely identifies them.
#'
#' @inherit start_b1 return
#'
#' @export
start_b3 <- function(df, comorb_string = "Comorbidity_",
                     drug_string = "Drug_") {

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title Function to implement START-C1 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining START-C1.
#'
#' START-C1 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item
#' }
#'
#' @param df Dataframe of patient information.
#' @param comorb_string Character string contained in the name of each
#'                      comorbidity column which uniquely identifies them.
#' @param drug_string Character string contained in the name of each drug
#'                    column which uniquely identifies them.
#'
#' @return  `all_checks`: logical vector,
#' `TRUE` if this STOPP/START criterion is satisfied, `FALSE` otherwise.
#'
#' @export
start_c1 <- function(df, comorb_string = "Comorbidity_",
                     drug_string = "Drug_") {

}


#' @title Function to implement START-C2 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining START-C2.
#'
#' START-C2 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item
#' }
#'
#' @param df Dataframe of patient information.
#' @param comorb_string Character string contained in the name of each
#'                      comorbidity column which uniquely identifies them.
#' @param drug_string Character string contained in the name of each drug
#'                    column which uniquely identifies them.
#'
#' @inherit start_c1 return
#'
#' @export
start_c2 <- function(df, comorb_string = "Comorbidity_",
                     drug_string = "Drug_") {

}


#' @title Function to implement START-C3 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining START-C3.
#'
#' START-C3 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item
#' }
#'
#' @param df Dataframe of patient information.
#' @param comorb_string Character string contained in the name of each
#'                      comorbidity column which uniquely identifies them.
#' @param drug_string Character string contained in the name of each drug
#'                    column which uniquely identifies them.
#'
#' @inherit start_c1 return
#'
#' @export
start_c3 <- function(df, comorb_string = "Comorbidity_",
                     drug_string = "Drug_") {

}


#' @title Function to implement START-C4 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining START-C4.
#'
#' START-C4 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item
#' }
#'
#' @param df Dataframe of patient information.
#' @param comorb_string Character string contained in the name of each
#'                      comorbidity column which uniquely identifies them.
#' @param drug_string Character string contained in the name of each drug
#'                    column which uniquely identifies them.
#'
#' @inherit start_c1 return
#'
#' @export
start_c4 <- function(df, comorb_string = "Comorbidity_",
                     drug_string = "Drug_") {


}


#' @title Function to implement START-C5 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining START-C5.
#'
#' START-C5 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item
#' }
#'
#' @param df Dataframe of patient information.
#' @param comorb_string Character string contained in the name of each
#'                      comorbidity column which uniquely identifies them.
#' @param drug_string Character string contained in the name of each drug
#'                    column which uniquely identifies them.
#'
#' @inherit start_c1 return
#'
#' @export
start_c5 <- function(df, comorb_string = "Comorbidity_",
                     drug_string = "Drug_") {

}


#' @title Function to implement START-C6 criterion.
#'
#' @description
#' Determine which patients triggered the conditions defining START-C6.
#'
#' START-C6 requires all of the following conditions to be satisfied:
#' \itemize{
#' \item
#' }
#'
#' @param df Dataframe of patient information.
#' @param comorb_string Character string contained in the name of each
#'                      comorbidity column which uniquely identifies them.
#' @param drug_string Character string contained in the name of each drug
#'                    column which uniquely identifies them.
#'
#' @inherit start_c1 return
#'
#' @export
start_c6 <- function(df, comorb_string = "Comorbidity_",
                     drug_string = "Drug_") {

}

#' #' @title Function to implement START-B1 criterion.
#' #'
#' #' @description
#' #' Determine which patients triggered the conditions defining START-B1.
#' #'
#' #' START-B1 requires all of the following conditions to be satisfied:
#' #' \itemize{
#' #'
#' #' }
#' #'
#' #' @param df Dataframe of patient information.
#' #' @param comorb_string Character string contained in the name of each
#' #'                      comorbidity column which uniquely identifies them.
#' #' @param drug_string Character string contained in the name of each drug
#' #'                    column which uniquely identifies them.
#' #'
#' #' @return  `all_checks`: logical vector,
#' #' `TRUE` if this STOPP/START criterion is satisfied, `FALSE` otherwise.
#' #'
#' #' @export
#' start_b1 <- function(df) {
#'
#' }
#'
#'
#' #' @title Function to implement START-B2 criterion.
#' #'
#' #' @description
#' #' Determine which patients triggered the conditions defining START-B2.
#' #'
#' #' START-B2 requires all of the following conditions to be satisfied:
#' #' \itemize{
#' #' \item
#' #' }
#' #'
#' #' @param df Dataframe of patient information.
#' #' @param comorb_string Character string contained in the name of each
#' #'                      comorbidity column which uniquely identifies them.
#' #' @param drug_string Character string contained in the name of each drug
#' #'                    column which uniquely identifies them.
#' #'
#' #' @inherit start_b1 return
#' #'
#' #' @export
#' start_b2 <- function(df) {
#'
#' }
#'
#'
#' #' @title Function to implement START-B3 criterion.
#' #'
#' #' @description
#' #' Determine which patients triggered the conditions defining START-B3.
#' #'
#' #' START-B3 requires all of the following conditions to be satisfied:
#' #' \itemize{
#' #' \item
#' #' }
#' #'
#' #' @param df Dataframe of patient information.
#' #' @param comorb_string Character string contained in the name of each
#' #'                      comorbidity column which uniquely identifies them.
#' #' @param drug_string Character string contained in the name of each drug
#' #'                    column which uniquely identifies them.
#' #'
#' #' @inherit start_b1 return
#' #'
#' #' @export
#' start_b3 <- function(df) {
#'
#' }
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'
#' #' @title Function to implement START-C1 criterion.
#' #'
#' #' @description
#' #' Determine which patients triggered the conditions defining START-C1.
#' #'
#' #' START-C1 requires all of the following conditions to be satisfied:
#' #' \itemize{
#' #' \item
#' #' }
#' #'
#' #' @param df Dataframe of patient information.
#' #' @param comorb_string Character string contained in the name of each
#' #'                      comorbidity column which uniquely identifies them.
#' #' @param drug_string Character string contained in the name of each drug
#' #'                    column which uniquely identifies them.
#' #'
#' #' @return  `all_checks`: logical vector,
#' #' `TRUE` if this STOPP/START criterion is satisfied, `FALSE` otherwise.
#' #'
#' #' @export
#' start_c1 <- function(df) {
#'
#' }
#'
#'
#' #' @title Function to implement START-C2 criterion.
#' #'
#' #' @description
#' #' Determine which patients triggered the conditions defining START-C2.
#' #'
#' #' START-C2 requires all of the following conditions to be satisfied:
#' #' \itemize{
#' #' \item
#' #' }
#' #'
#' #' @param df Dataframe of patient information.
#' #' @param comorb_string Character string contained in the name of each
#' #'                      comorbidity column which uniquely identifies them.
#' #' @param drug_string Character string contained in the name of each drug
#' #'                    column which uniquely identifies them.
#' #'
#' #' @inherit start_c1 return
#' #'
#' #' @export
#' start_c2 <- function(df) {
#'
#' }
#'
#'
#' #' @title Function to implement START-C3 criterion.
#' #'
#' #' @description
#' #' Determine which patients triggered the conditions defining START-C3.
#' #'
#' #' START-C3 requires all of the following conditions to be satisfied:
#' #' \itemize{
#' #' \item
#' #' }
#' #'
#' #' @param df Dataframe of patient information.
#' #' @param comorb_string Character string contained in the name of each
#' #'                      comorbidity column which uniquely identifies them.
#' #' @param drug_string Character string contained in the name of each drug
#' #'                    column which uniquely identifies them.
#' #'
#' #' @inherit start_c1 return
#' #'
#' #' @export
#' start_c3 <- function(df) {
#'
#' }
#'
#'
#' #' @title Function to implement START-C4 criterion.
#' #'
#' #' @description
#' #' Determine which patients triggered the conditions defining START-C4.
#' #'
#' #' START-C4 requires all of the following conditions to be satisfied:
#' #' \itemize{
#' #' \item
#' #' }
#' #'
#' #' @param df Dataframe of patient information.
#' #' @param comorb_string Character string contained in the name of each
#' #'                      comorbidity column which uniquely identifies them.
#' #' @param drug_string Character string contained in the name of each drug
#' #'                    column which uniquely identifies them.
#' #'
#' #' @inherit start_c1 return
#' #'
#' #' @export
#' start_c4 <- function(df) {
#'
#'
#' }
#'
#'
#' #' @title Function to implement START-C5 criterion.
#' #'
#' #' @description
#' #' Determine which patients triggered the conditions defining START-C5.
#' #'
#' #' START-C5 requires all of the following conditions to be satisfied:
#' #' \itemize{
#' #' \item
#' #' }
#' #'
#' #' @param df Dataframe of patient information.
#' #' @param comorb_string Character string contained in the name of each
#' #'                      comorbidity column which uniquely identifies them.
#' #' @param drug_string Character string contained in the name of each drug
#' #'                    column which uniquely identifies them.
#' #'
#' #' @inherit start_c1 return
#' #'
#' #' @export
#' start_c5 <- function(df) {
#'
#' }
#'
#'
#' #' @title Function to implement START-C6 criterion.
#' #'
#' #' @description
#' #' Determine which patients triggered the conditions defining START-C6.
#' #'
#' #' START-C6 requires all of the following conditions to be satisfied:
#' #' \itemize{
#' #' \item
#' #' }
#' #'
#' #' @param df Dataframe of patient information.
#' #' @param comorb_string Character string contained in the name of each
#' #'                      comorbidity column which uniquely identifies them.
#' #' @param drug_string Character string contained in the name of each drug
#' #'                    column which uniquely identifies them.
#' #'
#' #' @inherit start_c1 return
#' #'
#' #' @export
#' start_c6 <- function(df) {
#'
#' }

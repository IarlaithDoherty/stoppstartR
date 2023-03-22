#' @title Check which entries are true in each one of a list of logical vectors.
#'
#' @description
#' Combine `checks_list`, a list of logical vectors of the same length, into one
#' logical vector called `all_checks` using AND. Also create a character vector,
#' called `instruction` whose entries are `criterion_name` when `all_checks` is
#' `TRUE` and "" (blank) when `all_checks` is `FALSE`.
#'
#' @param checks_list A list of logical vectors of equal length.
#' @param criterion_name A character string giving a message for when all
#' corresponding entries across `checks_list` are `TRUE`.
#'
#' @return List with two elements:
#'
#' \itemize{
#' \item `all_checks`: logical vector,
#'
#' `TRUE` if all corresponding entries of `checks_list` are `TRUE`,
#' `FALSE` otherwise.
#' \item `instruction`: character vector,
#'
#' `criterion_name` if `all_checks` is `TRUE`, blank otherwise.
#' }
#' @export
combine_checks <- function(checks_list, criterion_name) {

  # 'all_checks' is a logical vector with one entry per patient.
  # TRUE if the patient is TRUE for each element of 'checks_list'.
  all_checks <- Reduce(x = checks_list, f = "&")
  # 'instruction' is a character vector with one entry per patient.
  # criterion_name if the patient is TRUE for 'all_checks', "" otherwise.
  instruction <- ifelse(all_checks, yes = criterion_name, no = "")

  return(list(all_checks = all_checks,
              instruction = instruction))
}

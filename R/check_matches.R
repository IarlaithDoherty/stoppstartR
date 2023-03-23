#' @title Check which patients contain code strings in specified columns.
#'
#' @description
#' Check which rows of `df` contain any codes in `codes` in the columns
#' starting with `column_string`.
#'
#' @param df Dataframe of patient information.
#' @param column_string
#'        Only columns with names containing this string will be checked.
#' @param codes
#'        A character vector of codes to search for.
#' @param match
#'        A character string:
#'        "any" to check for any matches, "none" for no matches.
#'
#' @return A logical vector indicating which rows / patients from `df` had an
#'         entry containing one of the codes in `codes` in any of the
#'         columns with names containing `column_string`.
#' @export
check_matches <- function(df, column_string, codes, match = "any") {

  # 'columns' is an integer vector.
  # These are the numbers of columns with names containing 'column_string'.
  columns <- grep(colnames(df), pattern = column_string)

  # Collapse the character vector, 'codes', into a single character string.
  # The codes are separated by "|", allowing grepl to check for any of them.
  codes_string <- paste(codes, collapse = "|")

  # 'column_checks' is a list of logical vectors, one per column.
  # Each vector has one TRUE / FALSE value per patient.
  # TRUE if that patient has any of the codes from 'codes' in that column.
  column_checks <- lapply(X = df[, columns],
                          FUN = function(x) {
                            grepl(x, pattern = codes_string)
                            }
                          )

  # 'combined_check' is a logical vector with a TRUE / FALSE value per patient.
  # TRUE if that patient has any TRUE values in 'column_checks'.
  combined_check <- Reduce(x = column_checks, f = "|")

  if (match == "any") {
    return(combined_check)
  } else if (match == "none") {
    return(!combined_check)
  } else {
    warning("match must be either 'any' or 'none'.")
  }
}
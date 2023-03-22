#' @title Check which patients contain code strings in specified columns.
#'
#' @description
#' Check which rows of `df` contain any codes in `code_string` in the columns
#' starting with `column_string`.
#'
#' @param df Dataframe of patient information.
#' @param column_string
#'        Only columns with names containing this string will be checked.
#' @param code_string
#'        A character string of codes to check matches for, separated by "|".
#'
#' @return A logical vector indicating which rows / patients from `df` had an
#'         entry containing one of the codes in `code_string` in any of the
#'         columns with names containing `column_string`.
#' @export
#'
#' @examples check_any_match(mock_patients,
#'                           column_string = "Drug_",
#'                           code_string = "C10AA")
check_any_match <- function(df, column_string, code_string) {

  # 'columns' is an integer vector.
  # These are the numbers of columns with names containing 'column_string'.
  columns <- grep(colnames(df), pattern = column_string)

  # 'column_checks' is a list of logical vectors, one per column.
  # Each vector has one TRUE / FALSE value per patient.
  # TRUE if that patient has any of the codes from 'code_string' in that column.
  column_checks <- lapply(X = df[, columns],
                                  FUN = function(x) {
                                    grepl(x, pattern = code_string)
                                  }
                          )

  # 'combined_check' is a logical vector with a TRUE / FALSE value per patient.
  # TRUE if that patient has any TRUE values in 'column_checks'.
  combined_check <- Reduce(x = column_checks, f = "|")

  return(combined_check)
}

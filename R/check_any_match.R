#' Check which rows of `df` contain any codes in `code_set` in the columns
#' starting with `column_string`.
#'
#' @param df Dataframe of patient information.
#' @param code_set A character string of codes to check matches for, separated by "|".
#' @param column_string
#'        Only columns with names containing this string will be checked.
#'
#' @return A logical vector indicating which rows / patients had a match in any column.
#' @export
#'
#' @examples check_any_match(mock_patients, column_prefix = "Drug_", code_set = "C10AA")
check_any_match <- function(df, column_prefix, code_set) {
  columns <- grep(colnames(df), pattern = column_prefix)
  column_checks <- lapply(X = df[, columns],
                                  FUN = function(x){
                                    grepl(x, pattern = code_set)
                                  }
    )
  combined_check <- Reduce(x = column_checks, f = "|")

  return(combined_check)
}

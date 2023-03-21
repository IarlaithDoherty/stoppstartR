#' Function to implement START-A5 rule.
#'
#' @param df Dataframe of patient information.
#'
#' @return List with two elements:
#'
#' `bool`: logical vector indicating whether each patient has triggered START-A5.
#'
#' `text`: character vector containing "START-A5" or "" for each patient.
#'
#' @export
#'
#' @examples start_a5(mock_patients)
start_a5_2 <- function(df) {

  # extras_tests[1] is a logical vector with one entry per patient.
  extras_check1 <- df$Age < 85

  comorbs_check1 <- check_any_match(df,
                                    column_prefix = "Comorbidity_",
                                    code_set = "I20|I21|I22|I24|I25")

  drugs_check1 <- !check_any_match(df,
                                  column_prefix = "Drug_",
                                  code_set = "C10AA")


  # bool is a logical vector with one entry per patient.
  # TRUE if that patient is TRUE for every condition.
  bool <- Reduce(x = list(extras_check1, comorbs_check1, drugs_check1),
                 f = "&")
  # text is a character vector with one entry per patient.
  # "START A5" if that patient is TRUE for every condition, "" (blank) otherwise.
  text <- ifelse(bool, yes = "START A5", no = "")
  # output is a named list consisting of bool and text.
  output <- list(bool = bool,
                 text = text)

  # This function will return the list called output.
  return(output)
}

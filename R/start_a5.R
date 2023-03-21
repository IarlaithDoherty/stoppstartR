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
start_a5 <- function(df) {

  # Numbers of comorbidity and drug columns used later to subset the dataframe..
  comorbs_cols <- grep(colnames(df), pattern = "Comorbidity_")
  drugs_cols   <- grep(colnames(df), pattern = "Drug_")

  # extras_cond1 is a logical vector with one entry per patient.
  extras_cond1 <- df$Age < 85

  # comorbs_set1 is a character string of comorbidity codes separated by "|" (or).
  # This allows grepl to check for matches with any of these codes.
  comorbs_set1 <- "I20|I21|I22|I24|I25"
  # comorbs_check1 is a list with one element per comorbidity column.
  # Each list element is a logical vector with one element per patient.
  # comorbs_check1 has 1 logical value per patient / column combination.
  # TRUE if that patient does have any of these comorbidities in that column.
  comorbs_check1 <- lapply(X = df[, comorbs_cols],
                           FUN = function(x){
                             grepl(x, pattern = comorbs_set1)
                             }
                           )
  # comorbs_cond1 is a logical vector with one entry per patient.
  # TRUE if that patient does have any of these comorbidities in any column.
  comorbs_cond1 <- Reduce(x = comorbs_check1, f = "|")

  # drugs_set1 is a character string of drug codes separated by "|" (or).
  # This allows grepl to check for matches with any of these codes.
  drugs_set1 <- "C10AA"
  # drugs_check1 is a list with one element per drug column.
  # Each list element is a logical vector with one element per patient.
  # drugs_check1 has 1 logical value per patient / column combination.
  # TRUE if that patient does not have any of these drugs in that column.
  drugs_check1 <- lapply(X = df[, drugs_cols],
                         FUN = function(x){
                           !grepl(x, pattern = drugs_set1)
                           }
                         )
  # drugs_cond1 is a logical vector with one entry per patient.
  # TRUE if that patient does not have any of these drugs in any column.
  drugs_cond1 <- Reduce(x = drugs_check1, f = "&")

  # bool is a logical vector with one entry per patient.
  # TRUE if that patient is TRUE for every condition.
  bool <- Reduce(x = list(extras_cond1, comorbs_cond1, drugs_cond1),
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

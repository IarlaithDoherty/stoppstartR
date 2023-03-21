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

  # Create empty lists to fill later.
  extras_tests <- list()
  comorbs_sets <- comorbs_checks <- comorbs_tests <- list()
  drugs_sets   <- drugs_checks   <- drugs_tests   <- list()

  # extras_tests[1] is a logical vector with one entry per patient.
  extras_tests[[1]] <- df$Age < 85

  # comorbs_sets is a list of character strings.
  # Each character string consists of comorbidity codes separated by "|" (or).
  # This allows grepl to check for matches with any of these codes.
  comorbs_sets[[1]] <- "I20|I21|I22|I24|I25"

  # comorbs_checks[[i]] is a list with one element per comorbidity column.
  # Each list element is a logical vector with one element per patient.
  # comorbs_checks[[i]] has 1 logical value per patient / column combination.
  # TRUE if that patient does have any of these comorbidities in that column.
  # comorbs_tests[[i]] is a logical vector with one entry per patient.
  # TRUE if that patient does have any of these comorbidities in any column.
  for(i in 1:length(comorbs_sets)) {
    comorbs_checks[[i]] <- lapply(X = df[, comorbs_cols],
                                  FUN = function(x){
                                    grepl(x, pattern = comorbs_sets[[i]])
                                  }
    )
    comorbs_tests[[i]] <- Reduce(x = comorbs_checks[[i]], f = "|")
  }

  # drugs_sets is a list of character strings.
  # Each character string consists of drug codes separated by "|" (or).
  # This allows grepl to check for matches with any of these codes.
  drugs_sets[[1]] <- "C10AA"

  # drugs_checks[[i]] is a list with one element per drug column.
  # Each list element is a logical vector with one element per patient.
  # drugs_checks[[i]] has 1 logical value per patient / column combination.
  # TRUE if that patient does not have any of these drugs in that column.
  # drugs_tests[[i]] is a logical vector with one entry per patient.
  # TRUE if that patient does not have any of these drugs in any column.
  for(i in 1:length(drugs_sets)) {
    drugs_checks[[i]] <- lapply(X = df[, drugs_cols],
                                FUN = function(x){
                                  !grepl(x, pattern = drugs_sets[[i]])
                                }
    )
    drugs_tests[[i]] <- Reduce(x = drugs_checks[[1]], f = "&")
  }

  # bool is a logical vector with one entry per patient.
  # TRUE if that patient is TRUE for every condition.
  bool <- Reduce(x = c(extras_tests, comorbs_tests, drugs_tests),
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

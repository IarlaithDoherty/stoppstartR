% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stopp_e.R
\name{stopp_e3}
\alias{stopp_e3}
\title{Implement STOPP-E3 criterion.}
\usage{
stopp_e3(df, gfr_column = "Lab Values: eGFR", drug_string = "Drug_")
}
\arguments{
\item{df}{data.frame: Patient information.}

\item{gfr_column}{Character string: The name of the patient GFR column.}

\item{drug_string}{Character string: Common text contained in the name of
each drug column which uniquely identifies them.}
}
\value{
A character vector with one entry per patient / row in \code{df}. Each
entry takes one of the following three values:
\itemize{
\item "Not Relevant" if the conditions are not satisfied.
\item "Appropriate" if the conditions are satisfied but the incorrect drugs
have not been prescribed.
\item The name of the criterion if the conditions are satisfied and the
incorrect drugs have been prescribed. That is, if this STOPP
criterion has been triggered.
}
}
\description{
Determine which patients triggered the conditions defining STOPP-E3.
}
\details{
STOPP-E3 requires all of the following conditions to be satisfied:
\itemize{
\item Any of the following drugs:

B01AF
\item GFR less than:

15
}
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stopp_e.R
\name{stopp_e6}
\alias{stopp_e6}
\title{Implement STOPP-E6 criterion.}
\usage{
stopp_e6(df, gfr_column = "Lab Values: eGFR", drug_string = "Drug_")
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
Determine which patients triggered the conditions defining STOPP-E6.
}
\details{
STOPP-E6 requires all of the following conditions to be satisfied:
\itemize{
\item Any of the following drugs:

A10BA02, A10BD02, A10BD03, A10BD05, A10BD07, A10BD08, A10BD10, A10BD11,
A10BD13, A10BD14, A10BD15, A10BD16, A10BD17, A10BD18, A10BD22

\item GFR less than:

30
}
}

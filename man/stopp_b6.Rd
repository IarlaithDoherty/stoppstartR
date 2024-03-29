% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stopp_b_one.R
\name{stopp_b6}
\alias{stopp_b6}
\title{Implement STOPP-B6 criterion.}
\usage{
stopp_b6(df, comorb_string = "Comorbidity_", drug_string = "Drug_")
}
\arguments{
\item{df}{data.frame: Patient information.}

\item{comorb_string}{Character string: Common text contained in the name of
each comorbidity column which uniquely identifies them.}

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
Determine which patients triggered the conditions defining STOPP-B6.
}
\details{
STOPP-B6 requires all of the following conditions to be satisfied:
\itemize{
\item Any of the following comorbidities:

I10, I15
\item None of the following comorbidities:

I50, I11.0, I13.0, I13.2
\item None of the following drugs:

C07, C08, C09, C03A, C03EA
\item Any of the following drugs:

C03C, C03EB
}
}

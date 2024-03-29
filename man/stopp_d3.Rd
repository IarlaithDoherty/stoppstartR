% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stopp_d_one.R
\name{stopp_d3}
\alias{stopp_d3}
\title{Implement STOPP-D3 criterion.}
\usage{
stopp_d3(df, comorb_string = "Comorbidity_", drug_string = "Drug_")
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
Determine which patients triggered the conditions defining STOPP-D3.
}
\details{
STOPP-D3 requires all of the following conditions to be satisfied:
\itemize{
\item Any of the following comorbidities:

N40, R33
\item Any of the following drugs:

N05AA01, N05AH02, N05AF01, N05AB02, N05AC04, N05AA03, N05AF05
}
}

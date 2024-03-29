% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/start_bc.R
\name{start_b1}
\alias{start_b1}
\title{Implement START-B1 criterion.}
\usage{
start_b1(df, comorb_string = "Comorbidity_", drug_string = "Drug_")
}
\arguments{
\item{df}{data.frame: Patient information.}

\item{comorb_string}{Character string: Common text contained in the name of
each comorbidity column which uniquely identifies them.}

\item{drug_string}{Character string: Common text contained in the name of
each drug column which uniquely identifies them.}
}
\value{
A character vector with one entry per patient / row in \code{df}. Each entry takes one of the following three values:
\itemize{
\item "Not Relevant" if the conditions are not satisfied.
\item "Appropriate" if the conditions are satisfied but the correct medications have been prescribed.
\item "START-A1" if the conditions are satisfied and the correct medications have not been prescribed. That is, if START-A1 has been triggered.
}
}
\description{
Determine which patients triggered the conditions defining START-B1.
}
\details{
START-B1 requires all of the following conditions to be satisfied:
\itemize{
\item Any of the following comorbidities:

J40, J41, J42, J43, J44, J45, J46
\item None of the following drugs:

R03AK04, R03AK05, R03AK06, R03AK07, R03AK08, R03AK09, R03AK10, R03AK11,
R03AL, R03AK12, R03AK13, R03AC, or R03BB.
}
}

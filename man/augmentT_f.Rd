% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/augmentT_f.R
\name{augmentT_f}
\alias{augmentT_f}
\title{Creates cell marginal max and percentages.}
\usage{
augmentT_f(.data, variablesmax, variablespct, verbose = getOption("verbose"))
}
\arguments{
\item{.data}{a dataframe}

\item{variablesmax}{a vector of character strings}

\item{variablespct}{a vector of character strings}
}
\value{
a dataframe.
}
\description{
Creates cell marginal max and percentages.
}
\details{
applys the functions augmentmaxT_f and augmentpctT_f to .data
}
\examples{
.data=BigSyn::STtableA1
variablesmax="AA.present";variablespct="AA.cont1"
ASTtableA1<-augmentT_f(.data,variablesmax,variablespct,verbose=F)
ASTtableA1[c("AA.present_","AA.cont1_","AA.present_La","AA.cont1_La","AA.present_La_Lb","AA.cont1_La_Lb")]
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ch12-fn.R
\name{cross.test}
\alias{cross.test}
\title{Test of a Cross Table}
\usage{
cross.test(x, v1, v2, alp = 0.05, dig = 2, dig2 = 4, prt = TRUE)
}
\arguments{
\item{x}{Cross Table}

\item{v1}{Row variable name (optional)}

\item{v2}{Column variable name (optional)}

\item{alp}{Level of significance, Default: 0.05}

\item{dig}{Number of digits below the decimal point for the table, Default: 2}

\item{dig2}{Number of digits below the decimal point for the results, Default: 4}

\item{prt}{Logical, whether to print tables in detail, Default: TRUE}
}
\value{
list(stat=test stat, df=df, crv=critical value, pv=p-value, exp=exp table, ress=residual square)
}
\description{
Test of Homogeniety/Independence from a Cross Table
}
\examples{
data(exa12_7)
x=exa12_7
rownames(x)=c("Kor", "Eng", "Math", "Etc")
colnames(x)=c("Sam", "Pub", "Exp", "Sal", "Etc")
cross.test(x)
}

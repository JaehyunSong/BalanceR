\name{summary.BalanceR}
\alias{summary.BalanceR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Diplay Summary of "BalanceR" Objects
}
\description{
Diplay Summary of "BalanceR" Objects.

This method is identical to print.BalanceR with only.SB = TRUE, abs = TRUE, simplify = TRUE.
}
\usage{
\method{summary}{BalanceR}(object, digits = 3, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An object of class 'BalanceR'}
  \item{digits}{Digits}
  \item{\dots}{Ignored.}
}
\author{Jaehyun Song <http://www.jaysong.net>}
\seealso{\code{\link{BalanceR}}, \code{\link{print.BalanceR}}}
\examples{
\dontrun{
## S3 method for class 'BalanceR'
data(BlcDF)

BlcChk <- BalanceR(BlcDF, group = "Group",
                   cov = c(Sex, Age, Educ, Marriage))

## Display the result with 3 digits.
summary(BlcChk, digits = 3)
}
}

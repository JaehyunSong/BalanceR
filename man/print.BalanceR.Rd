\name{print.BalanceR}
\alias{print.BalanceR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Print "BalanceR" Objects
}
\description{
Print "BalanceR" Objects
}
\usage{
\method{print}{BalanceR}(
  x,
  only.SB = FALSE,
  simplify = FALSE,
  abs = FALSE,
  digits = 3,
  ...
  )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{An object of class 'BalanceR'}
  \item{only.SB}{Display only standardized biases. Default is FALSE.}
  \item{simplify}{Display maximum standardized biases. When number of pairs is large, I recommend set this argument TRUE. Default is FALSE.}
  \item{abs}{Display absolute values of standardized biases. Default is FALSE.}
  \item{digits}{digits}
  \item{\dots}{Ignored.}
}
\author{Jaehyun Song <http://www.jaysong.net>}
\seealso{\code{\link{BalanceR}}, \code{\link{plot.BalanceR}}}
\examples{
\dontrun{
## S3 method for class 'BalanceR'
data(BlcDF)

BlcChk <- BalanceR(BlcDF, group = "Group",
                   cov = c(Sex, Age, Educ, Marriage))

## Display the result with 3 digits.
print(BlcChk, digits = 3)

## Display only standardized biases with 4 digits.
print(BlcChk, only.SB = TRUE, digits = 4)

## Display only absolute values of maximum standardized biases.
print(BlcChk, only.SB = TRUE, abs = TRUE, simplify = TRUE)

# A pipe operator (\%>\%) is also available
library(magrittr)

BlcChk \%>\%
    BalanceR(group = Group, cov = c(Sex, Age, Educ, Marriage)) \%>\%
    print(digits = 2)
}
}

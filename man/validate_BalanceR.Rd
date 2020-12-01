\name{validate_BalanceR}
\alias{validate_BalanceR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Validator for "BalanceR" Objects
}
\description{
Validator for "BalanceR" Objects
}
\usage{
validate_BalanceR(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{An object of class 'BalanceR'}
}
\author{Jaehyun Song <http://www.jaysong.net>}
\seealso{\code{\link{BalanceR}}}
\examples{
\dontrun{
## S3 method for class 'BalanceR'
data(BlcDF)

BlcChk <- BalanceR(BlcDF, group = "Group",
                   cov = c(Sex, Age, Educ, Marriage))

## Validate if BlcChk object is "BalanceR" object.
validate_BalanceR(BlcChk)
}
}

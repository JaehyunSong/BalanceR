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
print.BalanceR(data, only.SB = FALSE, digits = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{An object of class "BalanceR"}
  \item{only.SB}{Display only standardized biases. Default is FALSE.}
  \item{digits}{digits}
}
\author{Jaehyun Song <http://www.jaysong.net>}
\seealso{\code{\link{BalanceR}}, \code{\link{plot.BalanceR}}}
\examples{
\dontrun{
data(BlcDF)

BlcChk <- BalanceR(BlcDF, group = "Group",
                   cov = c("Sex", "Age", "Educ", "Marriage"))

print(BlcChk, digits = 3)

print(BlcChk, only.SB = TRUE, digits = 4)

# Pipe operator is also available
BlcChk %>%
    BalanceR(group = "Group", cov = c("Sex", "Age", "Educ", "Marriage")) %>%
    print(digits = 2)
}
}

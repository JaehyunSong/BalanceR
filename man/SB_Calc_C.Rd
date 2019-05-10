\name{SB_Calc_C}
\alias{SB_Calc_C}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculate Standardized Bias for Continuous Variables}
\usage{SB_Calc_C(x, y)}
\description{Calculate Standardized Bias for Continuous Variables}
\arguments{
  \item{x}{A numeric vector}
  \item{y}{A numeric vector}
}
\author{
Jaehyun Song <http://www.jaysong.net>
}
\seealso{\code{\link{BalanceR}}}
\examples{
\dontrun{
data(BlcDF)

# Standardized bias of "Age" variable between "Control" and "Treat1" group.
SB_Calc_C(BlcDF$Age[BlcDF$Group == "Control"],
          BlcDF$Age[BlcDF$Group == "Treat1"])
}
}

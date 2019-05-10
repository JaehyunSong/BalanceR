\name{SB_Calc_B}
\alias{SB_Calc_B}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculate Standardized Bias for Binary Variables}
\usage{
    SB_Calc_B(x, y)
}
\description{Calculate Standardized Bias for Binary Variables}
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

# Standardized bias of "Sex" variable between "Control" and "Treat1" group.
SB_Calc_B(BlcDF$Sex[BlcDF$Group == "Control"],
          BlcDF$Sex[BlcDF$Group == "Treat1"])
    }
}

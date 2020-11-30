\name{plot.BalanceR}
\alias{plot.BalanceR}
\title{
Plot "BalanceR" Object
}
\description{
Plot "BalanceR" Object
}
\usage{
\method{plot}{BalanceR}(
  x,
  point.size = 2.5,
  text.size = 12,
  vline = c(10, 25),
  color = TRUE,
  simplify = FALSE,
  abs = FALSE,
  ...
  )
}
\arguments{
  \item{x}{An object of class "BalanceR"}
  \item{point.size}{Size of points in the plot. Default is 2.5.}
  \item{text.size}{Size of texts in the plot. Default is 12.}
  \item{vline}{Numeric vector denoting x-intercept of vertical lines. Default is \code{c(10, 25)}}
  \item{color}{If \code{color = FALSE}, display a plot in black and white. Default is TRUE.}
  \item{simplify}{Display maximum standardized biases. When number of pairs is large, I recommend set this argument TRUE. Default is FALSE.}
  \item{abs}{Display absolute values of standardized biases. Default is FALSE.}
  \item{\dots}{Ignored.}
}
\author{Jaehyun Song <http://www.jaysong.net>}
\seealso{\code{\link{BalanceR}}, \code{\link{plot.BalanceR}}}
\examples{
\dontrun{
## S3 method for class 'BalanceR'
library(BlcDF)

## Show a plot
BlcChk <- BlcDF \%>\%
    BalanceR(group = Group,
             cov   = c(Sex, Age, Educ, Marriage))

plot(BlcChk)

## A pipe operator (\%>\%) is also available.
library(magrittr)
BlcChk \%>\%
    plot()

## Show a plot with vertical lines
BlcChk \%>\%
    plot(vline = c(3, 5, 10))

## Change text and point sizes
BlcChk \%>\%
    plot(point.size = 3, text.size = 12)

## Display only absolute values of maximum standardized biases.
BlcChk \%>\%
    plot(only.SB = TRUE, abs = TRUE, simplify = TRUE)

## Display black-white plot
BlcChk \%>\%
    plot(color = FALSE)
}
}

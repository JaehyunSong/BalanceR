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
  compare = NULL,
  names = NULL,
  ...
  )
}
\arguments{
  \item{x}{An object of class "BalanceR"}
  \item{point.size}{Size of points in the plot. Default is 2.5.}
  \item{text.size}{Size of texts in the plot. Default is 12.}
  \item{vline}{Numeric vector denoting x-intercept of vertical lines. Default is \code{c(10, 25)}}
  \item{color}{If \code{color = FALSE}, display a plot in black and white. Default is TRUE.}
  \item{simplify}{Display maximum standardized biases. When number of pairs is large, I recommend set this argument TRUE. Default is FALSE. If \code{compare} is not NULL, this argument is ignored.}
  \item{abs}{Display absolute values of standardized biases. Default is FALSE.}
  \item{compare}{An object of class "BalanceR". Default is NULL.}
  \item{names}{Name of objects \code{x} and {after}. Default is NULL. If \code{compare} is not NULL, default is \code{c("Sample 1", "Sample 2")}.}
  \item{\dots}{Ignored.}
}
\author{Jaehyun Song <http://www.jaysong.net>}
\seealso{\code{\link{BalanceR}}, \code{\link{print.BalanceR}}}
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

## Compare two BalanceR objects
BlcChk1 <- BlcDF[1:1000, ] \%>\%
    BalanceR(group = Group,
             cov   = c(Sex, Age, Educ, Marriage))

BlcChk2 <- BlcDF[1001:2000, ] \%>\%
    BalanceR(group = Group,
             cov   = c(Sex, Age, Educ, Marriage))

plot(BlcChk1, compare = BlcChk2,
     names = c("First 1000 Sample", "Second 1000 Sample"),
     abs = TRUE, point.size = 3, color = FALSE)
}
}

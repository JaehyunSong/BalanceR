\name{BalanceR}
\alias{BalanceR}
\title{Balance Check with Standardized Bias}
\usage{
BalanceR(data, group, cov)
}
\arguments{
    \item{data}{Default dataset to use for balance check}
    \item{group}{Default character which indicates experimental group}
    \item{cov}{A vector of covariates name}
}
\description{
Balance check using standardized bias. If SBs are under 3, 5, or 10, you can interprete covariates are balanced between experimental groups.
}
\examples{
\dontrun{
# Load a dummy dataset
data(BlcDF)

# Balance check of respondents' gender, age, education level, and marital status.
# A group indicator variable is "Group"
BlcDF \%>\%
    BalanceR(group = "Group",
             cov   = c("Sex", "Age", "Educ", "Marriage"))

## Define new names of covariates
BlcDF \%>\%
    BalanceR(group = "Group",
             cov   = c("Gender" = "Sex", "Age", "Educ", "Marriage"))

## Show a result with 4-digits
BlcDF \%>\%
    BalanceR(group = "Group",
             cov   = c("Sex", "Age", "Educ", "Marriage")) \%>\%
    print(digits = 4)

## Show only standardized biases
BlcDF \%>\%
    BalanceR(group = "Group",
             cov   = c("Sex", "Age", "Educ", "Marriage")) \%>\%
    print(only.SB = TRUE, digits = 3)

## Show a plot
BlcDF \%>\%
    BalanceR(group = "Group",
             cov   = c("Sex", "Age", "Educ", "Marriage")) \%>\%
    plot()

## Show a plot with vertical lines
BlcDF \%>\%
    BalanceR(group = "Group",
             cov   = c("Sex", "Age", "Educ", "Marriage")) \%>\%
    plot(vline = c(3, 5, 10))

## Change text and point sizes
BlcDF \%>\%
    BalanceR(group = "Group",
             cov   = c("Sex", "Age", "Educ", "Marriage")) \%>\%
    plot(point.size = 3, text.size = 12)

## Display black-white plot
BlcDF \%>\%
    BalanceR(group = "Group",
             cov   = c("Sex", "Age", "Educ", "Marriage")) \%>\%
    plot(color = FALSE)
}
}

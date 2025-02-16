% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/boxplot.drg.R
\name{boxplox.drg}
\alias{boxplox.drg}
\title{Boxplot of Payments by DRG Code}
\usage{
boxplox.drg(df, option = "Average.Medicare.Payments")
}
\arguments{
\item{df}{a dataframe of DRG code}

\item{option}{a string name for variable x in the dataframe df}
}
\value{
A boxplot
}
\description{
Boxplot of Payments by DRG Code
}
\examples{
drg <- read.scv('data/DRG_data.csv')
boxplox.drg(drg, 'Average.Medicare.Payments')

}

\name{lab6-package}
\alias{lab6-package}
\alias{lab6}
\docType{package}
\title{
\packageTitle{lab6}
}
\description{
\packageDescription{lab6}
}
\details{

This package contains three different functions for solving the knapsack problem: 

\enumerate{

\item \code{brute_force_knapsack(x,W)} : goes through all possible combinations of item and return the maximum value found;
\item \code{knapsack_dynamic(x,W)} : iterates over all possible values of w to find the optimum;
\item \code{greedy_knapsack(x,W)} : sorts the items of decreasing order of value per unit of weight and then it proceeds to insert them into the sack until there is not more room in the sack.

}

All three functions take two elements: 

\itemize{

\item a dataframe containing the values and the weights of each item (\code{x});
\item the knapsack maximum weight (\code{W}).

}

All of them return the maximum value they found and the elements choosen to reach that value. 

}
\author{
\packageAuthor{lab6}

Maintainer: \packageMaintainer{lab6}
}
\references{
\url{ https://en.wikipedia.org/wiki/Knapsack_problem }
}

\keyword{ package }

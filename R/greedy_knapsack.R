###RUN to check the time:
#system.time(greedy_knapsack(x = knapsack_objects[1:1000000,], W = 3500))

#'
#' Greedy heuristic in knapsack problem
#' 
#' 
#' @param x A dataframe with two columns: the values (v) and the weights (w) of each item to put in the knapsack.
#' @param W A positive number representing the knapsack size.
#' @return A list of two elements: a positive number with the maximum knapsack \code{value} and a vector of all the \code{elements} in the knapsack size.
#' @examples
#' set.seed(42)
#' n <- 2000
#' knapsack_objects <-data.frame(w=sample(1:4000, size = n, replace = TRUE),v=runif(n = n, 0, 10000))
#' greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
#' @references \url{ https://en.wikipedia.org/wiki/Knapsack_problem#Greedy_approximation_algorithm }
#'@export greedy_knapsack


greedy_knapsack<- function(x,W){
  
    if(!is.data.frame(x)){
      stop("x must be a dataframe")
    }
    if(any(x < 0,na.rm = TRUE)){
      stop("x must contain positive values")
    }
    if(!(length(x)==2)){
      stop("x must have two columns")
    }
    if(!(all(names(x)==c("w","v")))){
      stop("x columns' names must be 'v' and 'w'")
    }
    if(!(W>=0 && length(W)==1 && is.numeric(W))){
      stop("W must be one positive numeric value")
    }
  
  n<-dim(x)[1]
  
  x$weight<-x$v/x$w
  value<-0
  elements<-rep(0,n)
  k<-1
  
  #we find the max value of the weights, we take the position so calculate the sum and to save the elements we are adding
  while((sum(x$w[elements])+x$w[which.max(x$weight)])<=W && any(x$weight>0)){
    i<-which.max(x$weight)
    value<-value+x$v[i] 
    elements[k]<-i
    x$weight[i]<-0 
    k<-k+1
  }
  
  value<-round(value)
  elements<-elements[which(elements>0)]
  
  values<-list(value=value,elements=elements)
  
  return(values)
}



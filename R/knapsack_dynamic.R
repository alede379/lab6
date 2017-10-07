###RUN to check the time:
#system.time(knapsack_dynamic(x = knapsack_objects[1:500,], W = 3500))
#lineprof(knapsack_dynamic(x = knapsack_objects[1:500,], W = 3500))


#'
#' Dynamic programming in in knapsack problem
#' 
#' 
#' @param x A dataframe with two columns: the values (v) and the weights (w) of each item to put in the knapsack.
#' @param W A positive number representing the knapsack size.
#' @return A list of two elements: a positive number with the maximum knapsack \code{value} and a vector of all the \code{elements} in the knapsack size.
#' @examples
#' set.seed(42)
#' n <- 2000
#' knapsack_objects <-data.frame(w=sample(1:4000, size = n, replace = TRUE),v=runif(n = n, 0, 10000))
#' knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)
#' @references \url{ https://en.wikipedia.org/wiki/Knapsack_problem#0.2F1_knapsack_problem }
#'@export knapsack_dynamic


knapsack_dynamic<-function(x,W){
  
    if(!is.data.frame(x)){
      stop("x must be a dataframe")
    }
    if(any(x<=0)){
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
    m<-matrix(ncol=W+1,nrow=n+1) #matrix of alg.
    m[1,]<-rep(0,W+1)
    val<-x$v
    wei<-x$w
    
    #building m[i,j] and looking for the greatest sum lower than W
    for(i in 1:n){    
      for(j in 0:W){
        if(wei[i] > j){
          m[i+1,j+1]<-m[i,j+1]
        }else{
          m[i+1,j+1]<-max(m[i,j+1],m[i,j+1-wei[i]]+val[i])
        }
      }
    }
    
  #looking for the elements from the sum
    
  j=j+1  
  i<-which.max(m[,j]) #row selected is the one of the first element selected
  elements<-length(n)
  k<-1
  elements[k]<-i-1
  
  while(m[i,j]!=0 && j!=1 && i!=0){
    k<-k+1
    j<-(j-wei[i-1])
    i<-which(m[,j] == m[i-1,j])[1]
    elements[k]<-i-1
  }
  
  value<-round(m[n+1,W+1])
  elements<-sort(elements[which(elements>0)])
  
  values<-list(value=value,elements=elements)  
  
  return(values)
}



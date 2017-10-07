###RUN to create the object
#set.seed(42)
#n <- 2000
#knapsack_objects <-data.frame(w=sample(1:4000, size = n, replace = TRUE),v=runif(n = n, 0, 10000))
#greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)

###RUN to check the time:
#system.time(greedy_knapsack(x = knapsack_objects[1:1000000,], W = 3500))

#'@export greedy_knapsack
greedy_knapsack<- function(x,W){
  stopifnot((is.data.frame(x)==TRUE) && x>0 && x$w>0 && is.numeric(x$w)==TRUE && is.numeric(x$v)==TRUE && is.numeric(W)==TRUE && length(x)==2 && names(x)==c("w","v") &&  W>0) 
  
  n<-dim(x)[1]
  
  x$weight<-x$v/x$w
  value<-0
  elements<-rep(0,n)
  k<-1
  
  #we find the max value of the weights, we take the position so calculate the sum and to save the elements we are adding
  while((sum(x$w[elements])+x$w[which.max(x$weight)])<W && x$weight>0){
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



#this function takes a data.frame with two variables v and w 
#and returns the maximum knapsack value and which elements (rows in the data.frame)
#W is the knapsack size

#data to be used
set.seed(42)
n <- 2000
knapsack_objects <-data.frame(w=sample(1:4000, size = n, replace = TRUE),v=runif(n = n, 0, 10000))


knapsack_brute_force<-function(x,W){
  stopifnot((is.data.frame(x)==TRUE) && x>0 && length(x)==2 && names(x)==c("w","v") && W>=0) #check if the input are correct
  
  while(sum<=W){
    
    
  }
  
  return(value,elements)
}

#set.seed(42)
#n <- 2000
#knapsack_objects <-data.frame(w=sample(1:4000, size = n, replace = TRUE),v=runif(n = n, 0, 10000))
#knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)

### ---> TIME OF ESECUTION
#install.packages("tictoc")
#require(tictoc)
#tic()
#knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)
#exectime <- toc()
#exectime <- exectime$toc - exectime$tic

knapsack_dynamic<-function(x,W){
  
  #check if the input are correct
  stopifnot((is.data.frame(x)==TRUE) && x>0 && x$w>0 && is.numeric(x$w)==TRUE && is.numeric(x$v)==TRUE && is.numeric(W)==TRUE && length(x)==2 && names(x)==c("w","v") &&  W>0) 
    
    n<-dim(x)[1]
    m<-matrix(ncol=W+1,nrow=n+1) #matrix of alg.
   
    
    for(j in 0:(W)){
      m[1,j+1]<-0
    }
    
    #building m[i,j] and looking for the greatest sum lower than W
    
    for(i in 1:n){    
      for(j in 0:W){
        if(x$w[i] > j){
          m[i+1,j+1]<-m[i,j+1]
        }else{
          m[i+1,j+1]<-max(m[i,j+1],m[i,j+1-x$w[i]]+x$v[i])
        }
      }
    }
    
  #looking for the elements from the sum
    
  j=j+1  
  i<-which.max(m[,j])
  i<-i-1
  elements<-length(n)
  k<-1
  elements[k]<-i
  
  while(m[i,j]!=0 && j!=1 && i!=0){
    k<-k+1
    j<-(j-x$w[i])
    opt<-m[i,j]
    i<-which(m[,j]==opt)[1]
    elements[k]<-i-1
  }
  
  value<-round(m[n+1,W+1])
  elements<-sort(elements[which(elements>0)])
  
  values<-list(value=value,elements=elements)  
  
  return(values)
}



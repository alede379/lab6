#this function takes a data.frame with two variables v and w 
#and returns the maximum knapsack value and which elements (rows in the data.frame)
#W is the knapsack size

###RUN to create the object:
#set.seed(42) 
#n <- 2000
#knapsack_objects <-data.frame(w=sample(1:4000, size = n, replace = TRUE),v=runif(n = n, 0, 10000))

###RUN to check time:
#system.time(brute_force_knapsack(x = knapsack_objects[1:16,], W = 3500))
#lineprof(knapsack_brute_force(x = knapsack_objects[1:16,], W = 3500))

brute_force_knapsack<-function(x,W,parallel=FALSE){
  
  stopifnot((is.data.frame(x)==TRUE) && x>0 && length(x)==2 && names(x)==c("w","v") && W>=0) #check if the input are correct
  #if parallel=TRUE it should parallelize over the detected cores
        maxvalue<-0
        value<-0
        n<-dim(x)[1]
        elements<-length(n)
  
  if(parallel==TRUE){
    
    cores <- parallel::detectCores()
    
    cl <- parallel::makeCluster(cores, type = "PSOCK")
    
    comb_p <- parallel::parLapply(cl,maxvalue,value,n,elements,function(elements,maxvalue){
                          for(i in 1:n)
                          {
                            comb<-combn(n,i) #all possible combination of i from 1 to n
                            
                            j<-1
                            while(j<=ncol(comb)){ 
                              
                              if(sum(x$w[comb[,j]])<=W){
                                value<-sum(x$v[comb[,j]])
                                if(maxvalue<value){
                                  elements<-comb[,j] #save elements of that combination
                                  maxvalue<-value #save the max value you found
                                }
                              }
                              j<-j+1
                            }
                            
                          }
                        return(list(elements,maxvalue))
        }
    )
    
    value<-round(comb_p[2])
    values<-list(value=value,elements=comb_p[1])
    
    parallel::stopCluster(cl)

    }else{

        for(i in 1:n){
          comb<-combn(n,i) #all possible combination of i from 1 to n
          
          j<-1
          while(j<=ncol(comb)){ 
            
            if(sum(x$w[comb[,j]])<=W){
              value<-sum(x$v[comb[,j]])
              if(maxvalue<value){
                elements<-comb[,j] #save elements of that combination
                maxvalue<-value #save the max value you found
              }
            }
            j<-j+1
          }
        }
        
    }
  
  value<-round(maxvalue)
  values<-list(value=value,elements=elements)
  return(values)
  
}




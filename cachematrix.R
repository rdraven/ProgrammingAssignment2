##The makeCacheMatrix function initializes the list with cache functions
##Receives as parameter the matrix to be processed

makeCacheMatrix<-function(x=matrix()){
     
           #initial cache data is set to NULL
               m<-NULL                             
                          
           #the get function will return data to be processed             
               get<-function() x
               
           #the setInv function will be called to store processed data to cache
               setInv<-function(inv) m<<-inv

           #the getInv function will be called to return cached data
               getInv<-function() m

           #return the list with the functions 
               list(get=get, setInv=setInv, getInv=getInv)
}


##Function to return the inverse matrix
##The function receives as argument the list output of the makeCacheMatrix function
cacheSolve<-function(x=matrix(),...){

      #Retrieve the cached data          
          m<-x$getInv()

      #If the cached data is not NULL than no further processing is required
          if(!is.null(m)){
             message("getting cached data")
      #The results are returned       
             return(m) 
          }

          #If the cached data is NULL, the matrix to be processed is retrieved   
          data<-x$get()

          #the matrix is processed, solve() function performs inversion
          m<-solve(data, ...)

          #the result is cached
          x$setInv(m)

          #the result is displayed
          m
}
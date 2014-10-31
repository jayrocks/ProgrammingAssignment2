## The set and get functions are for setting and obtaining the values of the cached inverse matrix of x...
## The setMatrix and getMatrix functions are for setting and obtaining the value of the inverse matrix of x

## Generating the cached inverse matrix of x.....and then obtaining the value of the same...
## Setting and obtaining the value of the inverse matrix of x.....

makeCacheMatrix <- function(x = matrix()) {

invMat <- NULL  # invMat....is the Inverse Matrix
  
  set <- function(y) {
    x <<- y
    invMat <<- NULL
    return(x)
  }
  
  get <- function(){
     x
  }
  
  setMatrix <- function(inverseMat){
    m <<- inverseMat
  }  
    
  getMatrix <- function(){
    m
  }  
    
  list(set = set, get = get,
       setmean = setMatrix,
       getmean = getMatrix)

}


## Check if the cached value of the inverse of the matris of x is available then, getting and returning tat value...or else the creation of the new cached value of the inverse matrix of x.....

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
         m <- x$getMatrix()
  
  if(!is.null(m)) {
    message("getting cached Inverse Matrix")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...) ## Here the Solve function is used for finding the Inverse Matrix of matrix x......
  x$setMatrix(m)
  m
}

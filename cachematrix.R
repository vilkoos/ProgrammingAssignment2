####################################################################
## 
##   cacheSolve()
##   - takes an invertible matrix as input
##     - if in cache, retrieves the inverse matrix =
##     - if not in cache, calculates and caches the inverse matrix
##   returns the inverse
## 
##   makeCacheMatrix() 
##   is a helper function for cacheSolve, used to
##   - remember which matrix is inverted
##   - remember the calculated inverse
## 
##   together these functions prevent the necessity 
##                              to repeatedly calculate an inverse
##
####################################################################

#-------------------------------------------------------------------
##   makeCacheMatrix is a helper function for cacheSolve, used to
##   - remember which original matrix is inverted (setOrg, getOrg)
##   - remember the calculated inverse (setImat, get Imat)
#-------------------------------------------------------------------
makeCacheMatrix <- function(org_mat = matrix()) {
    # -- create empty local vars  ----------
    inv_mat <- NULL
    org_mat <- NULL    
    #---- source setOrg, getOrg, setImat and getImat funtions 
    setOrg <- function(y) {
        org_mat <<- y    # remember original matrix
        inv_mat <<- NULL # resets inverse matrix
        return(NULL)
    }
    #---------------------------------------------
    getOrg <- function() { return(org_mat) }
    #---------------------------------------------
    setImat <- function(inverse_matrix) {
        inv_mat <<- inverse_matrix
        return(NULL)
    }
    #---------------------------------------------
    getImat <- function(){
        return(inv_mat)
    }
    #-- build and return a list of references to functions-------------
    func_refs <- list(set = setOrg, get = getOrg, setInv = setImat, getInv = getImat)
    return(func_refs)
}

#-------------------------------------------------------------------
##   cacheSolve()
##   - takes an invertible matrix as input
##     - if in cache retrieves the inverse matrix =
##     - if not in cache calculates and caches the inverse matrix
##   returns the inverse
#-------------------------------------------------------------------
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv_mat <- x$getInv()
    if(!is.null(inv_mat)) {
        message("getting cached data")
    } else {
        org_mat <- x$get()
        inv_mat <- solve(org_mat)
        x$setInv(inv_mat)
    }
    return(inv_mat)
}

##################################################################
#
# test case, demonstrating usage
#
##################################################################

#--- create a matrix ti invert ------------
mat <- c(1,5,1,2,2,2,1,2,3)
dim(mat) <- c(3,3)

#--- init the cache mechanism ------------
cMat <- makeCacheMatrix()
cMat$set(mat)

#--- invert frist time ------ 
imat <- cacheSolve(cMat)
print(imat)
#cMat$setInv(imat)

#--- invert second time ----
#--- note the cache is used ----
imat <- cacheSolve(cMat)
print(imat)



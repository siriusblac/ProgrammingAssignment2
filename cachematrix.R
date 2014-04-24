## This function caches the result of a matrix in memory

makeCacheMatrix <- function(x = matrix())
{
	stored_result <<- NULL
	computed_matrix <<- x
	get <- function() computed_matrix
	set_result <- function(computed_result) stored_result <<- computed_result
	get_result <- function() stored_result
	
	return(list(get = get, set_result = set_result, get_result = get_result))
}


## This function is called, and it gives the cached result if available 
## else computes the new result and caches it for future use

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        result <- x$get_result()
        if(!is.null(result))
        {
        	message('Retrieving Cached result..')
        }
        else
        {
        	the_matrix <- x$get()
        	result <- solve(the_matrix)
        	x$set_result(result)
        }
        return(result)
}

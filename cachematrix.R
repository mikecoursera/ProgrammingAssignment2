## This is pregramming assignment #2 for the class R programming
## Due 6/22/2014
##
## Purpose is to create functions that will cache data from a specific 
## function.  Having a cache saves the computation time on subsequent calls
##
##

# makeCacheMatrix (baseMatrixData)
#   Create a caching object for Matrix type data.  Function is generic in that it
#   is not tied to a particular function.  But is tied to the particular data
#   type Matrix. 
# Parameters:
#   baseMatrixData - a matrix of data.  This is the data that is passed to the
#   processing, which the cache data is based upon.
# Return
#   list of routines to support the caching operations
makeCacheMatrix <- function(baseMatrixData = matrix()) 
{
	# initialize the cache to null
	cache <- NULL

	# set function - set the Matrix with a value
	set <- function (matrixData)
	{
		baseMatrixData <<- matrixData

        # when the value changes the cache of the results needs to be cleared
		cache <- NULL
	}

	# get - get the Matrix (not inverted)
	get <- function ()
	{
		baseMatrixData
	}

	# set the cache value
	setInversion <- function (cacheData)
	{
		cache <<- cacheData
	}

	# get the cache value
	getInversion <- function ()
	{
		cache
	}

	# put functions in a list, and return the list
	list(set = set, get = get, setInversion = setInversion, getInversion = getInversion)
}

# cacheSolve (x, ...)
#   Access the cached value for the parameter x, or solve the value and
#   store the result in the cache.  This is tied to the specific function
#   Matrix Inversion
# Parameters:
#   x - object that stores the matrix for which the result is desired.
#   ... - parameters to pass to the solve() function.  solve with a matrix
#   will generate an inversion matrix
# Return
#   matrix that is an inversion of the matrix stored in parameter x
cacheSolve <- function (x, ...)
{
	# check if value is in cache
	result <- x$getInversion()
	if (!is.null(result))
	{
	    message("getting cached data")
        return(result)
	}
    
    # value is not chached.
	message("generating data")
    # resolve the matrix inversion
	result <- solve(x$get(), ...)
    # cache the object for later use
	x$setInversion(result)
	return(result)
}

# TestProb2 ()
#   Test function to verify the 'makeCacheatrix' and 'cacheSolve' work
#   as required.  Although not required I needed to do this for my own
#   sense of accomplishment.  test data obtained from the website:
#   http://www.mathsisfun.com/algebra/matrix-inverse.html
# Parameters:
#   none
# Return
#   none

TestProb2 <- function()
{
    # load in the data
	sampleData <- matrix(c(4, 7, 2, 6), 2, 2)
    
    # create the caching object
	theMatrixData <- makeCacheMatrix()
	
    # run tests twice to validate the cache is cleared when new data is
    # pushed in
	for(i in 1:2)
	{
        # set the matrix data
        theMatrixData$set(sampleData)
        
        # inform of expected message
		message("Expect next line to say: generating data")
        
        # obtain the matrix inversion, should calculate it
		result <- cacheSolve(theMatrixData)
        
        # test results
        if (validateResult(result))
        {
            # on error return.  No need to proceed if not working
			return
		}
        
        # not needed
		result <- NULL

		# inform of expected message
		message("Expect next line to say: getting cached data")

		# obtain the matrix inversion, should pull from cache
		result <- cacheSolve(theMatrixData)
		
		# test results
		if (validateResult(result))
		{
		    # on error return.  No need to proceed if not working
		    return
		}
	}
}

# validateResult (answ, key)
#   Validate the answer obtained was correct.  This was needed because the use 
#   of floating point and its inablitiy to represent all numbers completely.
#   As such this will tolerate a small delta in answwers from the key.  Key was
#   provide by the website:
#   http://www.mathsisfun.com/algebra/matrix-inverse.html
#   If source data is changed the key will not be correct.  
# Parameters:
#   answ - matrix of answer to be checked
#   key - key to check answ against, use default if non provided
# Return
#   TRUE - matches close enough
#   FALSE - failed to match, need to examine in detail
validateResult <- function(answ, key = matrix(c(0.6, -0.7, -0.2, 0.4), 2, 2))
{
    # check if identical, doubtful
    if (identical(answ, key))
    {
        return(TRUE)
    }
    
    # find the delta between answer and key.  A better method would be least 
    # square, but saving that for version 2
    delta <- answ - key
    
    # see if the biggest deltas are trivial
    if ((min(delta) > -0.05) && (max(delta) < 0.05))
    {
        return(TRUE)
    }
    
    # big problems, need to examine
    message("Result is wrong.  Expected:")
    print(key)
    message("Result was:")
    print(answ)
    message("Delta:")
    print(answ - key)
    return(FALSE)
}
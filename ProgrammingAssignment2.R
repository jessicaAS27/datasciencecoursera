# Defining the size of the square matrix (change n to your desired size)
n <- 4 

# Creating a square matrix of size n x n
square_matrix <- matrix(1:(n^2), nrow = n, ncol = n)


# Function to create a cache for a matrix
makeCacheMatrix <- function(x = matrix()) {
  # Initialize a cache list
  cache <- NULL 
  
  # Function to set the matrix
  set <- function(matrix) {
    x <<- matrix
    cache$inverse <- NULL  # Invalidate the inverse cache when the matrix is updated
  }
  
  # Function to get the matrix
  get <- function() x
  
  # Function to set the inverse of the matrix
  setInverse <- function(inverse) {
    cache$inverse <- inverse
  }
  
  # Function to get the inverse of the matrix
  getInverse <- function() cache$inverse
  
  # Return a list of functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}
# Function to solve for the inverse of a cached matrix
cacheSolve <- function(x, ...) {
  
  # Check if the inverse is already in the cache
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    message("Getting cached data.")
    return(inverse)
  }
  
  # If not in the cache, calculate the inverse using solve
  matrix_to_solve <- x$get()
  inverse <- ginv(matrix_to_solve, ...)
  
  # Cache the inverse
  x$setInverse(inverse)
  
  inverse
}

cache_matrix <- makeCacheMatrix(square_matrix)
cached_inverse <- cacheSolve(cache_matrix)

# Print the cached inverse
print(cached_inverse)




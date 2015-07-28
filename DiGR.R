library(igraph)

StringToObject <- function(x) {
  # Converts a character vector to an object
  # Args:
  #  x, a length-1 character vector.
  
  if (length(x) > 1) {
    warning("Only the first item in the vector was used")
  }
  eval(parse(text = x[1]))
}

FuncToChar <- function(x) {
  # Converts a function into a character
  # Args:
  #  x, a function
  # Returns:
  #  a character vector, one entry per LoC in x
  
  as.character(attr(x, "srcref"))
}

FunctionsAdjacent <- function(x) {
  # Returns an adjacency matrix of which functions call each other.
  # Args:
  #  x, a character of function names to evaluate.
  # Returns:
  #  adj.mat, an adjacency matrix.
  
  x.fn    <- as.list(x)  # Convert to a list
  x.fn    <- lapply(x.fn, StringToObject)   # Convert to functions
  # convert each function into a text vector, one entry per LoC
  x.fn    <- lapply(x.fn, FuncToChar)
  # Not foolproof: 
  #  1. if function is behind a comment this will still count it.  
  #  2. if function appears twice on same line, this will only count it once.
  
  n.fn    <- length(x)  # count number of functions to evaluate
  adj.mat <- matrix(NA, nrow = n.fn, ncol = n.fn)  # initialize
  
  for (i in 1:n.fn) {
    for (j in 1:n.fn) {
      iter.logic    <- grepl(x[i], x.fn[[j]], fixed = T)
      adj.mat[i, j] <- sum(iter.logic)
    }
  }
  return(adj.mat)
}
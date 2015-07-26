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
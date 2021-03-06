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

FunctionsDigraph <- function(x, root = 1L) {
  # Plots a direction graph indicating how functions call each other.
  # Args:
  #  x, a character of function names to evaluate.
  #  root, an integer indicating which function(s) are 'root' functions.
  #        Root functions differ only in their plot color. Use 0 to
  #        indicate no root functions.
  # Returns {nothing}
  
  x.mat       <- FunctionsAdjacent(x)  # Create adjacency matrix
  x.g         <- graph_from_adjacency_matrix(x.mat, mode = "directed", diag = F)
  # The next two lines flip the edge directions
  el          <- get.edgelist(x.g, names = F)
  x.g         <- graph(rbind(el[,2],el[,1]))
  x.g$weight  <- 1  # weight each count in the matrix equally
  x.g         <- simplify(x.g)
  V(x.g)$name <- x  # decorate vertices with the function names
  color       <- rep("tan1", length(x))
  if (is.character(root)) {
    root.f    <- which(x %in% root)
    if (length(root) != length(root.f)) {
      warning(paste("Function", setdiff(root, x), "not found "))
    }
    root      <- root.f
  }
  color[root] <- "tomato"  # change root nodes' color 
  plot(x.g, edge.arrow.size = 0.25,
       vertex.color = color,
       vertex.size = 28,
       vertex.label.cex = 0.75,
       vertex.label.color = "black")
}

GetFunctions <- function(exclude = NA, path = getwd()) {
  # Returns a list of R functions in a directory, assuming each function
  # is in its own .R or .r file.
  # Args:
  #  exclude, a list of filenames to exclude. 
  #  path, a directory.
  
  pattern <- "[.][Rr]$"
  files   <- list.files(path, pattern)
  files   <- files[!files %in% exclude]
  gsub(pattern, "", files)  # strip the .r & return the function names
}

# Self-referential example:
x <- c("FunctionsAdjacent", "FuncToChar", "FunctionsDigraph", "StringToObject")
FunctionsDigraph(x, "FunctionsDigraph")
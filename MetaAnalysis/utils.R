library(sf)
library(stringdist)

scrg__get_features_from_sf <- function(sf) {
  # This function takes a geospatial object and returns the features as a data
  # frame.
  return(as.data.frame(st_drop_geometry(sf)))
}

scrg__make_string_distance_diff <- function(x, y) {
  # Takes two character vectors and compares the string distances between each
  # element of both vectors. Function returns a matrix of the results.
  x <- sort(x)
  y <- sort(y)
  if(scrg__venn_set(x, y)[4] == 0){
    # No values in either vector match
    return(
      matrix(
        c(replicate(length(x), 0), 
          replicate(length(y), 0)
        ), 
        nrow=length(x), 
        ncol=length(y), 
        dimnames=list(x, y)
      )
    )
  } else {
    str_dists <- list()
    for(a in x){
      for(b in y){
        str_dists <- append(str_dists, stringdist(a,b))
      }
    }
    return(
      matrix(
        str_dists,
        nrow=length(x),
        ncol=length(y),
        dimnames=list(y,x)
      )
    )
  }
}

scrg__venn_set <- function(x, y) {
  # Making a venn diagram of two vectors to determine what is not in the other 
  # vector
  # From: https://stackoverflow.com/a/17599164
  both <- union(x,y)
  inX <- both %in% x
  inY <- both %in% y
  return(table(inX,inY))
}

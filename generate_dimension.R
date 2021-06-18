generate_dimension <- function(unit, dimension) {
  names(dimension) <- unit
  dimen <- list()
  for (i in 1:length(dimension)) {
    dimen[[i]] <- dimension[i]
  }
  return(dimen)
}
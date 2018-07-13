# Conditinal-Concatenate function
c_if <- function(condition, dat, ...) {
  if(condition) {dat <- c(dat, ...)}
  return(dat)
}


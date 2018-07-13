# Conditinal-Concatenate function
c_if <- function(dat, condition, ...) {
  if(condition) {dat <- c(dat, ...)}
  return(dat)
}


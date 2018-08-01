# Conditinal-Concatenate function
#
# Concatenates the data `dat` with whatever is provided by `...` if the
#    `condition` is TRUE.
c_if <- function(dat, condition, ...) {
  if(condition) {dat <- c(dat, ...)}
  return(dat)
}


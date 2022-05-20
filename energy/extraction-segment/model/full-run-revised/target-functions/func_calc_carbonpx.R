## Tracey Mangin
## February 1, 2022
## function and param for calculating carbon price paths


perc_inc <- 0.07

calculate_carbonpx_val <- function(x0, r, t) {
  
  xt <- x0 * (1 + r) ^ t
  
}
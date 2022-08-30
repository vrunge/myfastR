
#' infoGenerator
#'
#' @description Generating dataframe for multiple change-point problems (with dynamic programming)
#' @param n number of rows
#' @param p number of change-point candidates
#' @return a dataframe with indices (k,j,i), positions (t1,t2), min value m and characteristics 'seen'
#' @examples
#' infoGenerator(100,5)
infoGenerator <- function(n = 1000, p = 10)
{
  info <- data.frame(matrix(ncol = 7, nrow = n)) ### info for pruning
  colnames(info) <- c("k", "j", "i", "t1", "t2", "m", "seen")
  info$k <- sample(x = p, size = n, replace = TRUE)
  info$j <- sample(x = p, size = n, replace = TRUE)
  info$i <- sample(x = p, size = n, replace = TRUE)
  info$t1 <- rnorm(n)
  info$t2 <- rnorm(n)
  info$m <- rchisq(n, 5)
  info$seen <- sample(x = c(0,1), size = n, replace = TRUE)
  return(info)
}

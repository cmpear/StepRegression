#' step_regression
#' @description this function takes a design matrix x, and a response vector y
#' @param x a matrix or variable that can be turned into a matrix via as.matrix.
#' @param y the response vector.  Defaults to NULL and redefined as the first column of x, or if that is constant, the last column of x
#' @param labels a list of column labels.  Defaults to NULL and redefind as the existing column names
#' @param addIntercept adds a column of ones as the first row of x to serve as an intercept.  Defaults to TRUE.  The 1s are added before missing ys are handled.
#' @importFrom stats sd
#' @returns a matrix of residuals for each variable in x.

step_regression <- function(x, y=NULL, labels = NULL, addIntercept = TRUE){
  if (!is.matrix(x)){
    x <- as.matrix(x)
  }
  if (!is.null(labels)){
    x = x[,labels]
    labels <- c(mean, colnames(x))
  }
  else{
    labels <- colnames(x)
  }
  if (addIntercept){
    x <- cbind(1,x)
    labels <- c("Intercept",labels)
  }
  if (is.null(y))
  {
    if (ncol(x)<=1){
      stop("Must either include a vector y, or matrix x must have at least 2 columns")
    }
    if (sd(x[,1]!=0)){
      message("No y vector provided, assuming first row contains y vector")
      y <- x[, 1]
      x <- x[,-1]
    }
    else{
      message("No v vector provided, first column does not vary, assuming last column contains y vector")
      y <- x[, ncol(x)]
      x <- x[,-ncol[x]]
    }
  }
  residuals <- regress_step(x,y)
  colnames(residuals)<-labels
  return(residuals)
}

#' regress_step
#' @description successively regresses each variable x our of the rest of the x matrix and y, then calls itself again.  Returns a vector of residuals for each variable.
#' @param x a design matrix.  Function called recursively with fewer columns of x until finished
#' @param y a response vector
#' @return a matrix of residuals, each column corresponds to a column of x
regress_step<-function(x,y){
  temp <- regress_step_helper(x[,1])
  print(colnames(x)[1])
  if (ncol(x)==1){
    residuals <- y - temp %*% y
    return(residuals)
  }
  residuals <- y - temp %*% y
  x <- x[,-1] - temp %*% x[,-1]
  # residuals replace y at each step
  return (cbind(residuals, regress_step(x,residuals)))
}

#' regress_step_helper
#' @description performs some matrix algebra on a vector as preparation to regress a variable out of x or y
#' @param vec response vector being to be regressed out
#' @note full function would be vec X solve(t(vec) X vec) X t(vec) X (vec or y); where X subs for matrix multiplication
regress_step_helper<-function(vec){
  vec %*% solve(t(vec) %*% vec) %*% t(vec)
}


# for (i in 1:ncol(x)){
#   plot(range(xZscore),range(res),type='n')
#   abline(h=0)
#   points(xZscore[,i],res[,i])
#   print(round(sum((res[,i])^2),6) - round(sum(residuals(lm (yZscore ~ xZscore[,1:i]))^2),6))
# }


#' Implements simple linear regression by gradient descent
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param explanatory The name of the explanatory variable in the data frame (unquoted)
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#'
#' @export
slr_gd <- function(dat, response, explanatory){

  ### Compute coefficients by gradient descent
  ### Return a data frame of the same form as in the `simple_linear_regression`

  x <- dat %>% pull({{explanatory}})
  y <- dat %>% pull({{response}})

  x <- scale(x)
  x <- as.matrix(data.frame(rep(1,length(y)),x))

  rate <- 0.00001
  iters <- 20000
  error <- 0.000001

  betas <- as.matrix(rnorm(n=dim(x)[2], mean=0,sd = 1))

  for (i in 1:iters){

    one <- data.matrix(2*rate*t(x))
    two <- data.matrix(y - (x %*% betas))

    dlb <- one %*% two
    betas1 <- betas + dlb

    if (error >= sum(abs(dlb))) {
      break
    }

    else{
      betas <- betas1
    }

  }

  results <- data.frame(t(betas1))
  colnames(results) <- c("Intercept","hp")

  print("I Standardized X so they might look different.")

  return(results)

}


#' Implements linear regression with many predictors by gradient descent
#'
#' This function computes coefficients for multiple regression by gradient descent
#' All columns of the provided data frame are used as predictors, except the
#' one specified as a response.
#'
#' No interaction terms are included.
#'
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#'
#'@export
mlr_gd <- function(dat, response) {



  x <- dat %>% select(-{{response}})
  y <- dat %>% pull({{response}})

  x <- scale(x)
  x <- as.matrix(data.frame(rep(1,length(y)),x))

  rate <- 0.00001
  iters <- 75000
  error <- 0.000001

  betas <- as.matrix(rnorm(n=dim(x)[2], mean=0,sd = 1))

  for (i in 1:iters){

    one <- data.matrix(2*rate*t(x))
    two <- data.matrix(y - (x %*% betas))

    dlb <- one %*% two
    betas1 <- betas + dlb

    if (error >= sum(abs(dlb))) {
      break
    }

    else{
      betas <- betas1
    }

  }

  results <- data.frame(t(betas1))
  colnames(results) <- c("Intercept","cyl","hp")

  print("I Standardized X so they might look different.")

  return(results)

}

#' Implements linear regression with many predictors by matrix decomposition
#'
#' This function computes coefficients for multiple regression by QR matrix decomposition
#' All columns of the provided data frame are used as predictors, except the
#' one specified as a response.
#'
#' No interaction terms are included.
#'
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#'
#'@export
mlr_qr <- function(dat, response) {

  x <- dat %>% select(-{{response}})
  y <- dat %>% pull({{response}})

  x <- scale(x)
  x <- as.matrix(data.frame(rep(1,length(y)),x))

  QR <- qr(x)
  results <- solve.qr(QR,y)

  results <- data.frame(t(results))

  ### Compute coefficients by QR decomposition
  ### Return a data frame of the same form as in the `multiple_linear_regression`

  return(results)

}

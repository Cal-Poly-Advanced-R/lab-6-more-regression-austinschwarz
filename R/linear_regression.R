#' Implements simple linear regression by hand
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param explanatory The name of the explanatory variable in the data frame (unquoted)
#' @param method The method used to compute the coefficients (NULL, "qr", "gradientdescent")
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#'
#' @export
simple_linear_regression <- function(dat, response, explanatory, method = NULL){

  x <- dat %>% pull({{explanatory}})
  y <- dat %>% pull({{response}})

  explan_name <- dat %>%
    select({{explanatory}}) %>%
    names()

  x_bar <- mean(x)
  y_bar <- mean(y)

  ### Edit code after here

  x <- cbind(rep(1,length(x)),x) #add intercept to x. x should be n by 2

  one <- solve(t(x) %*% x) #should get 2 by 2 matrix

  two <- t(x) %*% y #should get 2 by 1 matrix

  a = one %*% two # should get 2 by 1 matrix

  beta_0 <- a[[1,1]]
  beta_1 <- a[[2,1]]

  ### Stop editing

  results <- tibble::tibble(
    Intercept = beta_0,
    Slope = beta_1
  )

  names(results)[2] <- explan_name

  return(results)

}


#' Implements linear regression with many predictors by hand
#'
#' This function computes coefficients for multiple regression by hand.
#' All columns of the provided data frame are used as predictors, except the
#' one specified as a response.
#'
#' No interaction terms are included.
#'
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param method The method used to compute the coefficients (NULL, "qr", "gradientdescent")
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#'
#'@export
multiple_linear_regression <- function(dat, response, method = NULL) {

  y <- dat %>% select({{response}})
  x <- dat %>% select(-{{response}})

  x <- data.matrix(cbind(rep(1,dim(x)[1]),x)) #add intercept to x. x should be n by num of variables + 1

  one <- solve(t(x) %*% x) #should get numvar+1 by numvar+1 matrix

  two <- t(x) %*% data.matrix(y) #should get numvar+1 by 1 matrix

  a = one %*% two # should get numvar+1 by 1 matrix

  results <- data.frame(t(a))

  names(results)[1] <- "Intercept"

  return(results)

  return(results)

}

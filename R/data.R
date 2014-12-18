#' Simulations of the parameters of a simple linear regression with fake data.
#'
#' A coda object containing simulated values from the posterior distribution of the intercept, slope and residual of a linear regression with fake data (y = beta[1] + beta[2] * X + sigma). The purpose of the dataset is only to show the possibilities of the ggmcmc package.
#'
#' @name s
#' @docType data
#' @keywords datasets
#' @usage s
#' @format A coda object containing posterior distributions of the intercept, slope and residual of a linear regression with fake data.
NULL

#' Simulations of the posterior predictive distribution of a simple linear regression with fake data.
#'
#' A coda object containing simulated values from the posterior predictive distribution of the outcome of a linear regression with fake data (y ~ N(mu, sigma); mu = beta[1] + beta[2] * X; y.rep ~ N(mu, sigma); where y.rep is a replicated outcome, originally missing data). The purpose of the dataset is only to show the possibilities of the ggmcmc package.
#'
#' @name s.y.rep
#' @docType data
#' @keywords datasets
#' @usage s.y.rep
#' @format A coda object containing posterior distributions of the posterior predictive distribution of a linear regression with fake data.
NULL

#' Values for the observed outcome of a simple linear regression with fake data.
#'
#' A numeric vector containing the observed values of the outcome of a linear regression with fake data (y = beta[1] + beta[2] + X + sigma). The purpose of the dataset is only to show the possibilities of the ggmcmc package.
#'
#' @name y
#' @docType data
#' @keywords datasets
#' @usage y
#' @format A numeric vector containing the observed values of the outcome in the linear regression with fake data.
NULL

#' Simulations of the parameters of a simple linear regression with fake data.
#'
#' A coda object containing simulated values from the posterior distribution of the intercept and slope of a logistic regression with fake data (y ~ dbern(mu); logit(mu) = theta[1] + theta[2] * X), and the fitted / expected values (mu). The purpose of the dataset is only to show the possibilities of the ggmcmc package.
#'
#' @name s.binary
#' @docType data
#' @keywords datasets
#' @usage s.binary
#' @format A coda object containing posterior distributions of the intercept (theta[1]) and slope (theta[2]) of a logistic regression with fake data, and of the fitted / expected values (mu).
NULL

#' Values for the observed outcome of a binary logistic regression with fake data.
#'
#' A numeric vector containing the observed values (y) of the outcome of a logistic regression with fake data (y ~ dbern(mu); logit(mu) = theta[1] + theta[2] * X). The purpose of the dataset is only to show the possibilities of the ggmcmc package.
#'
#' @name y.binary
#' @docType data
#' @keywords datasets
#' @usage y.binary
#' @format A numeric vector containing the observed values of the outcome in the linear regression with fake data.
NULL

#' Simulations of the parameters of a hierarchical model using the radon example in Gelman & Hill (2007).
#'
#' A list containing the following elements: \code{counties} a data frame with the county label, ids and radon level; \code{id.county} a vector identifying counties in the data; \code{y} the outcome variable; \code{s.radon} a coda object with simulated values from the posterior distribution of all parameters, with few iterations for each one; \code{s.radon.yhat} a coda object containing simulated values from the posterior predictive distribution; and \code{s.radon.short} a coda object with simulated values from the posterior distribution of few parameters, with reasonable chain length. The purpose of the object is only to show the possibilities of the ggmcmc package.
#'
#' @name radon
#' @docType data
#' @keywords datasets
#' @usage radon
#' @format A list containing several elements to show the possibilities af ggmcmc.
NULL

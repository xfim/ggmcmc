#' Simulations of the parameters of a simple linear regression with fake data.
#'
#' A coda object containing simulated values from the posterior distribution of the intercept, slope and residual of a linear regression with fake data (y = beta[1] + beta[2] * X + sigma). The purpose of the dataset is only to show the possibilities of the ggmcmc package.
#'
#' @name s
#' @docType data
#' @keywords datasets
#' @usage data(s)
#' @format A coda object containing posterior distributions of the intercept, slope and residual of a linear regression with fake data.
NULL

#' Simulations of the posterior predictive distribution of a simple linear regression with fake data.
#'
#' A coda object containing simulated values from the posterior predictive distribution of the outcome of a linear regression with fake data (y ~ N(mu, sigma); mu = beta[1] + beta[2] * X; y.rep ~ N(mu, sigma); where y.rep is a replicated outcome, originally missing data). The purpose of the dataset is only to show the possibilities of the ggmcmc package.
#'
#' @name s.y.rep
#' @docType data
#' @keywords datasets
#' @usage data(s.y.rep)
#' @format A coda object containing posterior distributions of the posterior predictive distribution of a linear regression with fake data.
NULL

#' Values for the observed outcome of a simple linear regression with fake data.
#'
#' A numeric vector containing the observed values of the outcome of a linear regression with fake data (y = beta[1] + beta[2] + X + sigma). The purpose of the dataset is only to show the possibilities of the ggmcmc package.
#'
#' @name y
#' @docType data
#' @keywords datasets
#' @usage data(y)
#' @format A numeric vector containing the observed values of the outcome in the linear regression with fake data.
NULL

#' Simulations of the parameters of a simple linear regression with fake data.
#'
#' A coda object containing simulated values from the posterior distribution of the intercept and slope of a logistic regression with fake data (y ~ dbern(mu); logit(mu) = theta[1] + theta[2] * X), and the fitted / expected values (mu). The purpose of the dataset is only to show the possibilities of the ggmcmc package.
#'
#' @name s.binary
#' @docType data
#' @keywords datasets
#' @usage data(s.binary)
#' @format A coda object containing posterior distributions of the intercept (theta[1]) and slope (theta[2]) of a logistic regression with fake data, and of the fitted / expected values (mu).
NULL

#' Values for the observed outcome of a binary logistic regression with fake data.
#'
#' A numeric vector containing the observed values (y) of the outcome of a logistic regression with fake data (y ~ dbern(mu); logit(mu) = theta[1] + theta[2] * X). The purpose of the dataset is only to show the possibilities of the ggmcmc package.
#'
#' @name y.binary
#' @docType data
#' @keywords datasets
#' @usage data(y.binary)
#' @format A numeric vector containing the observed values of the outcome in the linear regression with fake data.
NULL




#' Simulated data for a continuous linear regression and its MCMC samples
#'
#' Simulate a dataset with one explanatory variable and one continuous outcome variable using (y ~ dnorm(mu, sigma); mu = beta[1] + beta[2] * X). The data loads three objects: the observed y values, a coda object containing simulated values from the posterior distribution of the intercept and slope of a linear regression, and a coda object containing simulated values from the posterior predictive distribution. The purpose of the dataset is only to show the possibilities of the ggmcmc package.
#' @name linear
#' @docType data
#' @keywords datasets
#' @usage data(linear)
#' @source Simulated data for ggmcmc
#' @format Three objects, namely:
#' \describe{
#'   \item{s}{A coda object containing posterior distributions of the intercept (beta[1]) and slope (beta[2]) of a linear regression with simulated data.}
#'   \item{s.y.rep}{A coda object containing simulated values from the posterior predictive distribution of the outcome of a linear regression with simulated data (y ~ N(mu, sigma); mu = beta[1] + beta[2] * X; y.rep ~ N(mu, sigma); where y.rep is a replicated outcome, originally missing data).}
#'   \item{y}{A numeric vector containing the observed values of the outcome in the linear regression with simulated data.}
#' }
#' @examples
#' data(linear)
#' str(s)
#' str(s.y.rep)
#' str(y)
NULL

#' Simulated data for a binary logistic regression and its MCMC samples
#'
#' Simulate a dataset with one explanatory variable and one binary outcome variable using (y ~ dbern(mu); logit(mu) = theta[1] + theta[2] * X). The data loads two objects: the observed y values and the coda object containing simulated values from the posterior distribution of the intercept and slope of a logistic regression. The purpose of the dataset is only to show the possibilities of the ggmcmc package.
#'
#' @name binary
#' @docType data
#' @keywords datasets
#' @usage data(binary)
#' @source Simulated data for ggmcmc
#' @format Two objects, namely:
#' \describe{
#'   \item{s.binary}{A coda object containing posterior distributions of the intercept (theta[1]) and slope (theta[2]) of a logistic regression with simulated data.}
#'   \item{y.binary}{A numeric vector containing the observed values of the outcome in the binary regression with simulated data.}
#' }
#' @examples
#' data(binary)
#' str(s.binary)
#' str(y.binary)
#' table(y.binary)
NULL

#' Simulations of the parameters of a hierarchical model
#'
#' Using the radon example in Gelman & Hill (2007), the list contains several elements to show the possibilities of ggmcmc for applied Bayesian Hierarchical/multilevel analysis.
#'
#' @name radon
#' @docType data
#' @keywords datasets
#' @usage data(radon)
#' @source \url{http://www.stat.columbia.edu/~gelman/arm/examples/radon/}
#' @format A list containing several elements (data and outputs of the analysis):
#' \describe{
#'   \item{counties}{A data frame with the country label, ids and radon level.}
#'   \item{id.county}{A vector identifying counties in the data.}
#'   \item{y}{The outcome variable.}
#'   \item{s.radon}{A coda object with simulated values from the posterior distribution of all parameters, with few iterations for each one.}
#'   \item{s.radon.yhat}{A coda object containing simulated values from the posterior predictive distribution.}
#'   \item{s.radon.short}{A coda object with simulated values from the posterior distribution of few parameters, with reasonable chain length.}
#' }
#' @examples
#' data(radon)
#' names(radon)
#' # Generate a data frame suitable for matching with the generated samples
#' # through the "par_labels" function:
#' L.radon <- plab("alpha", match = list(County = radon$counties$County))
#'
"radon"

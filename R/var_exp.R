#' var_exp
#'
#' @description
#' \code{var_exp} is used to compute the proportion of the fraction of variance explained by a principal component analysis.
#'
#' @param data a data frame that contains the variables to be used in CUR decomposition.
#' @param standardize logical. If \code{TRUE} rescale an original data frame to have a mean of zero and a standard deviation of one.
#' @param ... Additional arguments to be passed to \code{dplyr::select}
#'
#' @details
#'
#' The objective of CUR decomposition is to find the most relevant variables and observations within a data matrix and to reduce the dimensionality. It is well known that as more columns (variables) and rows are selected, the relative error will be lower; however, this is not true for k (number of components to calculate leverages). Given the above, this function seeks to find the best-balanced scenario of k, the number of relevant columns, and rows that have an error very close to the minimum, and that, in turn, uses a smaller amount of information.
#'
#' @return
#'
#' \item{var_exp}{a data frame with the proportion of explained variance for each principal component.}
#'
#' @examples
#' \donttest{
#' var_exp(AASP, standardize = TRUE, hoessem:notabachillerato)
#' }
#' @author
#'
#' Cesar Gamboa-Sanabria, Stefany Matarrita-Munoz, Katherine Barquero-Mejias, Greibin Villegas-Barahona, Mercedes Sanchez-Barba and Maria Purificacion Galindo-Villardon.
#'
#' @seealso
#'
#' \code{\link{dCUR}}
#' \code{\link{CUR}}
#'
#' @references
#'
#' \insertRef{Mahoney697}{dCUR}
#' \insertRef{villegas2018modelo}{dCUR}
#' \insertRef{dynamyCUR}{dCUR}
#'
#' @export
var_exp <- function(data, standardize=FALSE,...){

  data <- dplyr::select(data,...)

  if(standardize){
    data <- scale(data)
  }else({})

  decomposition <- svd(data)
  sigma <- t(decomposition$u)%*%as.matrix(data)%*%decomposition$v

  var_expl <- round(cumsum(diag(sigma)/sum(diag(sigma)))*100, 2)
  data.frame(component=paste("PCA", 1:length(var_expl), sep=""), Variance=paste(sprintf("%.3f", var_expl), "%", sep=""))
}

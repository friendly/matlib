

#' The Matrix Sweep Operator
#'
#' The \code{swp} function \dQuote{sweeps} a matrix on the rows and columns given in \code{index} to produce a new matrix
#' with those rows and columns \dQuote{partialled out} by orthogonalization. This was defined as a fundamental statistical operation in
#' multivariate methods by Beaton (1964) and expanded by Dempster (1969). It is closely related to orthogonal projection,
#' but applied to a cross-products or covariance matrix, rather than to data.
#'
#' If \code{M} is the partitioned matrix
#' \deqn{\left[ \begin{array}{cc} \mathbf {R} &  \mathbf {S} \\ \mathbf {T} &  \mathbf {U} \end{array} \right]}
#' where \eqn{R} is \eqn{q \times q} then \code{swp(M, 1:q)} gives
#' \deqn{\left[ \begin{array}{cc} \mathbf {R}^{-1} &  \mathbf {R}^{-1}\mathbf {S} \\ -\mathbf {TR}^{-1} &  \mathbf {U}-\mathbf {TR}^{-1}\mathbf {S} \\ \end{array} \right]}
#'
#' @param M a numeric matrix
#' @param index a numeric vector indicating the rows/columns to be swept.  The entries must be less than or equal
#'     to the number or rows or columns in \code{M}.  If missing, the function sweeps on all rows/columns \code{1:min(dim(M))}.
#'
#' @return the matrix \code{M} with rows and columns in \code{indices} swept.
#' @references Beaton, A. E. (1964), \emph{The Use of Special Matrix Operations in Statistical Calculus}, Princeton, NJ: Educational Testing Service.
#'
#'      Dempster, A. P. (1969) \emph{Elements of Continuous Multivariate Analysis}. Addison-Wesley Publ. Co., Reading, Mass.
#'
#' @seealso \code{\link{Proj}}, \code{\link{QR}}
#' @export
#' @examples
#' data(therapy)
#' mod3 <- lm(therapy ~ perstest + IE + sex, data=therapy)
#' X <- model.matrix(mod3)
#' XY <- cbind(X, therapy=therapy$therapy)
#' XY
#' M <- crossprod(XY)
#' swp(M, 1)
#' swp(M, 1:2)

swp <- function (M, index)
{
  p <- ncol(M)
  if (missing(index)) index <- 1:min(dim(M))
  u <- is.na(match(1:p, index))
  a <- (1:p)[u]
  out <- 0 * M
  dimnames(out) <- dimnames(M)
  if (length(a) == 0)
    return(-solve(M))
  else if (length(a) == p)
    return(M)
  else {
    Saa <- M[a, a, drop = FALSE]
    Sab <- M[a, index, drop = FALSE]
    Sbb <- M[index, index, drop = FALSE]
    B <- Sab %*% solve(Sbb)
    out[a, a] <- Saa - B %*% t(Sab)
    out[a, index] <- B
    out[index, a] <- t(B)
    out[index, index] <- -solve(Sbb)
    return(out)
  }
}

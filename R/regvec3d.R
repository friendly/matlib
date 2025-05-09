
#' Vector space representation of a two-variable regression model
#'
#' \code{regvec3d} calculates the 3D vectors that represent the projection of a two-variable multiple
#' regression model from n-D \emph{observation} space into the 3D mean-deviation \emph{variable} space that they span, thus
#' showing the regression of \code{y} on \code{x1} and \code{x2} in the model \code{lm(y ~ x1 + x2)}.
#' The result can be used to draw 2D and 3D vector diagrams accurately reflecting the partial and marginal
#' relations of \code{y} to \code{x1} and \code{x2} as vectors in this representation.
#'
#' If additional variables are included in the model, e.g., \code{lm(y ~ x1 + x2 + x3 + ...)}, then
#' \code{y}, \code{x1} and \code{x2} are all taken as \emph{residuals} from their separate linear fits
#' on \code{x3 + ...}, thus showing their partial relations net of (or adjusting for) these additional predictors.
#'
#' A 3D diagram shows the vector \code{y} and the plane formed by the predictors,
#' \code{x1} and \code{x2}, where all variables are represented in deviation form, so that
#' the intercept need not be included.
#'
#' A 2D diagram, using the first two columns of the result, can be used to show the projection
#' of the space in the \code{x1}, \code{x2} plane.
#'
#' In these views, the ANOVA representation of the various sums of squares for the regression
#' predictors appears as the lengths of the various vectors.  For example, the error sum of
#' squares is the squared length of the \code{e} vector, and the regression sum of squares is
#' the squared length of the \code{yhat} vector.
#'
#' The drawing functions \code{\link{vectors}} and \code{\link{vectors3d}} used by the \code{\link{plot.regvec3d}} method only work
#' reasonably well if the variables are shown on commensurate scales, i.e., with
#' either \code{scale=TRUE} or \code{normalize=TRUE}.
#'
#' @param x1   The generic argument or the first predictor passed to the default method
#' @param ...  Arguments passed to methods
#'
#' @return     An object of class \dQuote{regvec3d}, containing the following components
#'             \item{model}{The \dQuote{lm} object corresponding to \code{lm(y ~ x1 + x2)}.}
#'             \item{vectors}{A 9 x 3 matrix, whose rows correspond to the variables in the model,
#'                the residual vector, the fitted vector, the partial fits for \code{x1}, \code{x2},
#'                and the marginal fits of \code{y} on \code{x1} and \code{x2}.
#'                The columns effectively represent \code{x1}, \code{x2}, and \code{y}, but
#'                are named \code{"x"}, \code{"y"} and \code{"z"}.}
#' @seealso  \code{\link{plot.regvec3d}}
#' @references        Fox, J. (2016). \emph{Applied Regression Analysis and Generalized Linear Models}, 3rd ed., Sage, Chapter 10.
#' @family vector diagrams
#' @import stats
#' @export
#'
regvec3d <- function(x1, ...){
  UseMethod("regvec3d")
}

#' @param formula     A two-sided formula for the linear regression model. It must contain two quantitative predictors
#'                    (\code{x1} and \code{x2}) on the right-hand-side.  If further predictors are included, \code{y},
#'                    \code{x1} and \code{x2} are taken as residuals from the their linear fits on these variables.
#' @param data        A data frame in which the variables in the model are found
#' @param which       Indices of predictors variables in the model taken as \code{x1} and \code{x2}
#' @param name.x1     Name for \code{x1} to be used in the result and plots. By default, this is taken as the
#'                    name of the \code{x1} variable in the \code{formula}, possibly abbreviated according to \code{abbreviate}.
#' @param name.x2     Ditto for the name of \code{x2}
#' @param name.y      Ditto for the name of \code{y}
#' @param name.e      Name for the residual vector. Default: \code{"residuals"}
#' @param name.y.hat  Name for the fitted vector
#' @param name.b1.x1  Name for the vector corresponding to the partial coefficient of \code{x1}
#' @param name.b2.x2  Name for the vector corresponding to the partial coefficient of \code{x2}
#' @param abbreviate  An integer.  If \code{abbreviate >0}, the names of \code{x1}, \code{x2} and \code{y}
#'                    are abbreviated to this length before being combined with the other \code{name.*} arguments
#'
#' @describeIn regvec3d Formula method for regvec3d
#' @references Fox, J. and Friendly, M. (2016). "Visualizing Simultaneous Linear Equations, Geometric Vectors, and
#' Least-Squares Regression with the matlib Package for R". \emph{useR Conference}, Stanford, CA, June 27 - June 30, 2016.
#' @export
#'
#' @examples
#' library(rgl)
#' therapy.vec <- regvec3d(therapy ~ perstest + IE, data=therapy)
#' therapy.vec
#' plot(therapy.vec, col.plane="darkgreen")
#' plot(therapy.vec, dimension=2)

regvec3d.formula <- function(formula, data=NULL, which=1:2, name.x1, name.x2,
                             name.y, name.e, name.y.hat,
                             name.b1.x1, name.b2.x2,
                             abbreviate=0, ...){
  mod <- lm(formula, data)
  X <- model.matrix(mod)
  intercept <- which(colnames(X) == "(Intercept)")
  if (length(intercept) > 0) X <- X[, -intercept]
  y <- model.response(model.frame(mod))
  if (ncol(X) > 2) {
    X1 <- X[, -which, drop=FALSE]
    y <- residuals(lm.fit(X1, y))
    x1 <- residuals(lm.fit(X1, X[, which[1]]))
    x2 <- residuals(lm.fit(X1, X[, which[2]]))
    const.names <- if (ncol(X) <= 4) paste("|", paste(colnames(X)[-which], collapse=", "))
    else "| others"
  }
  else{
    x1 <- X[, 1]
    x2 <- X[, 2]
    const.names <-""
  }
  if (missing(name.x1)) name.x1 <- paste(colnames(X)[which[1]], const.names)
  if (missing(name.x2)) name.x2 <- paste(colnames(X)[which[2]], const.names)
  if (missing(name.y)) name.y <- paste(as.character(formula[2]), const.names)
  if (abbreviate > 0){
    name.x1 <- abbreviate(name.x1, abbreviate)
    name.x2 <- abbreviate(name.x2, abbreviate)
    name.y <- abbreviate(name.y, abbreviate)
  }
  if (missing(name.e)) name.e <- "residuals"
  if (missing(name.b1.x1)) name.b1.x1 <- paste("b1", name.x1)
  if (missing(name.b2.x2)) name.b2.x2 <- paste("b2", name.x2)
  regvec3d(x1, x2, y, name.x1=name.x1, name.x2=name.x2, name.y=name.y,
           name.e=name.e, name.b1.x1=name.b1.x1, name.b2.x2=name.b2.x2, ...)
}

#' @param x2           second predictor variable in the model
#' @param y            response variable in the model
#' @param scale        logical; if \code{TRUE}, standardize each of \code{y}, \code{x1}, \code{x2} to standard scores
#' @param normalize    logical; if \code{TRUE}, normalize each vector relative to the maximum length of all
#' @param name.y1.hat  Name for the vector corresponding to the marginal coefficient of \code{x1}
#' @param name.y2.hat  Name for the vector corresponding to the marginal coefficient of \code{x2}
#'
#' @describeIn regvec3d Default method for regvec3d
#' @export
#'
regvec3d.default <- function(x1, x2, y, scale=FALSE, normalize=TRUE,
                             name.x1=deparse(substitute(x1)), name.x2=deparse(substitute(x2)),
                             name.y=deparse(substitute(y)), name.e="residuals", name.y.hat=paste0(name.y, "hat"),
                             name.b1.x1=paste0("b1", name.x1), name.b2.x2=paste0("b2", name.x2),
                             name.y1.hat=paste0(name.y, "hat 1"), name.y2.hat=paste0(name.y, "hat 2"), ...){
  force(name.x1)
  force(name.x2)
  force(name.y)
  force(name.e)
  force(name.y.hat)
  force(name.b1.x1)
  force(name.b2.x2)
  force(name.y1.hat)
  force(name.y2.hat)
  nms <- make.names(c(name.y, name.x1, name.x2), unique=TRUE)
  nm.y <- nms[1]
  nm.x1 <- nms[2]
  nm.x2 <- nms[3]
  formula <- as.formula(paste(nm.y, "~",nm.x1, "+", nm.x2))
  Data <- data.frame(y, x1, x2)
  names(Data) <- nms
  model <- lm(formula, data=Data)
  len <- function(x) sqrt(sum(x^2))
  n <- length(y)
  y <- if (scale) scale(y)/sqrt(n - 1) else y - mean(y)
  x1 <- if (scale) scale(x1)/sqrt(n - 1) else x1 - mean(x1)
  x2 <- if (scale) scale(x2)/sqrt(n - 1) else x2 - mean(x2)
  x1s <- c(len(x1), 0, 0)
  f1 <- lsfit(x1, x2, intercept=FALSE)
  r1 <- c(0, len(residuals(f1)), 0)
  x2s <- r1 + coef(f1)*x1s
  fy <- lsfit(cbind(x1, x2), y, intercept=FALSE)
  ry <- c(0, 0, len(residuals(fy)))
  b <- coef(fy)
  b1x1s <- x1s*b[1]
  b2x2s <- x2s*b[2]
  yhat <- b1x1s + b2x2s
  ys <- yhat + ry
  m1 <- lsfit(x1, y, intercept=FALSE)
  y1hat <- coef(m1)*x1s
  m2 <- lsfit(x2, y, intercept=FALSE)
  m <- max(len(x1), len(x2), len(y))
  y2hat <- coef(m2)*x2s
  vectors <- rbind(x1s, x2s, ys, ry, yhat, b1x1s, b2x2s, y1hat, y2hat)
  if (normalize) vectors <- vectors / m
  rownames(vectors) <- c(name.x1, name.x2, name.y, name.e,
                         name.y.hat, name.b1.x1, name.b2.x2, name.y1.hat, name.y2.hat)
  colnames(vectors) <- c("x", "y", "z")
  result <- list(model=model, vectors=vectors, scale=scale, normalize=normalize)
  class(result) <- "regvec3d"
  result
}

#' Plot method for regvec3d objects
#'
#' The plot method for \code{regvec3d} objects uses the low-level graphics tools in this package to draw 3D and 3D
#' vector diagrams reflecting the partial and marginal
#' relations of \code{y} to \code{x1} and \code{x2} in a bivariate multiple linear regression model,
#' \code{lm(y ~ x1 + x2)}.
#'
#' A 3D diagram shows the vector \code{y} and the plane formed by the predictors,
#' \code{x1} and \code{x2}, where all variables are represented in deviation form, so that
#' the intercept need not be included.
#'
#' A 2D diagram, using the first two columns of the result, can be used to show the projection
#' of the space in the \code{x1}, \code{x2} plane.
#'
#' The drawing functions \code{\link{vectors}} and \code{\link{vectors3d}} used by the \code{\link{plot.regvec3d}} method only work
#' reasonably well if the variables are shown on commensurate scales, i.e., with
#' either \code{scale=TRUE} or \code{normalize=TRUE}.
#'
#' @param x           A \dQuote{regvec3d} object
#' @param y           Ignored; only included for compatibility with the S3 generic
#' @param dimension   Number of dimensions to plot: \code{3} (default) or \code{2}
#' @param col         A vector of 5 colors. \code{col[1]} is used for the y and residual (e) vectors, and for x1 and x2;
#'                    \code{col[2]} is used for the vectors \code{y -> yhat} and \code{y -> e};
#'                    \code{col[3]} is used for the vectors \code{yhat -> b1} and \code{yhat -> b2};
#' @param col.plane   Color of the base plane in a 3D plot or axes in a 2D plot
#' @param cex.lab     character expansion applied to vector labels. May be a number or numeric vector corresponding to the the
#'        rows of \code{X}, recycled as necessary.
#' @param show.base  If \code{show.base > 0}, draws the base plane in a 3D plot; if \code{show.base > 1},
#'                    the plane is drawn thicker
#' @param show.marginal  If \code{TRUE} also draws lines showing the marginal relations of \code{y} on \code{x1} and on \code{x2}
#' @param show.hplane If \code{TRUE}, draws the plane defined by \code{y}, \code{yhat} and the origin in the 3D
#' @param show.angles If \code{TRUE}, draw and label the angle between the \code{x1} and \code{x2} and between \code{y} and \code{yhat},
#'                     corresponding respectively to the correlation between the xs and the multiple correlation
#' @param error.sphere Plot a sphere (or in 2D, a circle) of radius proportional to the length of
#'                     the residual vector, centered either at the origin (\code{"e"})
#'                     or at the fitted-values vector (\code{"y.hat"}; the default is \code{"none"}.)
#' @param scale.error.sphere Whether to scale the error sphere if \code{error.sphere="y.hat"}; defaults to \code{TRUE} if the
#'                           vectors representing the variables are scaled, in which case the oblique projections of the error spheres
#'                           can represent confidence intervals for the coefficients; otherwise defaults to \code{FALSE}.
#' @param level.error.sphere The confidence level for the error sphere, applied if \code{scale.error.sphere=TRUE}.
#' @param grid        If \code{TRUE}, draws a light grid on the base plane
#' @param add         If \code{TRUE}, add to the current plot; otherwise start a new rgl or plot window
#' @param ...         Parameters passed down to functions [unused now]
#'
#' @return            None
#' @references        Fox, J. (2016). \emph{Applied Regression Analysis and Generalized Linear Models}, 3rd ed., Sage, Chapter 10.
#' @seealso  \code{\link{regvec3d}}, \code{\link{vectors3d}}, \code{\link{vectors}}

#' @family vector diagrams
#' @export
#' @importFrom graphics symbols
#'
#' @examples
#' if (require(carData)) {
#'    data("Duncan", package="carData")
#'    dunc.reg <- regvec3d(prestige ~ income + education, data=Duncan)
#'    plot(dunc.reg)
#'    plot(dunc.reg, dimension=2)
#'    plot(dunc.reg, error.sphere="e")
#'    summary(dunc.reg)
#'
#'    # Example showing Simpson's paradox
#'    data("States", package="carData")
#'    states.vec <- regvec3d(SATM ~ pay + percent, data=States, scale=TRUE)
#'    plot(states.vec, show.marginal=TRUE)
#'    plot(states.vec, show.marginal=TRUE, dimension=2)
#'    summary(states.vec)
#' }

plot.regvec3d <- function(x, y, dimension=3,
                          col=c("black", "red", "blue", "brown", "lightgray"), col.plane="gray",
                          cex.lab=1.2,
                          show.base=2, show.marginal=FALSE, show.hplane=TRUE, show.angles=TRUE,
                          error.sphere=c("none", "e", "y.hat"), scale.error.sphere=x$scale,
                          level.error.sphere=0.95,
                          grid=FALSE, add=FALSE, ...){

  angle <- function(v1, v2) {
    r12 <- crossprod(v1, v2)/(len(v1)*len(v2))
    acos(r12)*180/pi
  }

  error.sphere <- match.arg(error.sphere)
  vectors <- x$vectors
  origin <- c(0,0,0)
  abs <- TRUE
  if (dimension == 3){
    if (!add) {
      open3d()
      aspect3d("iso")
    }
    ref.length <- vectors3d(vectors[1:7, ], draw=FALSE)
    vectors3d(vectors[3:4, ], color=col[1], lwd=2, cex.lab=cex.lab, ref.length=ref.length)
    vectors3d(vectors[1:2, ], color=col[1], lwd=2, cex.lab=cex.lab, ref.length=ref.length)
    vectors3d(vectors[5:7, ], color=col.plane, lwd=2, cex.lab=cex.lab, ref.length=ref.length)
    if (show.base > 0) planes3d(0, 0, 1, 0, color=col.plane, alpha=0.2)
    if (show.base > 1) planes3d(0, 0, 1, -.01, color=col.plane, alpha=0.1)
    lines3d(vectors[c(3, 5), ], color=col[2], lwd=2)     # y -> yhat
    lines3d(vectors[c(3, 4), ], color=col[2])            # y -> e
    lines3d(vectors[c(5, 6), ], color=col[3])            # yhat -> b1
    lines3d(vectors[c(5, 7), ], color=col[3])            # yhat -> b2
    if (show.marginal){
      vectors3d(vectors[8:9, ], color=col.plane, cex.lab=cex.lab, ref.length=ref.length)
      lines3d(vectors[c(3, 8), ], color=col[4])
      lines3d(vectors[c(3, 9), ], color=col[4])
      corner(origin, vectors[8, ], vectors[3, ], color=col[4], d=0.05, absolute=abs)
      corner(origin, vectors[9, ], vectors[3, ], color=col[4], d=0.05, absolute=abs)
      lines3d(vectors[c(5, 8), ], color=col[3])
      lines3d(vectors[c(5, 9), ], color=col[3])
      corner(origin, vectors[8, ], vectors[5, ], color=col[3], d=0.05, absolute=abs)
      corner(origin, vectors[9, ], vectors[5, ], color=col[3], d=0.05, absolute=abs)
    }
    if (show.hplane) triangles3d(rbind(vectors[c(3,5),], origin), color=col[2], alpha=0.2)
    if (grid) grid3d("z", col="darkgray", lty=2, n=8)
    if (show.angles){
      R2 <- summary(x$model)$r.squared
      angleR2 <- acos(sqrt(R2))*180/pi
      text3d(0.1*(vectors[3, ] + vectors[5, ]), texts=paste(round(angleR2, 1), "deg."), color=col[4])
      arc(vectors[5, ], origin, vectors[3, ], color=col[4])
      angle12 <- angle(vectors[1,], vectors[2,])
      text3d(0.1*(vectors[1, ] + vectors[2, ]), texts=paste(round(angle12, 1), "deg."), color=col[3])
      arc(vectors[1, ], origin, vectors[2, ], color=col[3])
    }
    corner(vectors[5, ], origin, vectors[4, ], color=col[4], d=0.05, absolute=abs)
    corner(origin, vectors[5, ], vectors[3, ], color=col[4], d=0.05, absolute=abs)
    if ("e" == error.sphere) spheres3d(0, 0, 0, radius=len(vectors[4, ]),
                                       color=col[5], alpha=0.1)
    else if ("y.hat" == error.sphere) {
      sqrt.vif <- sqrt(1/(1 - (cos(angle(vectors[1,], vectors[2,])*pi/180))^2))
      spheres3d(x$vectors[5, ],
                radius= rad <- if (scale.error.sphere)
                  sqrt.vif*qt((1 - level.error.sphere)/2, df=x$model$df.residual, lower.tail=FALSE)*len(vectors[4, ])/sqrt(x$model$df.residual)
                else len(vectors[4, ]),
                color=col[5], alpha=0.25)
      if (scale.error.sphere) circle3d(x$vectors[5, ], rad, color=col[3], lwd=2)
    }
  }
  else {
    vecs2D <- vectors[c(1,2,5,6,7,8,9), 1:2]
    xlim <- range(vecs2D[,1]) + c(-.1, .1)
    ylim <- range(vecs2D[,2]) + c(-.1, .1)
    if (!add) plot(xlim, ylim, type="n", xlab="", ylab="", asp=1, axes=FALSE)
    #        abline(h=0, v=0, col=col.plane)
    if ("e" == error.sphere) symbols(0, 0, circles=len(vectors[4, ]),
                                     fg=col[5], bg=col[5], add=TRUE, inches=FALSE,
                                     xpd=TRUE)
    else if ("y.hat" == error.sphere) {
      sqrt.vif <- sqrt(1/(1 - (cos(angle(vectors[1,], vectors[2,])*pi/180))^2))
      symbols(vecs2D[3, 1], vecs2D[3, 2], circles=if(scale.error.sphere)
        sqrt.vif*qt((1 - level.error.sphere)/2, df=x$model$df.residual, lower.tail=FALSE)*len(vectors[4, ])/sqrt(x$model$df.residual)
        else len(vectors[4, ]),
        fg=col[5], bg=col[5], add=TRUE, inches=FALSE,
        xpd=TRUE)
    }
    if (show.marginal){
      vectors(vecs2D[6:7, ], pos.lab=c(4, 4), col=col[c(1, 1)], cex.lab=cex.lab, xpd=TRUE)
      lines(vecs2D[c(3, 6), ], col=col[4], lty=2)
      lines(vecs2D[c(3, 7), ], col=col[4], lty=2)
      corner(c(0, 0), vecs2D[6, ], vecs2D[3, ], col=col[4], absolute=abs)
      corner(c(0, 0), vecs2D[7, ], vecs2D[3, ], col=col[4], absolute=abs)
    }
    vectors(vecs2D[1:5, ], pos.lab=c(4, 4, 4, 1, 2), col=col[c(1, 1, 2, 3, 3)], cex.lab=cex.lab, xpd=TRUE)
    lines(vecs2D[c(3, 4),], col=col[3], lty=2)
    lines(vecs2D[c(3, 5),], col=col[3], lty=2)
    if (show.angles){
      arc(vecs2D[1, ], c(0, 0), vecs2D[2, ], d=0.2, absolute=abs, col=col[3])
      angle12 <- angle(vectors[1,], vectors[2,])
      txt.coords <- 0.2*(vecs2D[1, ] + vecs2D[2, ])
      text(txt.coords[1], txt.coords[2], paste(round(angle12, 1), "deg."),
           col=col[3])
    }
  }
}

#' Summary method for regvec3d objects
#'
#' The \code{summary} method prints the vectors and their vector lengths, followed by the \code{summary}
#' for the model.
#'
#' @param object A \code{regvec3d} object for the \code{summary} method
#' @rdname plot.regvec3d
#'
#' @export
#'
summary.regvec3d <- function(object, ...){
  vectors <- object$vectors
  length <- apply(vectors, 1, function(x) sqrt(sum(x^2)))
  vectors <- cbind(vectors, length)
  colnames(vectors)[4] <- "length"
  print(vectors)
  print(summary(object$model))
}

#' Print method for regvec3d objects
#'
#' @rdname plot.regvec3d
#' @export
#'
print.regvec3d <- function(x, ...) {
  vectors <- x$vectors
  print(vectors)
  invisible(x)
}

circle3d <- function(center, radius, segments=100, fill=FALSE, ...){
  #' Draw a horizontal circle
  #'
  #' A utility function for drawing a horizontal circle in the (x,y) plane in a 3D graph
  #'
  #' @param center  A vector of length 3.
  #' @param radius  A positive number.
  #' @param segments  An integer specifying the number of line segments to use to draw the circle (default, 100).
  #' @param fill logical; if \code{TRUE}, the circle is filled (the default is \code{FALSE}).
  #' @param ... \pkg{rgl} material properties for the circle.
  #' @family vector diagrams
  #' @export
  #' @examples
  #' ctr=c(0,0,0)
  #' circle3d(ctr, 3, fill = TRUE)
  #' circle3d(ctr - c(-1,-1,0), 3, col="blue")
  #' circle3d(ctr + c(1,1,0),   3, col="red")

  angles <- seq(0, 2*pi, length=100)
  x <- center[1] + radius*sin(angles)
  y <- center[2] + radius*cos(angles)
  polygon3d(x, y, z=rep(center[3], segments), fill=fill, ...)
}

# Functions to add decorators over or under matrices
#
#' @name Eqn_helpers
#' @aliases overset underset overbrace underbrace 
#' @aliases Eqn_overset Eqn_underset Eqn_overbrace Eqn_underbrace 
#' @title Functions to Add Labels and/or Braces 
#' 
#' @description
#' 
#' These functions extend \code{\link{Eqn}} to add labels or braces over or under a LaTeX expression
#' or a \code{"latexMatrix"} object.
#' \itemize{
#'    \item{\code{overset} and \code{underset} typesets a label over or under an object}
#'    \item{\code{overbrace} and \code{underbrace} typesets a brace, with an optional label over or under an object}
#' }
#' Each of these have \code{Eqn_*} aliases to fit with the style of \code{\link{Eqn}} helper functions.
#' 
#' For example, given the matrix \code{A = matrix(1:4), 2, 2)}, the call \code{Eqn(overset(A, "A"))}
#' generates:
#' \preformatted{
#' \overset{\mathbf{A}} 
#'  { \begin{pmatrix} 
#'   1 & 3 \\ 
#'   2 & 4 \\ 
#'   \end{pmatrix}
#'  }
#' }
#'
#'  When rendered in LaTeX, this produces:
#'  \deqn{
#'  \overset{\mathbf{A}} 
#'  { \begin{pmatrix} 
#'   1 & 3 \\ 
#'   2 & 4 \\ 
#'   \end{pmatrix}
#'  }
#'  }
#'  
#'  You can also use these for straight LaTeX expressions, such this equation showing and labeling
#'  the Hat matrix in regression. See the examples for the call to \code{underbrace} for this.
#'  \deqn{\mathbf{\hat{y}} =
#'        \underbrace{\mathbf{X}(\mathbf{X}^{\top}\mathbf{X})^{-1}
#'        \mathbf{X}^{\top}}_{\mathbf{\mathbf{H}}}\mathbf{y}} 
#' 
#'
#' @param x     a numeric or character matrix, or a character string LaTeX expression or 
#'        a \code{"latexMatrix"} object
#' @param label   a character string used as the label above or below the object \code{x}.
#'        If missing, and a \code{"matrix"} object was passed, it's name is used as the label.
#'        In LaTeX, these are rendered in a size appropriate for superscripts and subscripts,
#'        but you can use a size modifier to change this, for example \code{'\\Large{"A"}'}.  
#' @param label.style The name of a math font used to to typeset the label. One of
#'        \code{c("mathbf", "mathrm", "mathit", "mathsf", "mathcal", "mathtt", " ")}.
#'        The default, \code{"mathbf"} wraps the label inside \code{"\\mathbf{ }"}
#'        commonly used for the name of a matrix.
#'
#' @return Returns a character vector containing the LaTeX expressions for the given operation. You can pass
#'        this to \code{\link{cat}} to display the result on the console, or include it inside a call
#'        to \code{Eqn} to typeset it.
#' @export
#'
#' @examples
#' library(matlib)
#' A <- matrix(1:4, 2, 2)
#' B <- matrix(4:1, 2, 2)
#' AB <- A + B
#' Eqn(overset(A, "A"))
#'   #  missing label: uses the name of the object
#' Eqn(overset(A))
#'
#' # test just a character LaTeX expression
#' Eqn('a', overset('=', '?'), 'b')
#' 
#' # a labelled latexMatrix equation
#' Eqn(overset(A, "A"), "+",
#'     overset(B, "B"), "=",
#'     underset(AB, "A+B"))
#'     
#'  # using a LaTeX expression as the label  
#'  Lambda <- latexMatrix("\\lambda", nrow=2, ncol=2,
#'                        diag=TRUE)
#'  Eqn(overset(Lambda, "\\Lambda"))
#'
#'  # generate LaTeX expression for the Hat matrix, label as "H"
#' H <- "\\mathbf{X} (\\mathbf{X}^{\\top}\\mathbf{X})^{-1} \\mathbf{X}^{\\top}"
#' Eqn("\\mathbf{\\hat{y}} =", underbrace(H, "\\mathbf{H}"), "\\mathbf{y}")
#' 
#' # Combine this with overbrace
#' Eqn(overbrace(underbrace(H, "\\mathbf{H}"), "\\LARGE\\mathbf{\\hat{y}}"))
#'
#' @rdname Eqn_helpers
#' @export

overset <- function(x,
                    label,
                    label.style = c("mathbf", "mathrm", "mathit", "mathsf", "mathcal", "mathtt", " ")
                    )
 {
  if (missing(label) && is.matrix(x)) label <- deparse(substitute(x))
  if (is.matrix(x)) x <- latexMatrix(x)  |> getLatex(x)
  label.style <- match.arg(label.style)
  if (label.style != " ") label <- paste0("\\", label.style, "{", label, "}")
  over <- paste0("\\overset{", label, "}")
  c(over, "\n{",
    ifelse(inherits(x, 'latexMatrix'), getLatex(x), x),
    "}\n" )
  }

# Is it useful to allow for label.size, or could this just be handled by label = "\Large(A)" ?
# overset <- function(x,
#                     label,
#                     label.style = c("mathbf", "mathrm", "mathit", "mathsf", "mathcal", "mathtt", " "),
#                     label.size = c("normalsize", "large", "Large", "LARGE")
#                     )
#   {
#   if (missing(label) && is.matrix(x)) label <- deparse(substitute(x))
#   if (is.matrix(x)) x <- latexMatrix(x) |> getLatex()
#   label.style <- match.arg(label.style)
#   label.size <- match.arg(label.size)
#   if (label.size != "normalsize") label <- paste0("\\", label.size, "{", label, "}")
#   if (label.style != " ") label <- paste0("\\", label.style, "{", label, "}")
#   over <- paste0("\\overset{", label, "}")
#   return(c(over, "\n{", x, "}\n" ))
#   }

#' @rdname Eqn_helpers
#' @export
underset <- function(x,
                     label,
                     label.style = c("mathbf", "mathrm", "mathit", "mathsf", "mathcal", "mathtt", " ")
                    )
{
  if (missing(label) && is.matrix(x)) label <- deparse(substitute(x))
  if (is.matrix(x)) x <- latexMatrix(x) |> getLatex(x)
  label.style <- match.arg(label.style)
  if (label.style != " ") label <- paste0("\\", label.style, "{", label, "}")
  under <- paste0("\\underset{", label, "}")
  c(under, "\n{",
           ifelse(inherits(x, 'latexMatrix'), getLatex(x), x),
           "}\n" )
}


#' @rdname Eqn_helpers
#' @export
overbrace <- function(x,
                      label=NULL,
                      label.style = c("mathbf", "mathrm", "mathit", "mathsf", "mathcal", "mathtt", " ")
                      )
  {
  if(is.matrix(x)) x <- latexMatrix(x)
  res <- paste0("\\overbrace{",
                ifelse(inherits(x, 'latexMatrix'), getLatex(x), x),
                "}")
  if (!is.null(label)) {
    label.style <- match.arg(label.style)
    if (label.style != " ") label <- paste0("\\", label.style, "{", label, "}")
    res <- paste0(res, "^{", label, "}")
    }
  res
  }

#' @rdname Eqn_helpers
#' @export
underbrace <- function(x,
                       label=NULL,
                       label.style = c("mathbf", "mathrm", "mathit", "mathsf", "mathcal", "mathtt", " ")
                       )
  {
  if(is.matrix(x)) x <- latexMatrix(x)
  res <- paste0("\\underbrace{",
                ifelse(inherits(x, 'latexMatrix'), getLatex(x), x),
                "}")
  if (!is.null(label)) {
    label.style <- match.arg(label.style)
    if (label.style != " ") label <- paste0("\\", label.style, "{", label, "}")
    res <- paste0(res, "_{", label, "}")
  }
  res
  }

# Make these aliases of Eqn_ functions

#' @rdname Eqn_helpers
#' @export
Eqn_overset <- overset

#' @rdname Eqn_helpers
#' @export
Eqn_underset <- underset

#' @rdname Eqn_helpers
#' @export
Eqn_overbrace <- overbrace

#' @rdname Eqn_helpers
#' @export
Eqn_underbrace <- underbrace

if (FALSE) {
  library(matlib)
  A <- matrix(1:4, 2, 2)
  B <- matrix(4:1, 2, 2)
  AB <- A + B
  Eqn(overset(A, "A"))
  # test missing label
  Eqn(overset(A))
  # test just a character LaTeX
  Eqn('a', overset('=', '?'), 'b')

  # test label styles
  Eqn(overset(A, "A", label.style = "mathtt"))
  Eqn(overset(A, "AAAAA", label.style = "mathcal"))
  Eqn(overset(A, "A", label.style = " "))
  Eqn(overset(A, "\\Large{A}", label.style = " "))
  
  # character matrix
  abcd <- matrix(letters[1:4], 2, 2)
  Eqn(overset(abcd))

  # test equations
  Eqn(overset(A, "A"), "+",
      overset(B, "B"), "=",
      underset(AB, "A+B"))

  # test latexMatrix objects
  Lambda <- latexMatrix("\\lambda", nrow=2, ncol=2,
                   diag=TRUE)
  # OK -- 
  Eqn(Lambda)
  Eqn(overset(Lambda, "\\Lambda"))
  Eqn(underset(Lambda, "\\Lambda"))

  # over/underbrace
  Eqn(overbrace(A, "A"))
  Eqn(underbrace(A, "A"))
  
  ## -------Underbrace:
  
  # generate Hat matrix, label as H
  H <- "\\mathbf{X}(\\mathbf{X}^{\\top}\\mathbf{X})^{-1}\\mathbf{X}^{\\top}"

  # Do this with Eqn()
  Eqn("\\mathbf{\\hat{y}} =", underbrace(H, "\\mathbf{H}"), "\\mathbf{y}")
  
  # We can even combine this with overbrace
  Eqn(overbrace(underbrace(H, "\\mathbf{H}"), "\\LARGE\\mathbf{\\hat{y}}"))
  
  

  # data(dogfood, package = "heplots") -- not yet on CRAN
  load(here::here("dev", "dogfood.RData"))
  dogfood.mod <- lm(cbind(start, amount) ~ formula, data=dogfood)
  dogfood.aov <- car::Anova(dogfood.mod)
  SSP_H <- dogfood.aov$SSP[["formula"]]   |> round(digits = 2)
  SSP_E <- dogfood.aov$SSPE               |> round(digits = 2)
  SSP_T <- SSP_H + SSP_E

    # make row/colnames disappear
  options(print.latexMatrix = list(display.labels=FALSE))
  # do overbrace directly: WORKS
  Eqn("\\overset{\\mathbf{SSP}_T}{", SSP_T, "}")

  # these now work:
  Eqn(overset(SSP_H, "\\mathbf{SSP_H}"))
  Eqn(overset(SSP_E, "\\mathbf{SSP_E}"))

  # use latexMatrix()
  H <- latexMatrix(SSP_H)
  Eqn(overset(H, "\\mathbf{SSP_H}"))

  # show SSP_H + SSP_E = (SSP_H+SSP_E)
  Eqn(overset(SSP_T, "\\mathbf{SSP}_T"), "=",
      overset(SSP_H, "\\mathbf{SSP}_H"), "+",
      overset(SSP_E, "\\mathbf{SSP}_E")
      )

  # This works too:
  Eqn("\\overset{\\mathbf{SSP}_T}{", SSP_T, "}",
      "+",
      "\\overset{\\mathbf{SSP}_H}{", SSP_H, "}",
      "=",
      "\\overset{\\mathbf{SSP}_E}{", SSP_E, "}"
    )
  # And this
  Eqn("\\overset{\\mathbf{SSP}_T}{", latexMatrix(SSP_T), "}",
      "+",
      "\\overset{\\mathbf{SSP}_H}{", latexMatrix(SSP_H), "}",
      "=",
      "\\overset{\\mathbf{SSP}_E}{", latexMatrix(SSP_E), "}"
  )

}

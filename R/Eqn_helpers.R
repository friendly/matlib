# Functions to add decorators over or under matrices
#
#' @name Eqn_helpers
#' @aliases overset underset overbrace underbrace 
#' @aliases Eqn_overset Eqn_underset Eqn_overbrace Eqn_underbrace
#' @aliases Eqn_newline Eqn_hspace Eqn_vspace Eqn_size Eqn_text
#' @aliases newline hspace vspace size
#' 
#' @title Helpers to Compose Equations with Eqn 
#' 
#' @description
#' 
#' These functions extend \code{\link{Eqn}} to facilitate composing LaTeX equations with desirable features
#' and pleasant typography.
#'
#' \itemize{
#'    \item{\code{Eqn_overset} and \code{Eqn_underset} typesets a label over or under a LaTeX expression
#' or a \code{"latexMatrix"} object}
#'    \item{\code{Eqn_overbrace} and \code{Eqn_underbrace} typesets a brace, with an optional label over or under an object}
#'    \item{\code{Eqn_newline},  \code{Eqn_hspace} and \code{Eqn_vspace} facilitate spacing of parts of an equation.}
#'    \item{\code{Eqn_size} changes size of LaTeX text; \code{Eqn_text} includes a literal string in equations.}
#' }
#' Each of these (except \code{Eqn_text}) have aliases without the \code{Eqn_} prefix for brevity.
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


#' Emit a newline in an equation
#'
#' \code{Eqn_newline()} emits a newline (\code{\\}) in an equation, with
#' an optional increase to the padding following the newline.
#'
#' @rdname Eqn_helpers
#' @export
#'
#' @examples
#'
#' Eqn_newline()
#' Eqn_newline('10ex')
#'
Eqn_newline <- function(space = 0){
  ret <- if(space > 0){
    metric <- substr(space, nchar(space)-1, nchar(space))
    checkLaTeXMetric(metric)
    sprintf(' \\\\[%s] \n', space)
  } else ' \\\\ \n'
  ret
}

# add shorthand alias
#' @rdname Eqn_helpers
#' @export
newline <- Eqn_newline

#' Eqn_text Include literal string in equations
#'
#' \code{Eqn_text()} inserts a literal string to be rendered in a text font in an equation.
#'
#' @param text argument to be used within \code{\\text{}}
#' @rdname Eqn_helpers
#' @export
Eqn_text <- function(text) sprintf("\\text{%s}", text)

#' Add horizontal spaces to equations
#'
#' \code{Eqn_hspace()} is used to create (symmetric) equation spaces, most typically around
#' \code{=} signs
#' Input to \code{lhs}, \code{rhs} can be a
#' numeric to increase the size of the space or a
#' character vector to be passed to the LaTeX macro \code{\\hspace{}}.
#'
#' @param lhs spacing size. Can be a number between -1 and 6. -1 provides negative
#'   spaces and 0 gives no spacing. Input can also be a character vector, which will be
#'   passed to \code{\\hspace{}} (e.g., \code{'1cm'}; see \code{space} argument
#'   for supported metrics).
#'   Default is 5, resulting in a \code{\\quad} space.
#'
#' @param mid character vector to place in the middle of the space specification. Most
#'   commonly this will be operators like \code{'='}
#'
#' @param rhs see lhs for details. If left as \code{NULL} and \code{mid} is specified
#'   the this will be set to \code{rhs} to create symmetric spaces around \code{mid}
#'
#' @param times number of times to repeat the spacings
#'
#' @rdname Eqn_helpers
#' @export
#'
#' @examples
#'
#' Eqn_hspace()
#' Eqn_hspace(3) # smaller
#' Eqn_hspace(3, times=2)
#' Eqn_hspace('1cm')
#'
#' # symmetric spacing around mid
#' Eqn_hspace(mid='=')
#' Eqn_hspace(mid='=', times=2)
#'
Eqn_hspace <- function(lhs = 5, mid='', rhs=NULL, times=1){
  
  spacer <- function(inp){
    space <- if(is.numeric(inp)){
      stopifnot(inp <= 6 && inp >= -1)
      switch(as.character(inp),
             "-1"='\\!',
             "0"='',
             "1"='\\,',
             "2"='\\:',
             "3"='\\;',
             "4"='\\ ',
             "5"='\\quad',
             "6"='\\qquad')
    } else {
      metric <- substr(inp, nchar(inp)-1, nchar(inp))
      checkLaTeXMetric(metric)
      sprintf('\\hspace{%s}', inp)
    }
    space
  }
  
  stopifnot(is.character(lhs) || is.numeric(lhs))
  if(!is.null(rhs))
    stopifnot(is.character(rhs) || is.numeric(rhs))
  if(mid != "" && is.null(rhs))
    rhs <- lhs
  if(is.null(rhs)) rhs <- 0
  space.lhs <- paste0(rep(spacer(lhs), times=times), collapse='')
  space.rhs <- paste0(rep(spacer(rhs), times=times), collapse='')
  paste0(c(space.lhs, mid, space.rhs), collapse='')
}

# add shorthand alias
#' @rdname Eqn_helpers
#' @export
hspace <- Eqn_hspace

#' Insert Vertical Space in an Equation
#'
#' \code{Eqn_vspace()} inserts vertical space between lines in an equation.
#' Typically used for aligned, multiline equations.
#'
#' @param space includes extra vertical space. Metric of the vertical space
#'   must be 'ex', 'pt', 'mm', 'cm', 'em', 'bp', 'dd', 'pc', or 'in'
#' @rdname Eqn_helpers
#' @export
#' @examples
#'
#' Eqn_vspace('1.5ex')
#' Eqn_vspace('1cm')
#'
#'
Eqn_vspace <- function(space){
  metric <- substr(space, nchar(space)-1, nchar(space))
  checkLaTeXMetric(metric)
  sprintf(" \\vspace{%s} \n", space)
}

# add shorthand alias
#' @rdname Eqn_helpers
#' @export
vspace <- Eqn_vspace


checkLaTeXMetric <- function(metric){
  valid <- c('em', 'pt', 'mm', 'cm', 'ex',
             'bp', 'dd', 'pc', 'in')
  ret <- metric %in% valid
  if(!ret) stop('LaTeX metric is invalid', call. = FALSE)
  invisible(ret)
}

#' Change size of LaTeX text
#'
#' \code{Eqn_size()} is used to increase or decrease the size of LaTeX text and equations. Can be applied
#' to a specific string or applied to all subsequent text until overwritten.
#'
#' @param string a string that should have its text size modified. If missing
#'   the size modifier is returned, which applies the size modifier
#'   to the remainder of the text until reset with \code{Eqn_size()}
#' @param size numeric size of LaTeX text modifier,
#'   ranging from -3 (\code{\\tiny}) to 5 (\code{\\HUGE}), with 0 defining the
#'   normal test size (\code{\\normalsize}; default)
#'
#' @rdname Eqn_helpers
#' @export
#'
#' @examples
#'
#' # set size globally
#' Eqn_size(size=3)
#' Eqn_size() # reset
#'
#' # locally for defined string
#' string <- 'e = mc^2'
#' Eqn_size(string, size=1)
#'
#'
Eqn_size <- function(string, size = 0){
  stopifnot(size <= 5 && size >= -3)
  size <- switch(as.character(size),
                 "-3"='\\tiny ',
                 "-2"='\\scriptsize ',
                 "-1"='\\footnotesize ',
                 "0"='\\normalsize ',
                 "1"='\\large ',
                 "2"='\\Large ',
                 "3"='\\LARGE ',
                 "4"='\\huge ',
                 "5"='\\Huge ')
  ret <- if(missing(string)) gsub(' ', '', size)
  else paste0('{', size, string, '}')
  ret
}

# add shorthand alias
#' @rdname Eqn_helpers
#' @export
size <- Eqn_size

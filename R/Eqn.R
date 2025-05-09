#' Create a LaTeX Equation Wrapper
#'
#' @description
#'
#' The \code{Eqn} function is designed to produce LaTeX expressions of mathematical
#' equations for writing.
#' The output can be copied/pasted into documents or
#' used directly in chunks in \code{.Rmd}, \code{.Rnw}, or \code{.qmd}
#' documents to compile to equations.
#'
#' \code{Eqn} wraps the equations generated by its arguments
#' in either a \code{\\begin{equation} ...\\end{equation}} or
#' \code{\\begin{align} ...\\end{align}} LaTeX environment. See also
#' \code{\link{ref}} for consistent inline referencing of numbered equations in documents.
#'
#' In a code chunk, use the chunk options \code{results='asis', echo=FALSE} to show only
#' the result of compiling the LaTeX expressions. For example,
#' \preformatted{
#' ```{r results = "asis", echo = FALSE}
#' Eqn("\\mathbf{X} = \\mathbf{U} \\mathbf{\\Lambda} \\mathbf{V}", label='eq:svd')
#' ```
#' }
#'
#' Note that you can avoid the "leaning toothpick syndrome" of all those doubled backslashes by using R's new (as of 4.0.0)
#' "raw strings", composed as \code{r"(...)"} or \code{r"{...}"}
#'
#' \preformatted{
#' ```{r results = "asis", echo = FALSE}
#' Eqn(r"{\mathbf{X} = \mathbf{U} \mathbf{\Lambda} \mathbf{V}}", label = 'eq:svn')
#' ```
#' }
#'
#'
#' A collection of helper functions, such as \code{\link{Eqn_newline}}, \code{\link{Eqn_hspace}}
#' facilitate formatting of equations and functions like \code{\link{Eqn_overset}} and \code{\link{Eqn_overbrace}}
#' provide for decorators over or under a LaTeX expression or matrix. See \code{\link{Eqn_helpers}}
#' for their descriptions and examples.
#'
#' @param ... comma separated LaTeX expressions that are either a) a \code{character} vector,
#'   which will be automatically wrapped the
#'   expression inside a call to \code{\link{cat}}, b) a \code{matrix} object
#'   containing character or numeric information, which will be passed \code{\link{latexMatrix}},
#'   along with the information in \code{mat_args}, or
#'   c) an object that was explicitly created via \code{\link{latexMatrix}}, which
#'   provides greater specificity.
#'
#'   Note that user defined functions that use \code{\link{cat}} within
#'   their body should return an empty character vector to avoid printing the
#'   returned object
#' @param label character vector specifying the label to use (e.g., \code{eq:myeqn}), which
#'   for LaTeX can be reference via \code{\\ref{eq:myeqn}} or via the inline function
#'   \code{\link{ref}}. Including a label will also include an equation number automatically.
#'
#'   For compiled documents if an HTML output is detected (see \code{html_output})
#'   then the equations will be labelled  via \code{(\#eq:myeqn)} and references via \code{\@ref(eq:myeqn)},
#'   or again via \code{\link{ref}} for convenience. For Quarto documents the label must be of the
#'   form \code{eq-LABEL}
#' @param html_output logical; use labels for HTML outputs instead of the LaTeX? Automatically
#'   changed for compiled documents that support \code{knitr}. Generally not
#'   required or recommended for the user to modify, except to view the generated syntax
#' @param quarto logical; use Quarto referencing syntax? When \code{TRUE}
#'   the \code{html_output} will be irrelevant. Generally not recommended for the
#'   user to modify, except to view the generated syntax
#' @param align logical; use the \code{align} environment with explicit \code{&} representing alignment
#'   points. Default: \code{FALSE}
#' @param mat_args list of arguments to be passed to \code{\link{latexMatrix}} to change the
#'   properties of the \code{matrix} input object(s). Note that these inputs are used globally, and apply to
#'   each \code{matrix} object supplied. If further specificity is required create
#'   \code{\link{latexMatrix}} objects directly.
#' @param preview logical; render an HTML version of the equation and display? This is intended for
#'  testing purposes and is only applicable to interactive R sessions, though
#'  for code testing purposes can be set globally
#'  via \code{\link{options}} (e.g., \code{options('previewEqn' = FALSE)}).
#'  Disabled whenever \code{quarto} or \code{html_output} are \code{TRUE}
#' @param preview.pdf logical; build a PDF of the preview equation? Generally
#'  not require unless additional LaTeX packages are required that are not supported
#'  by MathJax
#' @param preview.packages character vector for adding additional LaTeX package information to the
#'  equation preview. Only used when \code{preview.pdf = TRUE}
#' @returns NULL
#' @importFrom knitr is_html_output
#' @importFrom rstudioapi viewer
#' @importFrom rmarkdown render
#' @references Josiah Parry, Raw strings in R, \url{https://josiahparry.com/posts/2023-01-19-raw-strings-in-r.html}
#' @author Phil Chalmers
#' @seealso \code{\link{Eqn_helpers}}, \code{\link{latexMatrix}}, \code{\link{matrix2latex}}, \code{\link{ref}}
#' @export
#' @examples
#'
#' # character input
#' Eqn('e=mc^2')
#'
#' # show only the LaTeX code
#' Eqn('e=mc^2', preview=FALSE)
#'
#' # Equation numbers & labels
#' Eqn('e=mc^2', label = 'eq:einstein')
#' Eqn("X=U \\lambda V", label='eq:svd')
#'
#' # html_output and quarto outputs only show code
#' #   (both auto detected in compiled documents)
#' Eqn('e=mc^2', label = 'eq:einstein', html_output = TRUE)
#'
#' # Quarto output
#' Eqn('e=mc^2', label = 'eq-einstein', quarto = TRUE)
#'
#' \dontrun{
#' # The following requires LaTeX compilers to be pre-installed
#'
#' # View PDF instead of HTML
#' Eqn('e=mc^2', preview.pdf=TRUE)
#'
#' # Add extra LaTeX dependencies for PDF build
#' Eqn('\\bm{e}=mc^2', preview.pdf=TRUE,
#'     preview.packages=c('amsmath', 'bm'))
#'
#' }
#'
#' # Multiple expressions
#' Eqn("e=mc^2",
#'     Eqn_newline(),
#'     "X=U \\lambda V", label='eq:svd')
#'
#' # expressions that use cat() within their calls
#' Eqn('SVD = ',
#'     latexMatrix("u", "n", "k"),
#'     latexMatrix("\\lambda", "k", "k", diag=TRUE),
#'     latexMatrix("v", "k", "p", transpose = TRUE),
#'     label='eq:svd')
#'
#' # align equations using & operator
#' Eqn("X &= U \\lambda V", Eqn_newline(),
#'     "& = ", latexMatrix("u", "n", "k"),
#'     latexMatrix("\\lambda", "k", "k", diag=TRUE),
#'     latexMatrix("v", "k", "p", transpose = TRUE),
#'     align=TRUE)
#'
#' #  numeric/character matrix example
#' A <- matrix(c(2, 1, -1,
#'               -3, -1, 2,
#'               -2,  1, 2), 3, 3, byrow=TRUE)
#' b <- matrix(c(8, -11, -3))
#'
#' # numeric matrix wrapped internally
#' cbind(A,b) |> Eqn()
#' cbind(A,b) |> latexMatrix() |> Eqn()
#'
#' # change numeric matrix brackets globally
#' cbind(A,b) |> Eqn(mat_args=list(matrix='bmatrix'))
#'
#' # greater flexibility when using latexMatrix()
#' cbind(A, b) |> latexMatrix() |> partition(columns=3) |> Eqn()
#'
#' # with showEqn()
#' showEqn(A, b, latex=TRUE) |> Eqn()
#'
Eqn <- function(...,
                label = NULL,
                align = FALSE,
                preview = getOption('previewEqn'),
                html_output = knitr::is_html_output(),
                quarto = getOption('quartoEqn'),
                mat_args = list(),
                preview.pdf = FALSE,
                preview.packages=NULL) {

  # for connection safety
  sink.reset <- function(){
    if(sink.number() > 1L){
      for(i in seq_len(sink.number())){
         sink(NULL)
      }
    }
  }
  on.exit(sink.reset())
  if(is.null(preview)) preview <- TRUE
  quarto <- setQuartoEqn(quarto)
  if(is.null(quarto)) quarto <- FALSE
  preview <- preview && interactive()
  if(html_output || quarto) preview <- FALSE
  if(preview){
      tmpfile <- tempfile()
      # everything except the kitchen ...
      sink(tmpfile)
      preview.header <- if(!is.null(preview.packages) && preview.pdf){
          preview.header <- sprintf('\noutput:\n    pdf_document:\n      extra_dependencies: [%s]',
                                    paste(preview.packages, collapse=','))
      } else  ""
      cat(sprintf(
"
---
title: '&nbsp;'%s
---
", preview.header))
  }
  stopifnot(is.logical(quarto))
  number <- !is.null(label)
  wrap <- if(align) "align" else "equation"
  if(!number) wrap <- paste0(wrap, '*')
  if(quarto) cat("\n$$")
  if(quarto && !align) cat("\n")
  else cat(sprintf("\n\\begin{%s}\n", wrap))
  if(!is.null(label) && !quarto){
      if(html_output){
          if(substring(label, 1, 2) != 'eq')
              stop('HTML outputs require labels to start with \"eq\"')
          cat(sprintf('(\\#%s)\n', label))
      } else cat(sprintf('\\label{%s}\n', label))
  }
  dots <- list(...)
  for(i in 1L:length(dots)){
    if(is.matrix(dots[[i]])){
        print( do.call(latexMatrix, c(list(symbol=dots[[i]]), mat_args)) )
    } else if(is.character(dots[[i]])){
        cat(dots[[i]])
    } else {
        print(dots[[i]])
    }
  }
  if(quarto && !align) cat("\n")
  else cat(sprintf("\\end{%s}\n", wrap))
  if(quarto){
      if(quarto) cat("$$ ")
      if(!is.null(label)){
          if(substring(label, 1, 3) != 'eq-')
              stop('Quarto equation labels must start with \"eq-\"')
          cat(sprintf('{#%s}', label))
      }
      cat("\n")
  }
  if(preview){
      sink()
      if(preview.pdf){
          rmarkdown::render(tmpfile, 'pdf_document', clean = TRUE, quiet = TRUE)
          rstudioapi::viewer(paste0(tmpfile, '.pdf'))
      } else {
          rmarkdown::render(tmpfile, 'html_document', clean = TRUE, quiet = TRUE)
          rstudioapi::viewer(paste0(tmpfile, '.html'))
      }
      lines <- readLines(tmpfile)
      dashloc <- which(lines == '---')
      inp <- paste0(lines[-c(1, dashloc[1]:dashloc[2])], collapse='\n')
      cat(inp)
  }
  invisible(NULL)
}

setQuartoEqn <- function(quarto){
    if(interactive()){
        if(is.null(quarto)) quarto <- FALSE
        return(quarto)
    }
    if(isTRUE(getOption('knitr.in.progress')) && is.null(getOption('quartoEqn'))){
        if(!is.null(knitr::opts_knit$get('quarto.version')))
            options('quartoEqn' = TRUE)
        else options('quartoEqn' = FALSE)
    }
    invisible(getOption('quartoEqn'))
}



#' Provide inline reference of equations
#'
#' \code{ref{}} provides inline references to equations in R
#' markdown and Quarto documents.
#' Depending on the output type this function will provide the correct
#' inline wrapper for MathJax or LaTeX equations. This provides more
#' consistent referencing when switching between HTML and PDF outputs as
#' well as documentation types (e.g., \code{.Rmd} vs \code{.qmd}).
#'
#' @param parentheses logical; include parentheses around the referenced equation?
#'
#' @export
#' @rdname Eqn
#'
#' @examples
#'
#' # used inside of Eqn() or manually defined labels in the document
#' Eqn('e = mc^2', label='eq:einstein')
#'
#' # use within inline block via `r ref()`
#' ref('eq:einstein')
#' ref('eq:einstein', parentheses=FALSE)
#' ref('eq:einstein', html_output=TRUE)
#'
#' # With Quarto
#' Eqn('e = mc^2', label='eq-einstein', quarto=TRUE)
#' ref('eq:einstein', quarto=TRUE)
#' ref('eq:einstein', quarto=TRUE, parentheses=FALSE)
#'
ref <- function(label,
                parentheses = TRUE,
                html_output = knitr::is_html_output(),
                quarto = getOption('quartoEqn')) {
    quarto <- setQuartoEqn(quarto)
    if(is.null(quarto)) quarto <- FALSE
    ret <- if(quarto){
        if(parentheses)
            sprintf('([-@%s])', label)
        else sprintf('[-@%s]', label)
    } else {
        if(html_output){
            sprintf('\\@ref(%s)', label)
        } else {
            if(parentheses) sprintf('(\\ref{%s})', label)
            else sprintf('\\ref{%s}', label)
        }
    }
    if(quarto) return(I(ret))
    ret
}

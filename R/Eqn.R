#' Create a LaTeX Equation Wrapper
#'
#' @description
#'
#' The \code{Eqn} function is designed to produce LaTeX expressions of mathematical
#' equations for writing.
#' The output can be copied/pasted into documents or
#' used directly in chunks in \code{.Rmd}, \code{.Rnw}, or \code{.qmd}
#' documents to compile to equations.
#' It wraps the equations generated by its arguments
#' in either a \code{\\begin{equation} ...\\end{equation}} or
#' \code{\\begin{align} ...\\end{align}} LaTeX environment. See also
#' \code{\link{ref}} for consistent inline referencing of numbered equations.
#'
#' In a code chunk, use the chunk options \code{results='asis', echo=FALSE} to show only
#' the result of compiling the LaTeX expressions.
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
#' @author Phil Chalmers
#' @seealso \code{\link{latexMatrix}}, \code{\link{matrix2latex}}, \code{\link{ref}}
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
      on.exit(file.remove(tmpfile), add = TRUE)
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
        finp <- knitr::current_input()
        stripped <- gsub('.rmarkdown', '', finp)
        files <- dir()
        if(sum(grepl(paste0(stripped, '.qmd'), files)) == 1)
            options('quartoEqn' = TRUE)
        else options('quartoEqn' = FALSE)
        if(getOption('quartoEqn')){
            matched_files <- files[grepl(paste0(stripped, '.'), files, fixed=TRUE)]
            exts <- gsub(paste0(stripped, '.'), "", matched_files, fixed=TRUE)
            if(any(tolower(exts) %in% c('rmd', 'rnw')))
                stop(c('Detected files with identical names but different extensions. ',
                       '\n\n  Please use unique file names when a mix of .qmd and R markdown files',
                       ' (e.g., .Rmd, .Rnw) are in the same directory.'))
        }
    }
    invisible(getOption('quartoEqn'))
}

#' Emit a newline in an equation
#'
#' \code{Eqn_newline()} emits a newline (\code{\\}) in an equation, with
#' an optional increase to the padding following the newline.
#'
#' @rdname Eqn
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


#' Eqn_text Include literal string in equations
#'
#' \code{Eqn_text()} inserts a literal string to be rendered in a text font in an equation.
#'
#' @param text argument to be used within \code{\\text{}}
#' @rdname Eqn
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
#' @rdname Eqn
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


#' Insert Vertical Space in an Equation
#'
#' \code{Eqn_vspace()} inserts vertical space between lines in an equation.
#' Typically used for aligned, multiline equations.
#'
#' @param space includes extra vertical space. Metric of the vertical space
#'   must be 'ex', 'pt', 'mm', 'cm', 'em', 'bp', 'dd', 'pc', or 'in'
#' @rdname Eqn
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
#' @rdname Eqn
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

# Functions to add decorators over or under matrices
#
# Use case: I want to show the equation SSP_T = SSP_H + SSP_E with the matrix names at the top
#

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

underbrace <- function(x,
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
    res <- paste0(res, "_{", label, "}")
  }
  res
  }

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

  # test equations
  Eqn(overset(A, "A"), "+",
      overset(B, "B"), "=",
      underset(AB, "A+B"))

  # test latexMatrix objects
  Lambda <- latexMatrix("\\lambda", nrow=2, ncol=2,
                   diag=TRUE)
  # OK -- but note the docs use "\lambda" here
  Eqn(Lambda)
  # fails miserably
  Eqn(overset(Lambda, "\\Lambda"))

  # over/underbrace

  Eqn(overbrace(A, "A"))

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





 ## -------Underbrace:
  # I want to generate the equation \hat{y} = X (X'X)^{-1} X' y with a brace underneath showing the H matrix
  # This manual LaTeX works:
  eqn <- "
  \mathbf{\hat{y}}
   = \underbrace{\mathbf{X}(\mathbf{X}^{\top}\mathbf{X})^{-1}\mathbf{X}^{\top}}_\mathbf{H}\mathbf{y}
  "

  # generate this with underbrace()
  H <- "\\mathbf{X}(\\mathbf{X}^{\\top}\\mathbf{X})^{-1}\\mathbf{X}^{\\top}"
  underbrace(H, "\\mathbf{H}")

  # Do this with Eqn() - WORKS, at least when all the elements are just symbols, not matrices
  Eqn("\\mathbf{\\hat{y}} =", underbrace(H, "\\mathbf{H}"), "\\mathbf{y}")

  # We can even combine this with overbrace
  Eqn(overbrace(underbrace(H, "\\mathbf{H}"), "\\mathbf{\\hat{y}}"))

}

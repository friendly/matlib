# Functions to add decorators over or under matrices
#
# Use case: I want to show the equation SSP_T = SSP_H + SSP_E with the matrix names at the top
#

overset <- function(x, over){
  paste0("\\overset{", over, "} {", x, "}" )
}

underset <- function(x, under){
  paste0("\\underset{", under, "} {", x, "}" )
}

overbrace <- function(x, label=NULL){
  res <- paste0("\\overbrace{", x, "}")
  if (!is.null(label)) res <- paste0(res, "^{", label, "}")
  res
}

underbrace <- function(x, label=NULL){
  res <- paste0("\\underbrace{", x, "}")
  if (!is.null(label)) res <- paste0(res, "_{", label, "}")
  res
}

if (FALSE) {

  data(dogfood, package = "heplots")
  dogfood.mod <- lm(cbind(start, amount) ~ formula, data=dogfood) 
  dogfood.aov <- car::Anova(dogfood.mod) 
  SSP_H <- dogfood.aov$SSP[["formula"]]   |> round(digits = 2)
  SSP_E <- dogfood.aov$SSPE               |> round(digits = 2)
  SSP_T <- SSP_H + SSP_E

    # make row/colnames disappear
  options(print.latexMatrix = list(display.labels=FALSE))  
  # do overbrace directly: WORKS
  Eqn("\\overset{\\mathbf{SSP}_T}{", SSP_T, "}")
  
  # but these don't work:
  Eqn(overset(SSP_H, "\\mathbf{SSP_H}"))
  Eqn(overset(SSP_E, "\\mathbf{SSP_E}"))
  
  # use latexMatrix()
  H <- latexMatrix(SSP_H)
  Eqn(overset(H, "\\mathbf{SSP_H}"))

  # show SSP_H + SSP_E = (SSP_H+SSP_E)
  # What I want to get here:
  eqn <- "
\begin{equation*}
\overset{\mathbf{SSP}_T}
  {\begin{pmatrix} 
   35.4 & -59.2 \\ 
  -59.2 & 975.9 \\ 
  \end{pmatrix}}
=
\overset{\mathbf{SSP}_H}
  {\begin{pmatrix} 
    9.69 & -70.94 \\ 
  -70.94 & 585.69 \\ 
  \end{pmatrix}}
+
\overset{\mathbf{SSP}_E}
  {\begin{pmatrix} 
   25.8 &  11.8 \\ 
   11.8 & 390.3 \\ 
  \end{pmatrix}}
\end{equation*} 
 "
  
  # this generates an error:
  # argument is missing, with no default
  Eqn("\\overset{\\mathbf{SSP_T}}{", SSP_T, "}", 
      "+",
      "\\overset{\\mathbf{SSP_H}}{", SSP_H, "}",
      "=",
      "\\overset{\\mathbf{SSP_E}}{", SSP_E, "}",
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

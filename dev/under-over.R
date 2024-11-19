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
  X <- latexMatrix("x", 2,2)
  SSP_E <- latexMatrix("y", 2,2)
  

  Eqn(X + SSP_E)

  # do overbrace directly  
  Eqn("\\overset{\\mathbf{X}}{", X, "}")
  
  # show X + SSP_E = (X+SSP_E)
  
  Eqn("\\overset{\\mathbf{X}}{", X, "}", 
      "+",
      "\\overset{\\mathbf{SSP_E}}{", SSP_E, "}",
      "=",
      "\\overset{\\mathbf{X + SSP_E}}{", X+SSP_E, "}",
  )
  
  # but these don't work
  Eqn(overset(X, "\\mathbf{X}"))
  Eqn(overset(SSP_E, "\\mathbf{SSP_E}"))

  
  data(dogfood, package = "heplots")
  dogfood.mod <- lm(cbind(start, amount) ~ formula, data=dogfood) 
  dogfood.aov <- car::Anova(dogfood.mod) 
  SSP_H <- dogfood.aov$SSP 
  SSP_E <- dogfood.aov$SSPE 
  SSP_T <- SSP_H$formula + SSP_E
  # make row/colnames disappear
  rownames(SSP_T) <- rownames(SSP_H) <- rownames(SSP_E) <- NULL
  colnames(SSP_T) <- colnames(SSP_H) <- colnames(SSP_E) <- NULL
  
  Eqn("\\overset{\\mathbf{SSP}_T}{", SSP_T, "}")
  
  # show SSP_H + SSP_E = (SSP_H+SSP_E)
  
  Eqn("\\overset{\\mathbf{SSP_H}}{", SSP_H, "}", 
      "+",
      "\\overset{\\mathbf{SSP_E}}{", SSP_E, "}",
      "=",
      "\\overset{\\mathbf{SSP_T}}{", SSP_T, "}",
  )
  
  
  
  
}

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

